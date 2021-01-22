--------------------------------------------------------------------------------
-- Slack Bot for CS141 Labs                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the            --
-- LICENSE file in the root directory of this source tree.                    --
--------------------------------------------------------------------------------

module Warwick.SGT.Bot ( runBot ) where 

--------------------------------------------------------------------------------

import Control.Monad

import Data.CaseInsensitive (CI, mk)
import Data.Default.Class
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe
import Data.UUID

import System.Environment
import System.Exit

import Slack
import Slack.Response
import Slack.API.Chat
import Slack.API.Reactions
import Slack.Types.User
import Slack.Types.Message

import Warwick.Tabula
import Warwick.Tabula.API
import Warwick.Tabula.Member
import Warwick.Tabula.Payload.SmallGroup

import Warwick.SGT.CmdArgs

--------------------------------------------------------------------------------

-- | `mkUserMap` @users@ converts a list of `SlackUser` values @users@ into a
-- `M.Map` where the `SlackUser` values are indexed by their user id.
mkUserMap :: [SlackUser] -> M.Map T.Text SlackUser
mkUserMap users = M.fromList [(userId user, user) | user <- users]

-- | `resolveUsers` @userMap userIDs@ takes a `S.Set` of Slack user ids 
-- @userIDs@ and finds the corresponding `SlackUser` values in @userMap@.
resolveUsers :: M.Map T.Text SlackUser -> S.Set T.Text -> [SlackUser]
resolveUsers tbl = foldr (\uid r -> case M.lookup uid tbl of
    Nothing -> error "user not found"
    Just u -> u : r) []

-- | `mkMemberMap` @members@ converts @members@ into a `M.Map` indexed by the
-- members' email addresses.
mkMemberMap :: [Member] -> M.Map (CI T.Text) Member
mkMemberMap members = M.fromList 
    [(mk $ T.pack $ fromJust $ memberEmail m, m) | m <- members]

-- | `resolveMembers` @memberMap slackUsers@ retrieves the University IDs for
-- all Slack users in @slackUsers@.
resolveMembers :: M.Map (CI T.Text) Member -> [SlackUser] -> [T.Text]
resolveMembers tbl = foldr go [] where 
    go user r = 
        -- we assume all Slack users who responded have an email address
        -- attached; otherwise bad things(tm) happen
        case M.lookup (mk $ fromJust $ profileEmail $ userProfile user) tbl of
            -- the user isn't registered for the module, skip
            Nothing -> r
            -- we assume that if we have found a member, they have 
            -- a University ID
            Just member -> T.pack (fromJust (memberID member)) : r

-- | `determineGroupMembers` @tabulaCfg groupId eventId week@ determines which
-- students are already members of the event identified by @eventId@ for the
-- week identified by @week@.
determineGroupMembers 
    :: APIConfig 
    -> T.Text 
    -> T.Text
    -> Int 
    -> IO (S.Set T.Text)
determineGroupMembers cfg groupId eventId week = do
    -- retrieve attendance for the small group
    attRes <- withTabula Live cfg $ retrieveSmallGroupAttendance groupId

    -- check that the request was successful
    status <- case attRes of 
        Left err -> do 
            print err 
            exitWith (ExitFailure (-1))
        Right TabulaOK{..} -> pure tabulaData

    -- a predicate for checking that an event ref refers to the event we
    -- are interested in
    let isEvent EventRef{..} = erId == eventId
                            && erWeek == week

    -- checks if a particular student is a member of the event we are 
    -- interested in
    let isRegistered StudentAttendance{..} = 
            any (isEvent . seaEventRef) saEvents

    -- return a list of all members who are already part of the event
    -- in the given week
    pure $ S.fromList 
         $ map saUniversityID 
         $ filter isRegistered (sgarAttendance status)

-- | `runBot` @opts@ runs the lab bot using the configuration given by @opts@.
runBot :: BotOpts -> IO () 
runBot MkBotOpts{..} = do
    -- get the Slack API token, either from the environment or the command
    -- line arguments if unavailable
    cfg <- MkSlackConfig . fromMaybe (fromMaybe "" optsToken) . fmap T.pack
                       <$> lookupEnv "SLACK_TOKEN"

    -- retrieve a list of all users on the Slack team   
    userRes <- withSlackClient cfg $ listUsers def

    users <- case userRes of 
        Left err -> do 
            print err 
            exitWith (ExitFailure (-1))
        Right NotOk -> do
            putStrLn "Request to the users API failed"
            exitWith (ExitFailure (-1))
        Right Ok{..} -> pure $ mkUserMap responsePayload
        
    -- query the Slack API for reactions to the message
    reactionsRes <- withSlackClient cfg $ getReactions def{
        reactionsChannel = Just optsChannelFrom,
        reactionsFull = Just True,
        reactionsTimestamp = Just optsChannelTimestamp
    }

    -- get a set of users who have reacted to the message
    reactions <- case reactionsRes of 
        Left err -> do 
            print err 
            exitWith (ExitFailure (-1))
        Right NotOk -> do
            putStrLn "Request to the reactions API failed"
            exitWith (ExitFailure (-1))
        Right Ok{..} -> case messageReactions responsePayload of
            Nothing -> do 
                putStrLn "No reactions are contained in the response"
                exitWith (ExitFailure (-1))
            -- Slack groups the reactions by the emoji used, we don't care
            -- about this and just flatten them all into a set of ids that
            -- are then resolved to the corresponding users
            Just reactions -> pure $ resolveUsers users 
                                   $ S.fromList 
                                   $ concat 
                                   $ map reactionUsers reactions

    -- construct a list of everyone in attendance, with their email addresses
    -- attached for easier identification
    let message = T.concat $ flip map reactions $
            \MkSlackUser{..} -> T.concat 
                [ "- <@", userId, "> ("
                , fromMaybe "Unknown email" (profileEmail userProfile) 
                , ")\n"
                ]

    -- if an output channel has been specified, send a Slack message to it
    -- with the list of students in attendance, otherwise print it to stdout
    case optsChannelTo of 
        Nothing -> 
            putStrLn $ T.unpack message
        Just to -> 
            void $ withSlackClient cfg $ postMessage MkChatPostMessageReq{
                postMessageChannel = to,
                postMessageText = 
                    "*The following students are in attendance*:\n" <>
                    message
            }

    let tabulaOpts = (,,,,) <$> optsRegisterModule 
                            <*> optsRegisterSet 
                            <*> optsRegisterGroup 
                            <*> optsRegisterEvent
                            <*> optsRegisterWeek

    -- if we should register attendance on Tabula, get a list of all students
    -- registered for the module, query their email addresses, look up their
    -- University IDs based on email address, and then register attendance
    case tabulaOpts of
        Nothing -> pure ()
        Just (mc,set,grp,event,wk) -> do
            -- read the tabula credentials from a file
            Just tabulaCfg <- readAPIConfig "bot.json"

            -- try to retrieve all the members for the module
            membersRes <- withTabula Live tabulaCfg $ 
                listRegisteredUniversityIdsIn (ModuleCode mc) 2020

            universityIds <- case membersRes of 
                Left err -> do 
                    putStrLn "Request to the Tabula module members API failed"
                    exitWith (ExitFailure (-1))
                Right TabulaOK{..} -> pure tabulaData

            -- try to retrieve further info about each member, so that we have
            -- their email addresses and can match them up with the Slack users
            infoRes <- withTabula Live tabulaCfg $  retrieveMembers 
                universityIds ["member.universityId", "member.email"]

            attendeeIds <- case infoRes of 
                Left err -> do 
                    putStrLn "Request to the Tabula retrieve members API failed"
                    exitWith (ExitFailure (-1))
                Right TabulaOK{..} -> pure $ resolveMembers 
                    (mkMemberMap $ HM.elems tabulaData) reactions

            -- determine members of the event, so that we can add 
            -- missing students to it for this week
            alreadyRegistered <- determineGroupMembers 
                tabulaCfg (toText grp) (toText event) wk

            -- calculate the set of students not already registered for the
            -- event in the given week
            let toRegister = S.fromList attendeeIds S.\\ alreadyRegistered

            -- print some debugging info
            print attendeeIds
            print toRegister

            -- add all students to the event in this week who are not
            -- already added
            forM_ toRegister $ \universityId -> withTabula Live tabulaCfg $
                addEventMember (ModuleCode mc) set grp event wk $
                MkAddEventMemberReq universityId Nothing Nothing

            -- finally, register attendance for every member who is present
            res <- withTabula Live tabulaCfg $ 
                registerAttendance (ModuleCode mc) set grp event wk $ 
                MkRegisterAttendanceReq $ 
                M.fromList [ (uid, "attended-remotely") | uid <- attendeeIds ]

            print res

            pure ()

    pure ()

--------------------------------------------------------------------------------
