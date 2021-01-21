--------------------------------------------------------------------------------
-- Slack Bot for CS141 Labs                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the            --
-- LICENSE file in the root directory of this source tree.                    --
--------------------------------------------------------------------------------

module Warwick.SGT.Bot ( runBot ) where 

--------------------------------------------------------------------------------

import Control.Monad

import Data.Default.Class
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe

import System.Environment
import System.Exit

import Slack
import Slack.Response
import Slack.API.Chat
import Slack.API.Reactions
import Slack.Types.User
import Slack.Types.Message

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

    pure ()

--------------------------------------------------------------------------------
