--------------------------------------------------------------------------------
-- Slack Bot for CS141 Labs                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the            --
-- LICENSE file in the root directory of this source tree.                    --
--------------------------------------------------------------------------------

module Warwick.SGT.CmdArgs (
    BotOpts(..),
    parseCmdArgs
) where

--------------------------------------------------------------------------------

import Data.Text 
import Data.UUID

import Options.Applicative

--------------------------------------------------------------------------------

data BotOpts = MkBotOpts {
    optsToken :: Maybe Text,
    optsChannelFrom :: Text,
    optsChannelTimestamp :: Text,
    optsChannelTo :: Maybe Text,
    optsRegisterModule :: Maybe Text,
    optsRegisterSet :: Maybe UUID,
    optsRegisterGroup :: Maybe UUID,
    optsRegisterEvent :: Maybe UUID,
    optsRegisterWeek :: Maybe Int
} deriving (Eq, Show)

botOptsP :: Parser BotOpts
botOptsP = MkBotOpts 
    <$> optional (strOption (long "token" <> metavar "TOKEN"))
    <*> strOption (long "from" <> metavar "CHANNEL")
    <*> strOption (long "message" <> metavar "TIMESTAMP")
    <*> optional (strOption (long "to" <> metavar "CHANNEL"))
    <*> optional (strOption (long "module" <> metavar "MODULE"))
    <*> optional (option auto (long "set" <> metavar "SET"))
    <*> optional (option auto (long "group" <> metavar "GROUP"))
    <*> optional (option auto (long "event" <> metavar "EVENT"))
    <*> optional (option auto (long "week" <> metavar "WEEK"))

opts :: ParserInfo BotOpts
opts = info (botOptsP <**> helper) idm

parseCmdArgs :: IO BotOpts
parseCmdArgs = execParser opts

--------------------------------------------------------------------------------
