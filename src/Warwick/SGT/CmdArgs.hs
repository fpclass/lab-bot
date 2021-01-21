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

import Options.Applicative

--------------------------------------------------------------------------------

data BotOpts = MkBotOpts {
    optsToken :: Maybe Text,
    optsChannelFrom :: Text,
    optsChannelTimestamp :: Text,
    optsChannelTo :: Maybe Text
} deriving (Eq, Show)

botOptsP :: Parser BotOpts
botOptsP = MkBotOpts 
    <$> optional (strOption (long "token" <> metavar "TOKEN"))
    <*> strOption (long "from" <> metavar "CHANNEL")
    <*> strOption (long "message" <> metavar "TIMESTAMP")
    <*> optional (strOption (long "to" <> metavar "CHANNEL"))

opts :: ParserInfo BotOpts
opts = info (botOptsP <**> helper) idm

parseCmdArgs :: IO BotOpts
parseCmdArgs = execParser opts

--------------------------------------------------------------------------------
