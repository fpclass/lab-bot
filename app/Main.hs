--------------------------------------------------------------------------------
-- Slack Bot for CS141 Labs                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT licence found in the            --
-- LICENSE file in the root directory of this source tree.                    --
--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Warwick.SGT.CmdArgs
import Warwick.SGT.Bot

--------------------------------------------------------------------------------

-- | The main entry point for this application.
main :: IO ()
main = parseCmdArgs >>= runBot

--------------------------------------------------------------------------------
