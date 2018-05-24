{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module     : Yi.Keymap.Vim.Ex.Commands.Tab
-- License     : GPL-2
-- Maintainer  : yi-devel@googlegroups.com
-- Stability   : experimental
-- Portability : portable

module Yi.Keymap.Vim.Ex.Commands.Tab (parse) where

import           Control.Applicative              (Alternative(..))
import           Control.Monad                    (void)
import qualified Data.Attoparsec.Text as P        (anyChar, char, string)
import           Data.List                        (null)
import qualified Data.Text as T                   (pack)

import Yi.Keymap                                  (Action(EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand(cmdAction, cmdShow))
import           Yi.Editor                        (printMsg)

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
  void $ P.string "tab"
  n <- (some (P.char ' ') *> many (P.anyChar))
  return $ Common.pureExCommand {
    cmdShow = mappend "tab " (T.pack n),
	cmdAction = EditorA $ do
	  printMsg "Not yet implemented!"
   }