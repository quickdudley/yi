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
import           Control.Monad.State.Class        (get)
import qualified Data.Attoparsec.Text as P
import           Data.List                        (null)
import qualified Data.Text as T                   (pack)

import Yi.Keymap                                  (Action(EditorA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (parse, pureExCommand)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand(cmdAction, cmdShow))
import           Yi.Editor                        (currentBuffer,newTabE,printMsg,switchToBufferE)

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
  void $ P.string "tab"
  parseTabSplit

parseTabSplit :: P.Parser ExCommand
parseTabSplit = do
  void $ P.many1 P.space
  P.string "split"
  return $ Common.pureExCommand {
    cmdShow = "tab split",
    cmdAction = EditorA $ do
      e <- get
      let br = currentBuffer e
      newTabE
      switchToBufferE br
   }
