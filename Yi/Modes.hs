module Yi.Modes (defaultFundamentalMode,
                 cleverHaskellMode,
 latexMode, cppMode, haskellMode, literateHaskellMode, cabalMode, srmcMode, defaultModeMap) where

import System.FilePath
import qualified Yi.Syntax.Haskell             as Haskell
import qualified Yi.Syntax.LiterateHaskell     as LiterateHaskell
import qualified Yi.Syntax.Latex               as Latex
import qualified Yi.Syntax.Srmc                as Srmc
import qualified Yi.Syntax.Cabal               as Cabal
import qualified Yi.Syntax.Cplusplus           as Cplusplus
import qualified Yi.Syntax.Fractal as Fractal
import qualified Yi.Syntax.Paren as Paren
import qualified Yi.Syntax.Alex as Alex
import Yi.Syntax.Alex (Tok(..), Posn(..))
import Yi.Syntax
import Yi.Keymap
import Yi.Indent
import Control.Arrow (first)
import Yi.Prelude
import Yi.Buffer (AnyMode(..), Mode(..), emptyMode)
import Prelude ()
import qualified Yi.IncrementalParse as IncrParser

fundamental, defaultFundamentalMode :: Mode syntax
latexMode, cppMode, haskellMode, literateHaskellMode, cabalMode, srmcMode :: Mode Alex.Result

fundamental = emptyMode
  { 
   modeIndent = autoIndentB
  }

mkHighlighter' initSt scan = Alex.mkHighlighter initSt (fmap (first tokenToStroke) . scan)
    where tokenToStroke (Tok t len posn) = (posnOfs posn, t, posnOfs posn + len)


cppMode = fundamental 
  {
   modeHL = ExtHL (mkHighlighter' Cplusplus.initState Cplusplus.alexScanToken)
  }

haskellMode = fundamental 
   {
    modeHL = ExtHL $
    Alex.mkHighlighter Haskell.initState (fmap (first tokenToStroke) . Haskell.alexScanToken)
   , modeIndent = autoIndentHaskellB

   }
    where tokenToStroke (Tok t len posn) = (posnOfs posn, Haskell.tokenToStyle t, posnOfs posn + len)

cleverHaskellMode :: Mode (Paren.Expr (Tok Haskell.Token))
cleverHaskellMode = fundamental {
    modeIndent = autoIndentHaskellB,
    modeHL = ExtHL $
{--    lexer `withScanner` IncrParser.mkHighlighter Fractal.parse
      (\begin end -> fmap tokenToStroke . Fractal.getStrokes begin end) id -}
--}

{--
    lexer `withScanner` IncrParser.mkHighlighter Paren.parse
      (\begin end t -> Paren.getStrokes begin end t) id
--}

--
    (Paren.indentScanner . lexer) `withScanner` IncrParser.mkHighlighter Paren.parse
      (\point begin end t -> Paren.getStrokes point begin end t) snd
--}                                 
}
    where lexer = Alex.lexScanner Haskell.alexScanToken Haskell.initState 


literateHaskellMode = haskellMode 
  {
   modeHL = ExtHL (mkHighlighter' LiterateHaskell.initState LiterateHaskell.alexScanToken)
  }

cabalMode = fundamental 
  {
    modeHL = ExtHL (mkHighlighter' Cabal.initState Cabal.alexScanToken)
  }

latexMode = fundamental 
  {
   modeHL = ExtHL (mkHighlighter' Latex.initState Latex.alexScanToken)
  }

srmcMode = fundamental 
   {
    modeHL = ExtHL (mkHighlighter' Srmc.initState Srmc.alexScanToken)
   }

defaultFundamentalMode = fundamental

defaultModeMap :: ReaderT FilePath Maybe AnyMode
defaultModeMap = ReaderT (modeFromExtension . takeExtension)




{-
  Working out the mode from the extension of
  the file. Some of these are a little questionably haskell
  related. For example ".x" is an alex lexer specification
  I dare say that there are other file types that use ".x"
  as the file extension.
  For now though this is probably okay given the users of
  'yi' are mostly haskell hackers, as of yet.
-}
modeFromExtension :: String -> Maybe AnyMode
modeFromExtension ".hs"    = Just $ AnyMode haskellMode
modeFromExtension ".x"     = Just $ AnyMode haskellMode
modeFromExtension ".lhs"   = Just $ AnyMode literateHaskellMode
modeFromExtension ".hsinc" = Just $ AnyMode haskellMode -- haskell include files such as Yi/Syntax/alex.hsinc
modeFromExtension ".cabal" = Just $ AnyMode cabalMode
modeFromExtension ".tex"   = Just $ AnyMode latexMode
modeFromExtension ".sty"   = Just $ AnyMode latexMode
modeFromExtension ".ltx"   = Just $ AnyMode latexMode
modeFromExtension ".cxx"   = Just $ AnyMode cppMode
modeFromExtension ".cpp"   = Just $ AnyMode cppMode
modeFromExtension ".C"     = Just $ AnyMode cppMode
modeFromExtension ".hxx"   = Just $ AnyMode cppMode
modeFromExtension ".H"     = Just $ AnyMode cppMode
modeFromExtension ".h"     = Just $ AnyMode cppMode
modeFromExtension ".c"     = Just $ AnyMode cppMode -- I treat c file as cpp files, most users are smart enough to allow for that.
modeFromExtension ".pepa"  = Just $ AnyMode srmcMode -- pepa is a subset of srmc    
modeFromExtension ".srmc"  = Just $ AnyMode srmcMode
modeFromExtension _        = Nothing
