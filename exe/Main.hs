{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lens.Family
import Control.Exception (SomeException)
import System.Exit (ExitCode(..))

import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Control.Exception
import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.Import
import qualified Dhallix
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified Nix.Pretty
import qualified Options.Generic
import qualified System.Exit
import qualified System.IO

main :: IO ()
main = handle (Dhall.detailed (do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
    () <- Options.Generic.getRecord "Compile Dhall to Nix"

    inText <- Data.Text.IO.getContents

    expr <- case Dhall.Parser.exprFromText "(stdin)" inText of
        Left  err  -> Control.Exception.throwIO err
        Right expr -> return expr

    expr' <-
      StateT.evalStateT
        (Dhall.Import.loadWith expr)
        (Dhall.Import.emptyStatus "."
          & Dhall.Import.startingContext .~ Dhallix.context)
    case Dhall.TypeCheck.typeWith Dhallix.context expr' of
        Left  err -> Control.Exception.throwIO err
        Right _   -> return ()

    nix <- case Dhallix.dhallToNix expr' of
        Left err  -> Control.Exception.throwIO err
        Right nix -> return nix
    print (Nix.Pretty.prettyNix nix) ))

handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = case Control.Exception.fromException e of
        Just ExitSuccess -> do
            Control.Exception.throwIO e
        _ -> do
            System.IO.hPutStrLn System.IO.stderr ""
            System.IO.hPrint    System.IO.stderr e
            System.Exit.exitFailure
