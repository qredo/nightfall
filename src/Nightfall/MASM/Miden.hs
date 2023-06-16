module Nightfall.MASM.Miden where

import Data.List
import Data.Word
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Text.Read

import Nightfall.MASM
import Nightfall.MASM.Types

type MidenFelt = Word64 -- actually a bit smaller, [0..(2^64 - 2^32 + 1)]

data KeepFile = Keep FilePath | DontKeep
  deriving Show

whenKeep :: KeepFile -> (FilePath -> IO a) -> IO (Maybe a)
whenKeep k f = case k of
  DontKeep -> return Nothing
  Keep fp  -> Just <$> f fp

runMiden :: KeepFile -> Module -> IO (Either String [MidenFelt])
runMiden keep m = withSystemTempFile "nightfall-testfile-XXX.masm" $ \fp hndl -> do
    hPutStrLn hndl (ppMASM m)
    hClose hndl
    whenKeep keep $ \masmModSaveFp -> copyFile fp masmModSaveFp
    (ex, midenout, midenerr) <- readProcessWithExitCode "miden" ["run", "--assembly", fp] ""
    case ex of
        ExitSuccess -> do
            let xs = filter ("Output: " `isPrefixOf`) (lines midenout)
            case xs of
                [outline] -> do
                  let mstack = (readMaybe :: String -> Maybe [MidenFelt]) (drop 8 outline)
                  case mstack of
                    Nothing -> return (Left "couldn't decode stack")
                    Just stack -> return (Right stack)
                _ -> return (Left $ "unexpected miden run output: " ++ show (xs, midenout, midenerr))
        ExitFailure e -> return (Left $ "miden run failed: " ++ show (e, midenout, midenerr))

runMidenProve :: Module -> IO (FilePath, FilePath, String)
runMidenProve m = do
    fp <- writeSystemTempFile "nightfall-testfile-XXX.masm" (ppMASM m)
    let outFile = fp <.> "out"
        proofFile = fp <.> "proof"
    (ex, midenout, midenerr) <-
      readProcessWithExitCode "miden"
        ["prove", "--assembly", fp, "-o", outFile, "-p", proofFile] ""
    -- putStrLn midenout
    -- putStrLn midenerr
    case ex of
        ExitSuccess ->
            case  map (takeWhile (/='.') . drop (length hashPrefix)) $ filter (hashPrefix `isPrefixOf`) (lines midenout) of
                [hash] -> return (outFile, proofFile, hash)
                _ -> error "couldn't determine hash from miden prove output"
        ExitFailure n -> error $ "miden prove failed: " ++ show (n, midenout, midenerr)

  where hashPrefix = "Proving program with hash "

runMidenVerify :: FilePath -> FilePath -> String -> IO (Maybe (ExitCode, String, String))
runMidenVerify out proof hash = do
    (ex, mout, merr) <- readProcessWithExitCode "miden"
      ["verify", "-p", proof, "-o", out, "-h", hash] ""
    -- putStrLn mout
    -- putStrLn merr
    case ex of
        ExitSuccess -> return Nothing
        ExitFailure _ -> return $ Just (ex, mout, merr)
