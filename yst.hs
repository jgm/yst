{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Main
where
import Paths_yst
import Yst.Util
import Yst.Config
import Yst.Build
import System.FilePath
import System.Environment
import System.Directory
import System.Exit
import System.IO.UTF8
import System.IO (stderr)
import Prelude hiding (readFile, putStrLn, print, writeFile)
import Control.Monad

createSite :: FilePath -> IO ()
createSite path = do
  existsd <- doesDirectoryExist path
  existsf <- doesFileExist path
  when (existsd || existsf) $ do
    hPutStrLn stderr $ "Aborting!  " ++ path ++ " already exists."
    exitWith $ ExitFailure 5
  demoDir <- getDataFileName "demo"
  contents <- liftM (filter (/=".") . map (makeRelative demoDir)) $ getDirectoryContentsRecursive demoDir
  forM_ contents $ \file -> do
    let dest = path </> file 
    createDirectoryIfMissing True $ takeDirectory dest
    copyFile (demoDir </> file) dest
  hPutStrLn stderr $ "Created starter site in " ++ path

usageMessage :: IO ()
usageMessage = hPutStrLn stderr $
  "yst - create static website from string templates and YAML data\n" ++
  "Usage:\n" ++
  "yst [-f CONFIGFILE]        # build website\n" ++
  "yst create DIRECTORY       # create starter site in DIRECTORY"

main :: IO ()
main = do
  args <- getArgs
  site <- case args of
               ["-f",x]     -> parseConfigFile x
               []           -> parseConfigFile "config.yaml"
               ["create",d] -> createSite d >> exitWith ExitSuccess
               _            -> usageMessage >> exitWith (ExitFailure 1)
  buildSite site

