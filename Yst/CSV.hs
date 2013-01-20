{-# LANGUAGE ScopedTypeVariables #-}
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

module Yst.CSV (readCSVFile)
where
import Yst.Types
import Yst.Util
import Text.CSV
-- Note: ghc >= 6.12 (base >=4.2) supports unicode through iconv
-- So we use System.IO.UTF8 only if we have an earlier version
#if MIN_VERSION_base(4,2,0)
import Prelude hiding (catch)
#else
import Prelude hiding (readFile, catch)
import System.IO.UTF8
#endif
import Control.Exception (catch, SomeException)

readCSVFile :: FilePath -> IO Node
readCSVFile f = catch (toNode `fmap` readFile f)
                      (\(e::SomeException) -> do
                         errorExit 11 ("Error parsing " ++ f ++ ": " ++ show e)
                         return NNil)
  where toNode = csvToNode . parseCSV' f . stripBlanks . filter (/='\r')

parseCSV' :: FilePath -> String -> CSV
parseCSV' f s = case parseCSV f s of
                     Left e    -> error $ "Error parsing " ++ f ++ ": " ++ show e
                     Right c   -> c

csvToNode :: CSV -> Node
csvToNode [] = NNil
csvToNode (fieldNames : records) =
  NList $ map (\record -> NMap $ zip fieldNames $ map fieldToNode record) records 

fieldToNode :: Field -> Node
fieldToNode s =
  case parseAsDate s of
       Nothing -> NString s
       Just d  -> NDate d
