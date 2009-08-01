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

module Yst.Yaml (readYamlFile, nodeToYamlNode)
where
import Yst.Types
import Yst.Util
import Data.Yaml.Syck
import Data.Time
import System.Locale (defaultTimeLocale)
import Prelude hiding (readFile)
import System.IO.UTF8

readYamlFile :: FilePath -> IO Node
readYamlFile f = catch (readFile f >>= parseYaml >>= return . yamlNodeToNode)
                   (\e -> errorExit 11 ("Error parsing " ++ f ++ ": " ++ show e) >> return NNil)

yamlNodeToNode :: YamlNode -> Node
yamlNodeToNode n =
  case n_elem n of
        EStr s  -> case parseAsDate (unpackBuf s) of
                        Nothing  -> NString (unpackBuf s)
                        Just d   -> NDate d
        EMap xs | all (\(k,_) -> isStrNode k) xs -> NMap pairs
                     where pairs = map mkPair xs
                           mkPair (k,v) = (strFrom k, yamlNodeToNode v)
        EMap _  -> error "Map keys must all be strings."
        ESeq xs -> NList $ map yamlNodeToNode xs
        ENil    -> NNil

nodeToYamlNode :: Node -> YamlNode
nodeToYamlNode n =
  case n of
       NString s -> mkNode (EStr $ packBuf s)
       NDate s   -> mkNode (EStr $ packBuf $ formatTime defaultTimeLocale "%x" s)
       NMap xs   -> mkNode (EMap $ map (\(k,v) -> (nodeToYamlNode (NString k), nodeToYamlNode v)) xs)
       NList xs  -> mkNode (ESeq $ map nodeToYamlNode xs)
       NNil      -> mkNode ENil

isStrNode :: YamlNode -> Bool
isStrNode x = case n_elem x of
                   EStr _ -> True
                   _      -> False

strFrom :: YamlNode -> String
strFrom x = case n_elem x of
                 EStr z   -> unpackBuf z
                 _        -> error "expected EStr node"
