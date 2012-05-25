import Text.Pandoc

main = toJsonFilter includeCodeBlock
  where includeCodeBlock (cb@(CodeBlock (attr@(_, _, kvs)) _)) =
          case lookup "include" kvs of
            Just f -> readFile f >>= return . CodeBlock attr . chopNewline
            Nothing -> return cb
        includeCodeBlock b = return b

        chopNewline s = case reverse s of
            '\n' : rs -> reverse rs
            _ -> s
