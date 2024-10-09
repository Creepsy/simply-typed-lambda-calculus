module Main where

import Parser (expr)
import Text.Megaparsec (parseTest, MonadParsec (eof))

main :: IO ()
main = parseTest (expr <* eof) "\\ x :   Bool -> (   Bool->Bool  ) ->  Bool . if x then (x z) else y"