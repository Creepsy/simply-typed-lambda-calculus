module Main where

import Parser (expr)
import Text.Megaparsec (MonadParsec (eof), runParser, errorBundlePretty, PosState (PosState, pstateInput, pstateOffset, pstateSourcePos, pstateTabWidth, pstateLinePrefix), initialPos, defaultTabWidth)
import TypeChecker (typeCheck)

main :: IO ()
main = do
    let input = "\\y:Bool->Bool.\\ x: Bool . if x then (x x) else x"
        initPosState = PosState {
            pstateInput = input,
            pstateOffset = 0,
            pstateSourcePos = initialPos "",
            pstateTabWidth = defaultTabWidth,
            pstateLinePrefix = ""
        }
    let parseTree = runParser (expr <* eof) "" input
    either
        (\err -> do
            putStrLn $ errorBundlePretty err
        )
        (\pT -> do
            case typeCheck initPosState pT of
                Left err -> putStrLn $ errorBundlePretty err
                Right res -> print res
        )
        parseTree