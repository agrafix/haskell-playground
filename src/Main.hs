module Main where

import Tail

import Control.Monad
import Options.Applicative

tailParser =
    runTail' <$> argument str (metavar "FILE")
             <*> lineP
    where
      runTail' a b = runTail a b >>= showTail
      lineP :: Parser Integer
      lineP =
          option auto $
          long "lines"
          <> short 'n'
          <> metavar "N"
          <> help "lines to read from EOF"

argParser :: Parser (IO ())
argParser =
    subparser
    ( command "tail" (info tailParser idm )
    )

main :: IO ()
main = join $ execParser (info argParser idm)
