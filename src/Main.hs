module Main where

import Filter

import Options.Applicative

-- https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(syntax)#Inline_comments

import Control.Applicative (some)
-- stack exec -- opt-test --hello Bosse

-- NOTE: Many utilities use the convention to specify two consecutive dashes
-- (--) to signal "end of options at this point". Beyond this tag, no options
-- are are processed anymore, even if an argument begins with a dash.

data Config = Config { filePaths :: [String]
                     , actionPrefix :: String }
              deriving Show

config :: Parser Config
config = Config

         <$> some (argument str (metavar "FILE..." <> help "Paths to text files"))

         <*> strOption (   long "action-prefix"
                         <> short 'p'
                         <> value "#"
                         <> showDefault
                         <> metavar "ACTION-PREFIX"
                         <> help "The prefix used to denote the beginning of an inline comment"
                       )

filterFiles :: Config -> IO ()
filterFiles c = do
  putStrLn . show $ c
  mapM_ filterFile $ filePaths c
  
main :: IO ()
main = execParser opts >>= filterFiles
  where
    opts = info (helper <*> config)
      (    fullDesc
        <> progDesc "Filter one ore more text FILEs according to actions\
                    \specified as special comments in each text FILE."
      )
