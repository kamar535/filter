module Filter where

import System.FilePath.Posix

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Control.Monad (void)
import Text.Regex    (subRegex, mkRegex)
import Data.Maybe    (catMaybes)

------------------------------------------------------------------------------
-- Data tyes

type Line        = String
type Replacement = String
type Needle      = String
type ResultLine  = String

data Action = Keep Line
            | Skip Line
            | SkipLines [Line]
            | Replace Line Replacement
            | ReplaceWith Needle Replacement Line ResultLine
            | Insert Line
           deriving Show

purge :: Action -> Maybe String
purge (Keep s)              = Just s
purge (Skip _)              = Nothing
purge (SkipLines _)         = Nothing
purge (Replace _ s)         = Just s
purge (ReplaceWith _ _ _ s) = Just s
purge (Insert s)            = Just s

getPrefix :: String -> Parser String
getPrefix ".py" = symbol "#"
getPrefix ".hs" = symbol "--"
getPrefix _     = symbol "#"

filterFile :: String -> IO ()
filterFile filePath = do
  print . takeExtension $ filePath
  let p = getPrefix . takeExtension $ filePath


  s <- readFile filePath
  case parse file filePath s of
    Left err -> putStr (parseErrorPretty err)

    -- POSIX ==> the last line of any file ends with a newline ==> an extra
    -- empty line will be parsed (sepBy line newline) at end of input ==> use
    -- init to drop last element of [Action].

    Right ss -> do
      mapM_ putStrLn $ catMaybes $ map purge $ init ss

file :: Parser [Action]
file = do
  ls <- sepBy filters newline
  eof
  return ls

-- | Try each of these filter parsers in order until one succeeds.
filters :: Parser Action
filters = choice [ try skipFilter
                 , try replaceWithFilter
                 , try replaceFilter
                 , try insertFilter
                 , try skipLinesFilter
                 , keepFilter]

------------------------------------------------------------------------------
-- White space

indentation :: Parser String
indentation = many $ char ' ' <|> tab

space' :: Parser ()
space' = void indentation

------------------------------------------------------------------------------
-- Symbols

symbol :: String -> Parser String
symbol = L.symbol space'

prefix, skip, replace, insert, start, end, with :: Parser String

prefix  = symbol "#"
skip    = symbol "SKIP"
replace = symbol "REPLACE"
insert  = symbol "INSERT"
start   = symbol "START"
end     = symbol "END"
with    = symbol "WITH"

------------------------------------------------------------------------------
-- Lexemes

lexeme = L.lexeme space'

arg :: Parser String
arg = lexeme $ manyTill anyChar spaceChar

------------------------------------------------------------------------------
-- Single line filter parsers

keepFilter :: Parser Action
keepFilter = many (noneOf "\n") >>= (return . Keep)

lineFilter :: Parser String -> (String -> String -> String -> Action) -> Parser Action
lineFilter p f = do
  indent <- indentation
  before <- many (noneOf "#\n")
  void $ prefix >> p
  after <- many (noneOf "\n")
  return $ f indent before after

replaceFilter, insertFilter, skipFilter :: Parser Action

replaceFilter = lineFilter replace (\i b a -> Replace (i ++ b) (i ++ a))
insertFilter  = lineFilter insert  (\i _ a -> Insert  (i ++ a))
skipFilter    = lineFilter skip    (\i b a -> Skip    (i ++ b ++ a))

replaceWithFilter :: Parser Action
replaceWithFilter = do
  i <- indentation
  a <- many (noneOf "#\n")
  void $ prefix >> replace
  b <- arg
  void with
  c <- arg
  space'
  let new = subRegex (mkRegex b) a c
  return (ReplaceWith b c (i ++ a) (i ++ new))

------------------------------------------------------------------------------
-- Multi-line filter parsers

skipLinesFilter :: Parser Action
skipLinesFilter = do
  void $ prefix >> start >> skip >> newline
  ss <- (manyTill anyChar (prefix >> end >> skip))
  return . SkipLines $ lines ss
