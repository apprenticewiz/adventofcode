module Main ( main ) where

import Data.Int (Int32)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec
import Text.Parsec.String

data JValue = JObject [(String, JValue)]
            | JArray [JValue]
            | JString String
            | JNumber Int
            | JBool Bool
            | JNull
            deriving (Eq, Show)

usage :: String -> IO ()
usage progname = do
    hPutStrLn stderr $ "usage: " ++ progname ++ " <input file>"
    exitFailure

json :: Parser JValue
json = spaces *> jvalue <* spaces <* eof

jvalue :: Parser JValue
jvalue = choice
    [ JObject <$> jobject
    , JArray  <$> jarray
    , JString <$> jstring
    , JNumber <$> jnumber
    , JBool   <$> jbool
    , JNull   <$  string "null"
    ]

jobject :: Parser [(String, JValue)]
jobject = between (char '{' >> spaces) (spaces >> char '}')
              (pair `sepBy` (spaces >> char ',' >> spaces))
  where
    pair = do
           k <- jstring
           spaces >> char ':' >> spaces
           v <- jvalue
           return (k, v)

jarray :: Parser [JValue]
jarray = between (char '[' >> spaces) (spaces >> char ']')
         (jvalue `sepBy` (spaces >> char ',' >> spaces))

jstring :: Parser String
jstring = char '"' *> many (noneOf "\"") <* char '"'

jnumber :: Parser Int
jnumber = do
          sign <- option "" (string "-")
          digits <- many1 digit
          return (read (sign ++ digits))

jbool :: Parser Bool
jbool = (True <$ string "true") <|> (False <$ string "false")

sumJSON :: JValue -> Int
sumJSON (JNumber n) = n
sumJSON (JArray xs) = sum (map sumJSON xs)
sumJSON (JObject kvs)
  | any ((== JString "red") . snd) kvs = 0
  | otherwise                          = sum (map (sumJSON . snd) kvs)
sumJSON _           = 0

process :: String -> Int32
process content =
    case parse json "" content of
        Left err -> error (show err)
        Right ast -> fromIntegral $ sumJSON ast

main :: IO ()
main = do
    args <- getArgs
    progname <- getProgName
    case args of
        [filename] -> do
            content <- readFile filename
            let result = process content
            putStrLn $ "result = " ++ show result
        _ -> usage progname
