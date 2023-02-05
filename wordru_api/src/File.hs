{-# LANGUAGE TemplateHaskell #-}

module File where

import Data.FileEmbed
import qualified Data.ByteString as BS
import Data.Text.Encoding as DTE
import qualified Data.Text as DT
import qualified GHC.List as L
import qualified Data.ByteString.Char8 as C
import System.Random
import Data.Char
import Core

newRand :: Int -> Int -> IO Int
newRand l u = randomRIO (l,u):: IO Int

russianNounsTxt :: BS.ByteString
russianNounsTxt = $(embedFile "russian_nouns.txt")

countriesRuTxt :: BS.ByteString
countriesRuTxt = $(embedFile "countries_ru.txt")

capitalsRuTxt :: BS.ByteString
capitalsRuTxt = $(embedFile "capitals_ru.txt")

getRandomNoun :: Int -> IO String
getRandomNoun l = do
    comp <- newRand 3 10
    sourceStrings <- readSourceFile
    let filteredStrings = L.filter (\w -> L.length w == comp && wordHasOnlyLetters w) sourceStrings
    ind <- newRand 0 (L.length filteredStrings - 1)
    return $ filteredStrings !! ind

isKnownWord :: String -> IO Bool
isKnownWord w =
    do
        readStrings <- readSourceFile
        let isKnownWord = L.any (stringEqualCaseInsensitive w) readStrings
        return isKnownWord

readSourceFile :: IO [String]
readSourceFile = do
    let byteStrings = tokenise (C.pack "\n") russianNounsTxt
    let asString bs = DT.unpack $ DTE.decodeUtf8 bs
    return $ fmap asString byteStrings

tokenise :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
    where (h,t) = BS.breakSubstring x y
