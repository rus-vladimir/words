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
-- RUSSIAN
russianNounsTxt :: BS.ByteString
russianNounsTxt = $(embedFile "russian_nouns.txt")

russianTopTxt :: BS.ByteString
russianTopTxt = $(embedFile "1000_words_ru.txt")

countriesRuTxt :: BS.ByteString
countriesRuTxt = $(embedFile "countries_ru.txt")

capitalsRuTxt :: BS.ByteString
capitalsRuTxt = $(embedFile "capitals_ru.txt")

-- ENGLISH

englishNounsTxt :: BS.ByteString
englishNounsTxt = $(embedFile "english_nouns.txt")

englishTopTxt :: BS.ByteString
englishTopTxt = $(embedFile "1500_english_nouns.txt")

-- DUTCH
dutchNounsTxt :: BS.ByteString
dutchNounsTxt = $(embedFile "dutch_words.txt")

dutchTopTxt :: BS.ByteString
dutchTopTxt = $(embedFile "400_dutch_words.txt")


getWordsByLang :: String -> BS.ByteString
getWordsByLang "ru" = russianTopTxt
getWordsByLang "en" = englishTopTxt
getWordsByLang "nl" = dutchTopTxt
getWordsByLang _ = russianTopTxt


getAllKnownWordsByLang :: String -> BS.ByteString
getAllKnownWordsByLang "ru" = russianTopTxt <> russianNounsTxt
getAllKnownWordsByLang "en" = englishTopTxt <> englishNounsTxt
getAllKnownWordsByLang "nl" = dutchTopTxt <> dutchNounsTxt
getAllKnownWordsByLang _ = russianTopTxt <> russianNounsTxt


getRandomNoun :: String -> Int -> IO String
getRandomNoun lang leng = do
    comp <- newRand leng 10
    sourceStrings <- readSourceFile $ getWordsByLang lang
    let filteredStrings = L.filter (\w -> L.length w == comp && wordHasOnlyLetters w) sourceStrings
    ind <- newRand 0 (L.length filteredStrings - 1)
    return $ filteredStrings !! ind

isKnownWord :: String -> String -> IO Bool
isKnownWord lang w =
    do
        readStrings <- readSourceFile $ getAllKnownWordsByLang lang
        let isKnownWord = L.any (stringEqualCaseInsensitive w) readStrings
        return isKnownWord

readSourceFile :: BS.ByteString -> IO [String]
readSourceFile sf = do
    let byteStrings = tokenise (C.pack "\n") sf
    let asString bs = DT.unpack $ DTE.decodeUtf8 bs
    return $ fmap asString byteStrings

tokenise :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
tokenise x y = h : if BS.null t then [] else tokenise x (BS.drop (BS.length x) t)
    where (h,t) = BS.breakSubstring x y
