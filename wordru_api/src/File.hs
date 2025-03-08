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
russianNounsTxt = $(embedFile "dict/russian_nouns.txt")

russianTopTxt :: BS.ByteString
russianTopTxt = $(embedFile "dict/1000_words_ru.txt")

countriesRuTxt :: BS.ByteString
countriesRuTxt = $(embedFile "dict/countries_ru.txt")

capitalsRuTxt :: BS.ByteString
capitalsRuTxt = $(embedFile "dict/capitals_ru.txt")

-- ENGLISH

englishNounsTxt :: BS.ByteString
englishNounsTxt = $(embedFile "dict/english_nouns.txt")

englishTopTxt :: BS.ByteString
englishTopTxt = $(embedFile "dict/1500_english_nouns.txt")

-- DUTCH
dutchNounsTxt :: BS.ByteString
dutchNounsTxt = $(embedFile "dict/dutch_words.txt")

dutchTopTxt :: BS.ByteString
dutchTopTxt = $(embedFile "dict/400_dutch_words.txt")


-- ROMANIAN
romanianNounsTxt :: BS.ByteString
romanianNounsTxt = $(embedFile "dict/romanian_dict.txt")

romanianTopTxt :: BS.ByteString
romanianTopTxt = $(embedFile "dict/romanian_nouns.txt")


getWordsByLang :: String -> BS.ByteString
getWordsByLang "ru" = russianTopTxt
getWordsByLang "en" = englishTopTxt
getWordsByLang "nl" = dutchTopTxt
getWordsByLang "ro" = romanianTopTxt
getWordsByLang _ = englishTopTxt


getAllKnownWordsByLang :: String -> BS.ByteString
getAllKnownWordsByLang "ru" = russianTopTxt <> russianNounsTxt
getAllKnownWordsByLang "en" = englishTopTxt <> englishNounsTxt
getAllKnownWordsByLang "nl" = dutchTopTxt <> dutchNounsTxt
getAllKnownWordsByLang "ro" = romanianTopTxt <> romanianNounsTxt
getAllKnownWordsByLang _ = englishTopTxt <> englishTopTxt


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
