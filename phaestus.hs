module Main
    where

import Data.Char (toUpper, toLower)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Text (pack, replace, strip, unpack)
import Text.Regex.TDFA (getAllTextMatches, (=~))


main = putStrLn (doshit ["2 0 1 2 0 5 0 7",
                         "GIRERING TILL KONTO 1 2 3 4 5 6 - 0",
                         "Avsändarkonto",
                         "1 2 3 4 5 6 - 7",
                         "B L O O Z E , J A N N E ALFONSIA",
                         "FISKERIEVÄGEN 2 9 C l g h 2 0 5 3",
                         "3 2 5 9 9 BORTA",
                         "m e d l 1 2 3 4 Blop L i n d a l l s s o n - 0 8",
                         "AVSER REFERENS 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 URSP.BEL SEK 3 5 0 , 0 0 S I D A 1 ( 1 )"])

doshit :: [String] -> String
doshit raw_str = (unlines (getdata raw_str))

getdata :: [String] -> [String]
getdata rawLines =
    [ name, address, zipcode, postaddress, date
    , membernr, confidence, unchanged, payment, message ]
  where
    toTitle :: String -> String
    toTitle (x:xs) = toUpper x : map toLower xs

    killspace :: String -> String
    killspace str = intercalate ", " $
        map (unwords . map(toTitle . filter (/= ' ')) . getAllTextMatches .
        (=~ "(([^ ]( |$))+)|([^ ]+)")) (splitOn "," str)

    getAddress :: String -> String
    getAddress rawLine =
        case streetNum of
            "" -> toTitle rawLine
            _  -> killspace $ take (length rawLine - length streetNum) rawLine
                  ++ " " ++  (intercalate " Lgh " . splitOn "lgh" .
                              filter (/= ' ') $ tail streetNum)
      where streetNum = rawLine =~ "[^0-9]([0-9] ?)+[a-zA-Z]? ?([lL] ?[gG] ?[hH] ?([0-9] ?)*)?$"

    getPostAddress :: String -> (String, String)
    getPostAddress rawLine =
        case rawzipcode of
            "" -> (f $ pack rawLine, "-1")
            _  -> (f $ replace (pack rawzipcode) (pack "") (pack rawLine),
                  (filter (/= ' ') rawzipcode))
      where
        f = toTitle . unpack . strip
        rawzipcode = rawLine =~ "([0-9] ?){5}"

    getDate :: String -> String
    getDate rawLine =
        (take 4 line) ++ "-" ++ (slice 4 6 line) ++ "-" ++
            (slice 6 8 line)
      where line = filter (/= ' ') . take 15 $ rawLine

    getPayment :: String -> String
    getPayment rawLine =
        drop 3 $ (filter (/= ' ') rawLine) =~ "SEK[0-9]+,[0-9]{2}"

    -- Generate the shit
    name = killspace $ rawLines !! 4
    address = getAddress $ rawLines !! 5
    (postaddress, zipcode) = getPostAddress $ rawLines !! 6
    date = getDate $ rawLines !! 0
    membernr = "1234"
    confidence = "no"
    unchanged = "very much"
    payment = getPayment $ rawLines !! (length rawLines - 1)
    message = unlines $ slice 7 (-1) rawLines

slice :: Int -> Int -> [a] -> [a]
slice start end x
    | end < 0   = drop start $ take (length x + end) x
    | otherwise = drop start $ take end x
