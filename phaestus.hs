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
doshit raw_str = (intercalate "\n" (getdata raw_str))

getdata :: [String] -> [String]
getdata raw_lines =
    [ name, address, zipcode, postaddress, date
    , membernr, certainty, unchanged, payment, message ]
  where
    toTitle :: String -> String
    toTitle str = (toUpper $ head str) : (map toLower $ tail str)

    killspace :: String -> String
    killspace str = intercalate ", " $
        [unwords . map (toTitle . filter (/= ' ')) .
         getAllTextMatches $
         x =~ "(([^ ]( |$))+)|([^ ]+)" | x <- splitOn "," str]

    getAddress :: String -> String
    getAddress rawLine =
        case streetNum of
            "" -> toTitle rawLine
            _  -> killspace $ take (length rawLine - length streetNum) rawLine
                  ++ " " ++  (intercalate " Lgh " . splitOn "lgh" .
                              filter (/= ' ') $ tail streetNum)
      where
        streetNum = rawLine =~ "[^0-9]([0-9] ?)+[a-zA-Z]? ?([lL] ?[gG] ?[hH] ?([0-9] ?)*)?$"


    getPostAddress :: String -> (String, String)
    getPostAddress rawLine =
        case rawzipcode of
            "" -> (toTitle . unpack $ strip $ pack rawLine, "-1")
            _  -> (unpack . strip $ replace (pack rawzipcode) (pack "") (pack rawLine),
                  (filter (/= ' ') rawzipcode))
      where
        rawzipcode = rawLine =~ "([0-9] ?){5}"

    name = killspace $ raw_lines !! 4
    address = getAddress $ raw_lines !! 5
    (postaddress, zipcode) = getPostAddress $ raw_lines !! 6
    date = raw_lines !! 0
    membernr = "1234"
    certainty = "no"
    unchanged = "very much"
    payment = "24987"
    message = unlines . drop 7 $ init raw_lines


    --unpack $ toLower $ Text.tail str
--toUpper $ head str ++
