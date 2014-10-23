module CVSUtils
( parseCSV
, showCSV
, colFields
, readCSV
, writeCSV
, Separator
, Document
, CSV
, Entry
, Field
) where

import Data.List
import Data.List.Split

type Separator = String
type Document = String
type CSV = [Entry]
type Entry = [Field]
type Field = String
doc = "John;Doe;15\nTom;Sawyer;12\nAnnie;Blake;20"
brokenDoc = "One;Two\nThree;Four;Five"

isValid :: CSV -> Bool
isValid s = (length $ nub $ [length field | field <- s]) == 1

parseCSV :: Separator -> Document -> CSV
parseCSV delim s 
  | delim!!0 `notElem` s = error $ "The character '"++delim++"' does not occur in the text"
  | not $ isValid $ [splitOn delim entry | entry <- (splitOn "\n" s)] = 
  error "The CSV file is not well-formed"
  | otherwise = [splitOn delim entry | entry <- (splitOn "\n" s)]
  
showCSV :: Separator -> CSV -> Document
showCSV delim csv
  | not $ isValid csv = error "The CSV file is not well-formed"
  | otherwise = intercalate "\n" [intercalate delim entry | entry <- csv]
  
colFields :: Int -> CSV -> [Field]
colFields i csv 
  | i >= (length $ csv!!0) || i < 0 = 
  error $ "There is no column "++(show i)++" in the CSV document"
  | otherwise = [entry!!i | entry <- csv]

  
readCSV :: Separator -> FilePath -> IO CSV
readCSV delim filePath = do
  f <- readFile filePath
  return $ parseCSV delim f
  

writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV delim filePath csv = do
  io <- writeFile filePath $ showCSV delim csv
  return io