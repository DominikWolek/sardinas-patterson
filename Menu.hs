module Menu (
    showMenu,
    checkFile
) where

import Algorithm
import System.IO

showMenu :: IO ()
showMenu = do
    putStrLn "1 - Check set of words in terminal"
    putStrLn "2 - Check set of words written in file"
    anwser <- getLine
    case anwser of
        "1" -> checkInTerminal
        "2" -> checkFileMenu
        otherwise -> do
            putStrLn $ "What?\n"
            showMenu

checkInTerminal :: IO ()
checkInTerminal = do
    putStrLn "Write down words of code in separate lines"
    putStrLn "Finish reading set of words by writing \".\""
    words <- getListUntil "." []

    showResult (Algorithm.isListCode words)

getListUntil :: String -> [String] -> IO [String]
getListUntil end list = do
    line <- getLine
    if line == end
        then return $ reverse list
        else getListUntil end (line:list)

checkFileMenu :: IO ()
checkFileMenu = do
    putStrLn "Write path to the file"
    path <- getLine
    checkFile path

checkFile :: FilePath -> IO ()
checkFile path = do
    fileRaw <- readFile path
    (showResult.(Algorithm.isListCode).splitLines) fileRaw
    where
        split on = foldl (\(y:ys) x -> if (on x) then ([]:(y:ys)) else ((y ++ [x]):ys)) [[]]
        splitLines = reverse.(\x -> if (head x) == [] then (tail x) else x).(split (== '\n'))

showResult :: Bool -> IO ()
showResult result = do
    case result of
        True -> putStrLn "This set of words can be code."
        False -> putStrLn "This set of words CANNOT be code."
