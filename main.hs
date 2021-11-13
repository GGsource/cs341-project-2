import System.Environment (getArgs)
import Data.List
import System.Directory
import Data.String(words)

--This adds the line given to "directory" on a new line
addToDirectory :: [Char] -> [Char] -> [Char] -> FilePath -> IO ()
addToDirectory  fstName lstName phnNum outputFile = do
        appendFile outputFile  (fstName ++ " " ++ lstName ++ " " ++ phnNum ++ "\n")

linesToList :: FilePath -> IO [String]
linesToList path = do
        contents <- readFile path
        return (sort (lines contents))

printEntry :: Show a => [Char] -> a -> IO ()
printEntry string index = putStrLn (show index ++ ". " ++ string)

_printEntries :: (Show t, Num t) => [[Char]] -> t -> IO ()
_printEntries listOfEntries acc = do
        case listOfEntries of
                [] -> putStr ""
                hd:rest -> do
                        printEntry hd acc
                        _printEntries rest (acc + 1)

printEntries :: [[Char]] -> IO ()
printEntries listOfEntries = do
        case listOfEntries of
                [] -> putStrLn "Directory is empty."
                _ -> do
                        putStrLn "Listing all entries:"
                        _printEntries listOfEntries 0

removeIndex :: Int -> [a] -> [a]
removeIndex givenNdx givenList = do
        case givenList of
                [] -> []
                hd:rest -> do
                        case givenNdx of
                                0 -> rest
                                _ -> hd : removeIndex (givenNdx -1) rest

overwriteDirectory :: [String] -> IO ()
overwriteDirectory givenList = do
        case givenList of
                [] -> do
                        removeFile "directory"
                        renameFile "newdirectory" "directory"
                hd:rest -> do
                        appendFile "newdirectory" (hd ++ "\n")
                        overwriteDirectory rest

executeCommand :: [Char] -> IO ()
executeCommand cmd = do
        case cmd of
                "x" -> putStrLn "Exiting Homework 4, Goodbye!"
                "l" -> do
                        orderedEntries <- linesToList "directory"
                        printEntries orderedEntries
                        userPrompt
                _ -> do
                        putStrLn "Invalid command, try again."
                        userPrompt
executeCommandNdx :: [Char] -> [Char] -> IO()
executeCommandNdx cmd ndx = do
        orderedEntries <- linesToList "directory"
        let fixedList = removeIndex (read ndx) orderedEntries
        writeFile "newdirectory" ""
        overwriteDirectory fixedList
        userPrompt

executeCommandAdd :: [Char] -> [Char] -> [Char] -> [Char] -> IO()
executeCommandAdd cmd fstName lstName phnNum = do
        addToDirectory fstName lstName phnNum "directory"
        userPrompt

userPrompt = do
        putStrLn "Enter command: "
        getStr <- getLine
        let commandList = words getStr
        case commandList of
                x:y:[] -> if x == "r" then executeCommandNdx x y
                        else executeCommand (head commandList)
                x:y:z:n:[] -> if x == "a" then executeCommandAdd x y z n
                        else executeCommand (head commandList)
                _ -> executeCommand (head commandList)

main = do
        putStrLn "Welcome to Homework 4!"
        putStrLn "You may enter the following commands:"
        putStrLn "a - adds entry with given args"
        putStrLn "l - lists currently saved entries"
        putStrLn "r - removes the entry at the given index"
        putStrLn "x - exits this program"
        userPrompt
