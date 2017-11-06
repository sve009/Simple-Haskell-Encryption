import Data.Char
import System.Environment
import System.Directory
import System.IO

type Message = String
type Key = String
type Filepath = String

data Source = Message | Filepath

prepareKey :: [Char] -> [Char] -> [Char]
prepareKey msg key = take (length msg) (cycle key)

convertString :: [Char] -> [Int]
convertString = map ((\x -> if x < 50 then x + 128 else x) . ord) 

stringify :: [Int] -> [Char]
stringify = concat . map show  

unStringify :: [Char] -> [Int]
unStringify [] = []
unStringify (x:y:z:xs) = (read [x, y, z]):(unStringify xs)

encode :: [Char] -> [Char] -> [Char]
encode msg trueKey = stringify $ zipWith (+) (convertString msg) (convertString trueKey)

decode :: [Char] -> [Char] -> [Char]
decode msg trueKey = map (chr . \x -> if x > 127 then x - 128 else x) $ zipWith (-) (unStringify msg) (convertString trueKey)

process :: String -> Maybe ((String, Key) -> IO())
process "-e" = Just encrypt
process "-ef" = Just encryptFile
process "-d" = Just decrypt
process "-df" = Just decryptFile
process _ = Nothing

encrypt :: (Message, Key) -> IO()
encrypt (msg, key) = do
    let trueKey = prepareKey msg key
        cryptotext = encode msg trueKey
    print cryptotext

decrypt :: (Message, Key) -> IO()
decrypt (msg, key) = do
    let trueKey = prepareKey msg key
        normalText = decode msg trueKey
    print normalText 
    
encryptFile :: (Filepath, Key) -> IO()
encryptFile (path, key)  = do
    handle <- openFile path ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let trueKey = prepareKey contents key
        cryptoText = encode contents trueKey
    hPutStr tempHandle cryptoText
    hClose handle
    hClose tempHandle
    removeFile path
    renameFile tempName path

decryptFile :: (Filepath, Key) -> IO()
decryptFile (path, key) = do
    contents <- readFile path
    (tempName, tempHandle) <- openTempFile "." "temp"
    let trueKey = prepareKey contents key
        normalText = decode contents trueKey
    hPutStr tempHandle normalText
    hClose tempHandle
    removeFile path
    renameFile tempName path

main = do
    (command:args) <- getArgs
    let action = process command
        (source, key) = (args !! 0, args !! 1)
    case action of
        Just a -> a (source, key)
        Nothing -> print "You did not enter a correct flag. Flags are -e, -ef, and -d"
