import BF.Machine
import System.IO
import System.IO.Error
import System.Environment
import Control.Monad
import BF.Program

main = catchIOError mainAction handler

mainAction = do
    args <- getArgs
    realMain args

realMain [] = putStrLn "Error, need file"
realMain args = withFile (head args) ReadMode (\handle -> do
        dat <- hGetContents handle
        let env = createMachine dat
        runMachine env
        return ()
        )

handler :: IOError -> IO ()
handler err
    | isDoesNotExistError err = putStrLn "File does not exist"
    | isEOFError err = return ()
    | otherwise = ioError err
