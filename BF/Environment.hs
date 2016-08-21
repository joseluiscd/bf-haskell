module BF.Environment
where
    import BF.Memory
    import Control.Monad
    import Control.Monad.Trans.Maybe
    import Control.Monad.Trans.Class
    import Control.Monad.IO.Class
    import Data.Char


    type ProgramZipper = (String, String)
    type Environment = (ProgramZipper, MemoryZipper, [ProgramZipper])

    createEnvironment :: String -> Environment
    createEnvironment str = ((str, []), (replicate 100 0, replicate 100 0), [])

    liftMaybe :: Monad m => Maybe t -> MaybeT m t
    liftMaybe c = MaybeT $ return c

    runProgram :: Environment -> IO (Maybe Environment)
    runProgram env = do
        res <- runStep env
        print res
        --getChar
        case res of
            Just newenv -> runProgram newenv
            Nothing -> return res

    runStep :: Environment -> IO (Maybe Environment)
    runStep (program, memory, stack) = runMaybeT $ do
        instruction <- liftMaybe $ getInstruction program
        liftIO $ print instruction
        prog <- liftMaybe $ advanceProgram program
        case instruction of
            '>' ->  do
                mem <- liftMaybe $ incrementPointer memory
                return (prog, mem, stack)
            '<' -> do
                mem <- liftMaybe $ decrementPointer memory
                return (prog, mem, stack)
            '+' -> do
                mem <- liftMaybe $ incrementData memory
                return (prog, mem, stack)
            '-' -> do
                mem <- liftMaybe $ decrementData memory
                return (prog, mem, stack)

            '[' -> do
                b <- liftMaybe $ getByte memory
                (nprog, nstack) <- liftMaybe $ pushStack b prog stack
                return (nprog, memory, nstack)

            ']' -> do
                b <- liftMaybe $ getByte memory
                (nprog, nstack) <- liftMaybe $ popStack b prog stack
                return (nprog, memory, nstack)

            ',' -> do
                a <- liftIO getChar
                mem <- liftMaybe $ setByte (ord a) memory
                return (prog, mem, stack)

            '.' -> do
                x <- liftMaybe $ getByte memory
                liftIO $ putChar $ chr x
                return (prog, memory, stack)

            _ -> return (prog, memory, stack)

    getInstruction :: ProgramZipper -> Maybe Char
    getInstruction ([], _) = Nothing
    getInstruction (instr:_, _) = return instr

    advanceProgram :: ProgramZipper -> Maybe ProgramZipper
    advanceProgram ([], _) = Nothing
    advanceProgram (inst:prog, prev) = return (prog, inst:prev)

    popStack :: Int -> ProgramZipper -> [ProgramZipper] -> Maybe (ProgramZipper, [ProgramZipper])
    popStack c prog [] = Nothing
    popStack c prog (nprog:stack)
        | c /= 0 = return (nprog, nprog:stack)
        | c == 0 = return (prog, stack)


    pushStack :: Int -> ProgramZipper -> [ProgramZipper] -> Maybe (ProgramZipper, [ProgramZipper])
    pushStack c prog stack
        | c /= 0 = return (prog, prog:stack)
        | c == 0 = do
            retprog <- findMatchingBracket 0 prog
            return (retprog, stack)

    findMatchingBracket :: Int -> ProgramZipper -> Maybe ProgramZipper
    findMatchingBracket _ ([],_) = Nothing
    findMatchingBracket n (c:prog, rest)
        | c == '[' = findMatchingBracket (n+1) (prog, c:rest)
        | c == ']' && n == 0 = return (prog, c:rest)
        | c == ']' = findMatchingBracket (n-1) (prog, c:rest)
        | otherwise = return (prog, c:rest)
