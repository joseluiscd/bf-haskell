module BF.Environment
where
    import BF.Memory
    import Control.Monad
    import Control.Monad.Trans.Maybe
    import Control.Monad.Trans.Class
    import Data.Char

    type ProgramZipper = (String, String)
    type Environment = (ProgramZipper, MemoryZipper, [ProgramZipper])

    createEnvironment :: String -> Environment
    createEnvironment str = ((str, []), (replicate 10 0, []), [])

    runProgram :: Environment -> IO (Maybe Environment)
    runProgram env = do
        res <- runStep env
        case res of
            Just newenv -> runProgram newenv
            Nothing -> return res

    runStep :: Environment -> IO (Maybe Environment)
    runStep (program, memory, stack) = case getInstruction program of
        Nothing -> return Nothing
        Just '>' -> return $ do
            prog <- advanceProgram program
            mem <- incrementPointer memory
            return (prog, mem, stack)
        Just '<' -> return $ do
            prog <- advanceProgram program
            mem <- decrementPointer memory
            return (prog, mem, stack)
        Just '+' -> return $ do
            prog <- advanceProgram program
            mem <- incrementData memory
            return (prog, mem, stack)
        Just '-' -> return $ do
            prog <- advanceProgram program
            mem <- decrementData memory
            return (prog, mem, stack)
        Just '[' -> return $ do
            prog <- advanceProgram program
            b <- getByte memory
            (newprog, newstack) <- pushStack b prog stack
            return (newprog, memory, newstack)

        Just ']' -> return $ do
            prog <- advanceProgram program
            b <- getByte memory
            (nprog, nstack) <- popStack b prog stack
            return (nprog, memory, nstack)

        Just ',' -> do
            a <- getChar
            return $ do
                prog <- advanceProgram program
                mem <- setByte (ord a) memory
                return (prog, mem, stack)

        Just '.' -> runMaybeT $ do
            x <- MaybeT $ return $ getByte memory
            lift $ putChar $ chr x
            prog <- MaybeT $ return $ advanceProgram program
            return (prog, memory, stack)

        Just _ -> return $ do
            prog <- advanceProgram program
            return (prog, memory, stack)

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
