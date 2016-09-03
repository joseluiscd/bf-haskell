module BF.Machine
where
    import BF.Tape
    import BF.Program
    import Control.Monad
    import Control.Monad.Trans.Maybe
    import Control.Monad.Trans.Class
    import Control.Monad.IO.Class
    import Control.Monad.Trans.State
    import Data.Char


    type Machine = (Program, TapeZipper)

    createMachine :: String -> Machine
    createMachine str = (readProgram str, (repeat 0, repeat 0))

    getInstruction:: MaybeT (StateT Machine IO) Instruction
    getInstruction = do
        (program, tape) <- lift get
        case program of
            (ins:rest) -> do
                lift $ put (rest, tape)
                return ins
            _ -> fail ""

    runInstruction:: MaybeT (StateT Machine IO) ()
    runInstruction = do
        a <- getInstruction
        (program, tape) <- lift get
        case a of
            Fw -> modifyTape incrementPointer
            Bw -> modifyTape decrementPointer
            Incr -> modifyTape incrementData
            Decr -> modifyTape decrementData
            Rd -> do
                ch <- liftIO getChar
                modifyTape $ setByte $ ord ch
            Prnt -> getTape >>= (liftIO . putChar . chr)




    modifyTape:: (TapeZipper -> Maybe TapeZipper) -> MaybeT (StateT Machine IO) ()
    modifyTape fn = do
        (prog, tape) <- lift get
        maybeTape (prog, fn tape) >>= lift . put

    getTape:: MaybeT (StateT Machine IO) Int
    getTape = do
        (prog, tape) <- lift get
        MaybeT $ return $ getByte tape



    maybeTape :: (Program, Maybe TapeZipper) -> MaybeT (StateT Machine IO) (Program, TapeZipper)
    maybeTape (_, Nothing) = MaybeT $ return Nothing
    maybeTape (p, Just t) = MaybeT $ return $ Just (p, t)


    --
    --
    -- runStep :: Machine -> StateT MaybeT IO Machine
    -- runStep (program, memory, stack) = do
    --     instruction <- liftMaybe $ getInstruction program
    --     liftIO $ print instruction
    --     prog <- liftMaybe $ advanceProgram program
    --     case instruction of
    --         '>' ->  do
    --             mem <- liftMaybe $ incrementPointer memory
    --             return (prog, mem, stack)
    --         '<' -> do
    --             mem <- liftMaybe $ decrementPointer memory
    --             return (prog, mem, stack)
    --         '+' -> do
    --             mem <- liftMaybe $ incrementData memory
    --             return (prog, mem, stack)
    --         '-' -> do
    --             mem <- liftMaybe $ decrementData memory
    --             return (prog, mem, stack)
    --
    --         '[' -> do
    --             b <- liftMaybe $ getByte memory
    --             (nprog, nstack) <- liftMaybe $ pushStack b prog stack
    --             return (nprog, memory, nstack)
    --
    --         ']' -> do
    --             b <- liftMaybe $ getByte memory
    --             (nprog, nstack) <- liftMaybe $ popStack b prog stack
    --             return (nprog, memory, nstack)
    --
    --         ',' -> do
    --             a <- liftIO getChar
    --             mem <- liftMaybe $ setByte (ord a) memory
    --             return (prog, mem, stack)
    --
    --         '.' -> do
    --             x <- liftMaybe $ getByte memory
    --             liftIO $ putChar $ chr x
    --             return (prog, memory, stack)
    --
    --         _ -> return (prog, memory, stack)
