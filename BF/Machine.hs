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

    runMachine:: Machine -> IO ()
    runMachine m = void $ execStateT (runMaybeT runProgram') m

    runProgram' :: MaybeT (StateT Machine IO) ()
    runProgram' = forever runNextInstruction

    getInstruction:: MaybeT (StateT Machine IO) Instruction
    getInstruction = do
        (program, tape) <- lift get
        case program of
            (ins:rest) -> do
                lift $ put (rest, tape)
                return ins
            _ -> fail ""

    runNextInstruction:: MaybeT (StateT Machine IO) ()
    runNextInstruction = getInstruction >>= runInstruction

    runInstruction:: Instruction -> MaybeT (StateT Machine IO) ()
    runInstruction a = do
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
            Loop [] -> return ()
            Loop x -> runLoop x
            Noop -> return ()

    runLoop:: Program -> MaybeT (StateT Machine IO) ()
    runLoop prog = do
        n <- getTape
        unless (n == 0) $ do
            mapM_ runInstruction prog
            runLoop prog

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
