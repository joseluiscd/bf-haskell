module BF.Program
(Program,
Instruction(Incr, Decr, Fw, Bw, Prnt, Rd, Loop, End, Noop),
readProgram, testProgram) where

    import Data.Char
    import Control.Monad.Trans.Writer
    import Control.Monad.Trans.State
    import Control.Monad.Trans.Class
    import Control.Monad.Trans.Maybe

    data Instruction = Incr | Decr | Fw | Bw | Prnt | Rd | Loop [Instruction] | End | Noop deriving Show

    type Program = [Instruction]

    item:: MaybeT (State String) Char
    item = do
        x <- lift get
        case x of
            (a:rest) -> do
                lift $ put rest
                return a
            _ -> MaybeT $ return Nothing

    readProgram :: String -> Program
    readProgram prg = flip evalState prg $ execWriterT readProgram'


    readProgram' :: WriterT [Instruction] (State String) ()
    readProgram' = do
        i <- lift $ runMaybeT readInstr
        case i of
            Nothing -> return ()
            Just x -> do
                tell [x]
                readProgram'

    writeProgram :: Program -> String
    writeProgram = foldr fld "" where
        fld Incr a = '+':a
        fld Decr a = '-':a
        fld Fw a = '>':a
        fld Bw a = '<':a
        fld Prnt a = '.':a
        fld Rd a = ',':a
        fld (Loop x) a = '[': writeProgram x ++ (']':a)
        fld Noop a = '\n':a

    readInstr:: MaybeT (State String) Instruction
    readInstr = do
        x <- item
        case x of
            '[' -> lift readLoop
            ']' -> return End
            '+' -> return Incr
            '-' -> return Decr
            '>' -> return Fw
            '<' -> return Bw
            '.' -> return Prnt
            ',' -> return Rd
            _ -> return Noop


    readLoop:: State String Instruction
    readLoop = do
        a <- execWriterT readLoop'
        return $ Loop a

    readLoop':: WriterT [Instruction] (State String) ()
    readLoop' = do
        instr <- lift $ runMaybeT readInstr
        case instr of
            Nothing -> return ()
            Just End -> return ()
            Just x -> do
                tell [x]
                readLoop'
