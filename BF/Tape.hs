module BF.Tape
(TapeZipper,
incrementPointer,
decrementPointer,
incrementData,
decrementData,
getByte,
setByte
)
where
	import Data.Char

	type TapeZipper = ([Int],[Int])

	incrementPointer :: TapeZipper -> Maybe TapeZipper
	incrementPointer ([], _) = Nothing
	incrementPointer (p:mem, prev) = return (mem, p:prev)

	decrementPointer :: TapeZipper -> Maybe TapeZipper
	decrementPointer (_, []) = Nothing
	decrementPointer (mem, p:prev) = return (p:mem, prev)

	incrementData :: TapeZipper -> Maybe TapeZipper
	incrementData ([], _) = Nothing
	incrementData (p:mem, prev) = return (mod (p+1) 256:mem, prev)

	decrementData :: TapeZipper -> Maybe TapeZipper
	decrementData ([], _) = Nothing
	decrementData (p:mem, prev) = return (mod ((p-1)+256) 256:mem, prev)

	getByte :: TapeZipper -> Maybe Int
	getByte ([], _) = Nothing
	getByte (p:_, _) = return p

	setByte :: Int -> TapeZipper -> Maybe TapeZipper
	setByte b ([], _) = Nothing
	setByte b (p:mem, prev) = return (b:mem, prev)
