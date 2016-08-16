module BF.Memory
(MemoryZipper,
incrementPointer,
decrementPointer,
incrementData,
decrementData,
getByte,
setByte
)
where
	import Data.Char

	type MemoryZipper = ([Int],[Int])

	incrementPointer :: MemoryZipper -> Maybe MemoryZipper
	incrementPointer ([], _) = Nothing
	incrementPointer (p:mem, prev) = return (mem, p:prev)

	decrementPointer :: MemoryZipper -> Maybe MemoryZipper
	decrementPointer (_, []) = Nothing
	decrementPointer (mem, p:prev) = return (p:mem, prev)

	incrementData :: MemoryZipper -> Maybe MemoryZipper
	incrementData ([], _) = Nothing
	incrementData (p:mem, prev) = return (mod (p+1) 256:mem, prev)

	decrementData :: MemoryZipper -> Maybe MemoryZipper
	decrementData ([], _) = Nothing
	decrementData (p:mem, prev) = return (mod ((p-1)+256) 256:mem, prev)

	getByte :: MemoryZipper -> Maybe Int
	getByte ([], _) = Nothing
	getByte (p:_, _) = return p

	setByte :: Int -> MemoryZipper -> Maybe MemoryZipper
	setByte b ([], _) = Nothing
	setByte b (p:mem, prev) = return (b:mem, prev)
