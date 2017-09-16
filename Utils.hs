-- Utils

module Utils where
    shownum = show

    hd :: [a] -> a
    hd = head

    tl :: [a] -> [a]
    tl = tail

    zip2 :: [a] -> [b] -> [(a,b)]
    zip2 = zip


    {-
        Heap: A simple implement of heap
    -}
    type HeapOperation = (Int, Int, Int)
    type Heap a = (Int, [Int], [(Int, a)], HeapOperation)
    type Addr = Int

    hInitial :: Heap a
    hInitial = (0, [1..], [], (0,0,0))

    -- takes a heap and an objectm and returns a new heap
    -- andd an address, the object is found and the  address
    hAlloc :: Heap a -> a -> (Heap a, Addr)
    hAlloc (size, next:free, cts, (allocNum, updateNum, freeNum)) n = ((size+1, free, (next,n):cts, (allocNum+1, updateNum, freeNum)), next)

    -- returns a heap in which the address is noew associated
    -- with the object
    hUpdate :: Heap a -> Addr -> a -> Heap a
    hUpdate (size,free,cts,(allocNum, updateNum, freeNum)) a n = (size, free, (a,n) : remove cts a,(allocNum, updateNum+1, freeNum))

    -- remvoe the specified object
    hFree :: Heap a -> Addr -> Heap a
    hFree (size, free, cts,(allocNum, updateNum, freeNum)) a = (size-1, a:free, remove cts a,(allocNum, updateNum, freeNum+1))

    -- returns the object associated with the address
    hLookup :: Heap a -> Addr -> a
    hLookup (_, _, cts,_) a = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))

    -- returns the address of all objects in the heap
    hAddressed :: Heap a -> [Addr]
    hAddressed (_, _, cts,_) = [addr | (addr, _) <- cts]

    -- returns the number of objects in the heap
    hSize :: Heap a -> Int
    hSize (size, _, _,_) = size

    -- an address guaranteed to differ from every address returned by hAlloc
    hNull :: Addr
    hNull = 0

    hIsnull :: Addr -> Bool
    hIsnull = (==) 0

    showaddr :: Addr -> String
    showaddr a = "#" ++ shownum a

    remove :: [(Int, a)] -> Int -> [(Int, a)]
    remove [] a = error ("Attempt to update or free noexistent address #" ++ shownum a)
    remove ((a', n) : cts) a | a' == a = cts
                             | otherwise = (a', n) : remove cts a

    aLookup [] a def = def
    aLookup ((a', n) : cts) a  def  | a' == a = n
                                    | otherwise = aLookup cts a def

    type ASSOC a b = [(a, b)]

    aDomain :: ASSOC a b -> [a]
    aDomain xs = [x | (x, _) <- xs]

    aRange :: ASSOC a b -> [b]
    aRange xs = [y | (_, y) <- xs]

    aEmpty = []


    mapAccuml :: (a -> b -> (a, c))
                -> a 
                -> [b]
                -> (a, [c])
    mapAccuml f acc [] = (acc, [])
    mapAccuml f acc (x:xs) = let (acc1, x') = f acc x
                                 (acc2, xs') = mapAccuml f acc1 xs
                             in (acc2, x':xs')
