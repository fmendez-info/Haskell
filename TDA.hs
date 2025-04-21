module Set (Set,  emptySet, setEmpty, inSet, addSet, delSet) where
newtype Set a = Set [a] deriving Show

emptySet    ::  Set a
setEmpty    ::  Set a -> Bool
inSet       ::  Eq a => a -> Set a -> Bool
addSet      ::  Eq a => a -> Set a -> Set a
delSet      ::  Eq a => a -> Set a -> Set a
unionSet    ::  Eq a => Set a -> Set a -> Set a

emptySet = Set []

setEmpty (Set []) = True
setEmpty (Set [a]) = False

inSet _ (Set[]) = False
inSet x (Set (y:ys)) = if x == y then
                            True
                        else
                            inSet x (Set ys)

addSet x (Set[]) = Set [x]
addSet x (Set xs) = if inSet x (Set xs) then
                        Set xs
                    else
                        Set (x:xs)

delSet _ (Set[]) = Set []
delSet x (Set (y:ys)) = if x == y then
                            Set ys
                        else
                            addSet y (delSet x (Set ys))
{- prueba
delSet x (Set (y:ys)) = if setEmpty (Set (y:ys)) then
                            error "set vacio"
                        else
                            if x == y then
                                Set ys
                            else
                                addSet y (delSet x (Set ys))
-}

unionSet (Set xs) (Set[]) = Set xs
--unionSet (Set xs) (Set (y:ys)) = addSet y (S xs) and delSet y (S ys)
unionSet (Set (x:xs)) (Set ys) = if inSet x (Set ys) then
                                    unionSet (Set xs) (Set ys)
                                else
                                    unionSet (Set xs) (Set (x:ys))