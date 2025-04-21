data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
--nat2int 

int2nat :: Int -> Nat
int2nat 0 = Zero
--int2nat

addN :: Nat -> Nat -> Nat
--addN m n = int2nat (nat2int m + nat2int n)
addN n Zero = n
addN n (Succ m) = Succ (addN m n)

--multN :: Nat -> Nat -> Nat

--expN :: Nat -> Nat -> Nat






