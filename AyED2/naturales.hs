data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int 

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat

add :: Nat -> Nat -> Nat
--add m n = int2nat (nat2int m + nat2int n)
add n Zero = n
add n (Succ m) = Succ (add m n)

mult :: Nat -> Nat -> Nat


exp :: Nat -> Nat -> Nat