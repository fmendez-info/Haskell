data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

headCL::CList a -> a
headCL EmptyCL = error "Lista vacia"
headCL (CUnit x) = x
headCL (Consnoc x _  _) = x

tailCL::CList a -> CList a
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y

isEmptyCL::CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit::CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

reverseCL::CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x xs y) = Consnoc y (reverseCL xs) x

initsCL::CList a -> CList a


lastsCL::CList a -> CList a


concatCL CList a -> CList a -> CList a