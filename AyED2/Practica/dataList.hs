data List a = Nil | Cons a (List a)

head :: List a -> a
head (Cons x xs) = x

tail :: List a -> List a
tail (Cons x xs) = xs

myConcat :: List a -> List a
myConcat Nil = Nil
myConcat (Cons xs xss) = pegar xs (myConcat xss)

pegar :: List a -> List a -> List a
pegar Nil ys = ys
pegar xs Nil = xs
pegar (Cons x xs) ys = Cons x (pegar xs ys)