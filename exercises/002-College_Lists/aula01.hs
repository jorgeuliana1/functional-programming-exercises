double n = 2 * n
quadruple n = 2 * double n
factorial n = product [1 .. n]
average xs = div (sum xs) (length xs)

functionN = div a (length xs)
  where
     a = 10
     xs = [1, 2, 3, 4, 5]

last1 xs = xs !! (length xs - 1)
last2 xs = head (reverse xs)
begin1 xs = reverse (tail (reverse xs))
begin2 xs = reverse (drop 1 (reverse xs))
