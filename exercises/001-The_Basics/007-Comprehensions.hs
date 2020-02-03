-- We can do some more complex comprehensions
-- odd x returns True if x is odd
oddEven ns = [if odd x then "Odd!" else "Even!" | x <- ns]
main = print(oddEven [0 .. 10])