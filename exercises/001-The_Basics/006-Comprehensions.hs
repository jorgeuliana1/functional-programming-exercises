-- A more mathematical approach
-- Just like in mathematics we just do that:
-- doubles_bellow10 = [x * 2 | x <- [0 .. 10]]
-- We can use multiple conditions
doubles_bellow10 = [x * 2 | x <- [0 .. 10], x * 2 < 10]
-- Now the doubles_bellow10 are really bellow 10
-- Yes, I know I could have done
-- doubles_bellow10 = [x * 2 | x <- [0 .. 5]]

-- We can do even better!
doubles_bellowN n = [x * 2 | x <- [0 .. n], x * 2 < n]

main = do print doubles_bellow10
          print (doubles_bellowN 10)
          -- Must be the same