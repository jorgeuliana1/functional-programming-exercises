-- We are going to recreate 019-Filter.hs using lambda function
xs = [1, 2, 3, 4, 5, 8, 1, 8, 2, 8, 3, 8]
xf = filter (\ x -> x == 8) xs
main = print xf -- This is much better!
-- output: [8,8,8,8]