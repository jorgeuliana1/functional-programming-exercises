-- This is a Haskell comment

-- In Haskell the variables are assigned just like in Python
phrase = "Hello World!" -- Haskell has native support to strings
-- By the way, we can also use the 'let' keyword to define a name
-- 'phrase = "Hello World!"' and 'let phrase = "Hello World!"'
-- are the same

-- To print the phrase and to show some Haskell functionalities we are
-- going to create a function, our first one.
println x = putStrLn x
-- Basically we have just created a pretty name for an already existing
-- function, putStrLn.

-- Now, we just print the phrase
main = println phrase
-- This syntax is beautiful, note that we are defining the main function

-- About the function structure:
-- function_name function_parameter = function_body
-- We can have as many function parameters as we want

-- Note that we could have done this with only one line of code
-- putStrLn "Hello World!"