-- We can simply obtain the lowercase alphabet like this
low_case_alpha = ['a' .. 'z'] -- That's how we define a range
-- We can also do it for uppercase
upp_case_alpha = ['A' .. 'Z']
-- We can do a numeric range too
nums = [0 .. 9]
-- Finally we can merge them like this:
-- alphanumeric_chars = low_case_alpha ++ upp_case_alpha
-- We can't merve [0 .. 9], lists are homogeneous
-- We could have used only one line:
alphanumeric_chars = ['a' .. 'z'] ++ ['A' .. 'Z']

-- And we print it
main = do print alphanumeric_chars
          print nums