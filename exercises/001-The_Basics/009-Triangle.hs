-- Here we are going to use comprehensions and tuples to create a triangle
rightTriangles = [ (a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
main = print rightTriangles -- List of possible triangles