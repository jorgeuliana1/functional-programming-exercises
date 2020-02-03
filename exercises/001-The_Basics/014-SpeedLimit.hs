-- Simulation of a speed radar

-- Our system has two cameras, in a straight line
-- The cameras are 25 meters apart
-- The logic is simple:
--  We identify a car in the first camera
--  We wait it to appear in the second camera
--  We measure the time (in seconds) that it took for it to reach the second camera

-- The road we are working at has a speed limit of 17 meters per second
-- The minumum speed allowed is 10 meters per second

speedRadar :: Float -> String
speedRadar t
    | speed < downLimit = "Too slow"
    | speed > upLimit = "Too fast"
    | otherwise = "Acceptable speed"
    where
        distance = 25
        speed = distance / t
        downLimit = 10 --m/s
        upLimit = 17 --m/s

main = print(speedRadar 20) -- 25m/20s = 5/4 m/s = 1.25 m/s, this is "Too slow"