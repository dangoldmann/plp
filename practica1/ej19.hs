pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, b, c) | c <- [1..], a <-[1..c], b <- [1..c], a^2 + b^2 == c^2]