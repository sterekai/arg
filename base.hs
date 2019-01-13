module ARG.Base where
	decode :: Int -> [Int] -> Int
	decode base =
		foldl (\result digit -> base * result + digit) 0

	encode :: Int -> Int -> [Int]
	encode = process []
 
	process :: [Int] -> Int -> Int -> [Int]
	process digits _ 0 = digits
	process digits base number =
		process ((rem number base):digits) base (div number base)
