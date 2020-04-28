



-- Name: AllanLitva, stu#: 100943856 




-- calls countZerosHelper giving the appropriate value to the accumulator, aswell as converting negative values to positive 
countZeroFree :: [Int] -> Int 
countZeroFree x  = countZeros (convertToPositive x) 0 





-- walks through list, applying the checkZeros function to every element and storing the value in the accumulator. 
countZeros :: [Int] -> Int -> Int 
countZeros [] accum = accum 
countZeros (h:t) accum = countZeros t (accum + checkZeros h) 
    

	
	


-- done incase the single element 0 is in the list, my function would count it otherwise 
checkZeros :: Int -> Int 
checkZeros 0 = 0 
checkZeros x = checkZerosHelper x 




-- checks if input mod 10 == 0 ,if it is we return 0, meaning the input contains zeros.  otherwise it calls itself with (input 'div' 10), base case
-- is satisfied when the input is <= zero  
checkZerosHelper :: Int -> Int 
checkZerosHelper 0 = 1
checkZerosHelper x 
    | (modulus x 10 == 0) = 0 
	| otherwise = checkZerosHelper ( div x 10) 


	

	
-- my implimentation of modulus  	
modulus :: Int -> Int -> Int 
modulus x 0 = error "cant div by zero" 
modulus 0 x = 0 
modulus x y = x - y*(div x y) 





-- used to avoid problems using negative numbers, this works because i only care about the number of zeros, not the sign. 
convertToPositive :: [Int] -> [Int]
convertToPositive x = optimizedConvToPos x [] 


-- does the conversion to positive if needed, stores the result in the input param( ie tail call optimized) 	
optimizedConvToPos :: [Int]-> [Int] -> [Int] 
optimizedConvToPos [] accum = accum 
optimizedConvToPos (h:t) accum
    | h < 0 = optimizedConvToPos t ((-1)*h : accum) 
	| otherwise = optimizedConvToPos t ( h:accum) 






























