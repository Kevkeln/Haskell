module Date where

data Date = ISO Year Month Day
		  | US Month Day Year
		  | EU Day Month Year
		  deriving Show

data TimeComp = Years Int
			  | Months Int
			  | Days Int
			  deriving Show

type Year = Int 
type Month = Int
type Day = Int
type TimeDifference = [TimeComp]

maxMonths :: Int
maxMonths = 12

maxDays :: Month -> Int
maxDays 2 = 28
maxDays x | x < 1 || x > 12 = error "Month out of range"
		  | (odd x && x <= 7) || (even x && x >= 8) = 31
		  | otherwise = 30
		  
maxDays' :: Month -> Int
maxDays' 2 = 28
maxDays' x | (odd x && x <= 7) || (even x && x >= 8) = 31
		   | otherwise = 30
		  
toIso :: Date -> Date
toIso (EU d m y) = (ISO y m d)
toIso (US m d y) = (ISO y m d)
toIso (ISO y m d) = (ISO y m d)

fixDate :: Date -> Date
fixDate (ISO y m d) 
 | (d <= maxDays' m && d > 0 && m < 13 &&m > 0) = (ISO y m d)
 | m == 0 = fixDate((ISO (y-1) 12 d))
 | m > maxMonths && m < 24 = fixDate((ISO (y+1) (m-12) d))
 | m > maxMonths = fixDate (ISO (y+1) (m-12) d)
 | m < 0 && abs(m) < 12 = fixDate (ISO (y-1) (12+m) d)
 | m < 0 = fixDate (ISO (y-1) (12+m) d)
 | d == 0 = fixDate((ISO y (m-1) (maxDays' (m-1))))
 | d > 0 && d <= maxDays' m = fixDate (ISO y m d)
 | d > 0 = fixDate (ISO y (m+1) (d-maxDays' m))
 | d < 0 && abs(d) < maxDays' (m-1) = fixDate (ISO y (m-1) (maxDays' (m-1) +d))
 | d < 0 = fixDate (ISO y (m-1) (d + maxDays' (m-1)))
fixDate (EU d m y) = fixDate (toIso (EU d m y))
fixDate (US m d y) = fixDate (toIso (US m d y))								
{- (retarded shit ignore all of this) pointInTime :: Date -> [TimeComp] -> Date
pointInTime x [] = fixDate x
pointInTime x (t:ts) |  x == (ISO y m d) if t == (Day t) then pointInTime (ISO y m (d+t)) ts
                                         else if t == (Month t) then pointInTime (ISO y (m+t) d) ts
										 else if t == (Year t) then pointInTime (ISO (y+t) m d) ts
					 | otherwise = pointInTime (toIso x) (t:ts)					 


timeDiff :: Date -> Date -> [TimeComp]
timeDiff x1 x2 | (x1 == fixDate (ISO a b c)) && (x2 == fixDate (ISO d e f)) = (Day a-d) : (Month b-e) : (Year c-f) : []
			   | otherwise = timeDiff (fixDate x1) (fixDate x2) -}