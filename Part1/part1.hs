{-
    - Muhammed Raşit EROL
    - 150150023
    - 14/07/2020
    
    - Functional Programming Take Home Exam 
    - Part 1

    - Question: Given that 1 Jan 1900 was a Monday, how many Sundays fell on the first of the month during the 20th century
        (1 Jan 1901 to 31 Dec 2000)?
        
        Two solutions will be considered for this problem. The solutions (along with their JavaScript implementations) are
        explained on the following page: https://www.xarg.org/puzzle/project-euler/problem-19/
        Python implementations for these solutions can be found under the same directory of this file.
            
    - Compiling: ghc part1.hs -o part1
    - Run:       ./part1
    - Inputs: 
        * sundays1tr 1901 2000
        * sundays1 1901 2000
        * sundays2 1901 2000

-}

{- 
    - Question 1: Write a function "dayOfWeek" that, given a year, a month and a day, returns which day of the week the date is. 
        Use Zeller's congruence.

    - Answer: The code is given below.    

    - Function Parameters: dayOfWeek function takes year(y), month(m) and day(y) and returns which day of the week the date is. 
        All types are Integer.

    - Standart Library Funtions:
        * floor: returns the greatest integer not greater than argument. Type: (RealFrac a, Integral b) => a -> b
        * fromIntegral: It converts an Int to a Double. Type: (Integral a, Num b) => a -> b 
            Ex: { 4 -> 4.0 }

    - Implementation: In the function flow, the python version is used.

-}
dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = (d + k + t1 + t2 + t3 + 5 * j) `mod` 7
-- Final result is calculated as above and partial calculations are done below.
    where 
        -- In the two lines below, updated month and year value is calculated when month <= 2. 
        m' = if m <= 2 then (m + 12) else m
        y' = if m <= 2 then (y - 1) else y

        -- In the below, partial calculations are handled. 
        -- Because of that (/) :: Fractional a => a -> a -> a, fromIntegral is used.
        j  = y' `div` 100
        k  = y' `mod` 100
        t1 = floor $ fromIntegral (13 * (m' + 1)) / 5.0
        t2 = floor $ fromIntegral k / 4.0
        t3 = floor $ fromIntegral j / 4.0


{- 
    - Question 2: 
        a) Fill in the Haskell code below to calculate the result.
        b) What does the helper function (sundays') calculate?
        c) What if you don't define a "rest" and use its expression where it's needed?
    
    - Answers:
        a) The code is given below.
        b) (sundays') function calculates number of sundays from the given start year to the given end year.
            It is a recursive function, and runs until end year. For each year, it runs also for each month.
            It calls dayOfWeek function for each month in given year interval. nextY is used for next year, nextM is used for next month. 
            Shortly, it returns number of sundays in the given inteval.
        c) rest is used only for better implementation. With this way, it is more readable. Also,
              its expression can be used where it is needed wihout seperating code. It works with both ways.

    - Function Parameters: sundays1 function takes start year(start), end year(end) and returns number of sundays. 
        All types are Integer.

    - Standart Library Funtions:
        * There is no standart library function in this part.

    - Implementation: In the function flow, the function in the pdf is used. Detailed explanations are given below.
        * sundays1 calls sundays' with start year and first month as 1
        * sundays' checks year value
            * if it is bigger than end year then returns zero
            * otherwise, calls dayOfWeek y m 1 and checks is it one
                * if it is then calls sundays' for next month and counts sundays
                * else, only calls sundays' for next month
        * In each month increment, also year is recalculated. For example if month is 12 then
            it shows that we are in the the next year. With this method, only incrementing month also provide
            us to increment years.

-}
sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1 
-- sundays1 calls helper function sundays' with start year and first month
    where
        -- sundays' takes year(y) and month(m) and returns number of sundays
        sundays' :: Integer -> Integer -> Integer
        sundays' y m
            -- Firstly, end condition is checked, if the year is bigger than the end year then return zero
            -- This is the end condition for recursive calls
            | y > end   = 0 
            -- Otherwise,
            -- dayOfWeek y m 1 is called and checked is it one
            -- if it is then calls rest + 1 else only rest
            | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
            where
                -- calculates next year with checking month value, modulo is used for getting cycles for months
                -- with 12 value, next year is presented
                nextY = if (m `mod` 12) == 0 then y + 1 else y
                -- calculates next month with checking month value, , modulo is used for getting cycles for months
                -- with 12 value, months return back to first month
                nextM = if (m `mod` 12) == 0 then 1 else m + 1
                -- calls sundays' for next iteration with new month and year value
                rest  = sundays' nextY nextM 


{- 
    - Question 3: Write a tail recursive function of "sundays1" and name it “sundays1tr”
    
    - Answers: The code is given below.

    - Function Parameters: sundays1tr function takes start year(start), end year(end) and returns number of sundays. 
        All types are Integer.

    - Standart Library Funtions:
        * There is no standart library function in this part.

    - Implementation: The implementation details are given in the sundays1 function.
        * Addition to the sundays1 function, accumulator variable is used to store number of sundays for each call.
        * It is passed until the recursion is ended. After that, it is returned as return value of the function.
        * With this method, tail recursive version of sundays1 is implemented. The rest part is same with sundays1.

-}
sundays1tr :: Integer -> Integer -> Integer
sundays1tr start end = sundays' start 1 0
-- different from sundays1, accumulator is sent to sundays'
    where
        sundays' :: Integer -> Integer -> Integer -> Integer
        -- different from previous sundays', accumulator(acc) is used
        sundays' y m acc
            -- as end condition accumulator is returned
            | y > end   = acc
            -- sundays' is called with acc values
            | otherwise = if dayOfWeek y m 1 == 1 then sundays' nextY nextM (acc + 1) else sundays' nextY nextM acc
            where
                nextY = if (m `mod` 12) == 0 then y + 1 else y
                nextM = if (m `mod` 12) == 0 then 1 else m + 1
                -- this part is explained above


{- 
    - Question 4: Write the "leap" and "daysInMonth" functions as given in the Python source. 
        Using these, implement "sundays2".
    
    - Answers: The code is given below.

    - Function Parameters: 
        * leap function takes year(y) and returns bool values for some conditions. 
        * daysInMonth function takes year(y), month(m) and returns days in the month. 
        * sundays2 function take start and end year(start, end) and returns number of sundays for given interval
    
    - Standart Library Funtions:
        * There is no standart library function in this part.

    - Implementation: In the function flow, the functions in the pdf are used. Detailed explanations are given below. 
        * leap function is used for determining leap year. It takes year and determine whether it is leap or not.
        * daysInMonth function calculates number of days in the given month for the given year. 
        * sundays2 function calls helper function sundays' with start year and first month as 1 and weekday as 2
        * sundays' does the main job which is calculating number of sundays for the given interval
        * sundays' is recursive function and it ends when year is greater than end year
        * otherwise, it checks that weekday is zero for modulo seven
            * when true, calls rest + 1 which is another recursive call for sundays' with new year, month and weekday values
            * when false only rest is called
        * It recursively runs for each month in the year interval.
        * weekday is calculated according to the python code and send to new call for sundays'.
    
-}
-- using some conditions given year is determined as leap year or not.
leap :: Integer -> Bool
leap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)


-- using guards corresponding month is checked and number of days in the month is returned.
-- For example, if months if February, it is checked is it leap year and it is returned 29 or 28 according to leap result.
daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y
    | m == 2                                = if leap y then 29 else 28
    | m == 4 || m == 6 || m == 9 || m == 11 = 30
    | otherwise                             = 31


sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' start 1 (dayOfWeek start 1 1)
-- sundays2 calls helper function sundays' with start year, first months as 1 and weekday as 2 like in the python code
    where
        sundays' :: Integer -> Integer -> Integer -> Integer
        -- sundays' takes year(y), month(m) and weekday(weekday) and returns
        sundays' y m weekday
            -- if year is bigger than end year then recursion is ended
            | y > end   = 0
            -- recursion part is presented
            | otherwise = if weekday `mod` 7 == 1 then rest + 1 else rest
            where
                nextY    = if (m `mod` 12) == 0 then y + 1 else y
                nextM    = if (m `mod` 12) == 0 then 1 else m + 1
                weekday' = weekday + ((daysInMonth m y) `mod` 7)
                rest     = sundays' nextY nextM weekday'
                -- the workflow is similar to previous part, so it is not neccessary to explain
                -- only weekday is new, but it is already is given with python code


{- 
    - Question 5: Bring all of it together so that the main function will work:
    
    - Answers: The code is given below.

    - Some Scenarios: after execution (./part1)
        * Input : sundays1tr 1901 2000
          Output: 171

        * Input : sundays1 1901 2000
          Output: 171

        * Input : sundays2 1901 2000
          Output: 171

-}
-- returns corresponding function for user input
getFunction :: String -> (Integer -> Integer -> Integer)
getFunction name
    | name == "sundays1"   = sundays1
    | name == "sundays1tr" = sundays1tr
    | name == "sundays2"   = sundays2
    | otherwise            = error "unknown function"


main :: IO ()
main = do
    -- reads user input
    line <- getLine
    -- parses function, start and end year
    let [f, start, end] = words line
    -- calls correponding function with given start and end year values
    -- prints result after converting the resut to string
    putStrLn $ show $ (getFunction f) (read start :: Integer) (read end :: Integer)


{- 
    - Question 8: Is the number of weeks in 400 years an integer value? In other words, is the number of days
        in 400 years a multiple of 7? If so, what is the possibility that a certain day of a month (such as 1 Jan, or your
        birthday) is a Sunday (or some other day)? Are all days equally possible?
    
    - Answers: 
            Lets answer first part of the question that is the number of weeks in 400 years an integer value.
        We have to consider leap years. In 100 years, there are (100 / 4 = 25 - 1 = 24) leap years, here -1
        comes from 100 is not leap year. Hence, for 400 years there are 24 * 4 = 96 + 1 = 97 years, here +1
        comes from 400 is a leap year. Hence, for every 400 years there are 400*365 => 146,000 + 97 = 146097 days.
        Here, 97 days come from each leap year present additional day. Hence, every 400 years contains 146097 days, which is 
        divisible by 7 that shows that number of weeks in 400 years is integer value(20.871).
            
            The second part of the question which is what is the possibility that a certain day of a month (such as 1 Jan, or your
        birthday) is a Sunday (or some other day) and are all days equally possible, are solved as follows.
        Since 400 years is not divisible by 7, it is not possible to get same frequency for diffetent days of the week for certain day of week.
        https://cs.uwaterloo.ca/~alopez-o/math-faq/node73.html shows that freqency of some 400 years period of Jan1.
        It can be seen in here, probability that an arbitraryJanuary 1st is a Sunday is (58/400) but the same probability for 
        Monday is (56/400). They are not equally possible.
        
        https://cs.uwaterloo.ca/~alopez-o/math-faq/node73.html:
                   Sun      Mon     Tue     Wed     Thu     Fri     Sat
            Jan 1: 58       56      58      57      57      58      56
            Mar 1: 58       56      58      56      58      57      57

-}
