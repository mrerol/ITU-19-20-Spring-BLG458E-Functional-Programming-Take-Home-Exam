import Data.Char
-- Data.Char is imported for digitToInt function

{-
    - Muhammed Raşit EROL
    - 150150023
    - 14/07/2020
    
    - Functional Programming Take Home Exam 
    - Part 2

    - Question: This problem involves a solitaire card game invented just for this exercise. 
        You will write a program that tracks the progress of a game.
        
        A game is played with a card-list and a goal. The player has a list of held-cards, initially empty. The player makes a
        move by either drawing, which means removing the first card in the card-list from the card-list and adding it to the
        held-cards, or discarding, which means choosing one of the held-cards to remove. The game ends either when the
        player chooses to make no more moves or when the sum of the values of the held-cards is greater than the goal.
        
        The objective is to end the game with a low score (0 is best). Scoring works as follows: Let sum be the sum of the
        values of the held-cards. If sum is greater than goal, the preliminary score is three times sum − goal, else the
        preliminary score is goal − sum. The score is the preliminary score unless all the held-cards are the same color, in which
        case the score is the preliminary score divided by 2 (and rounded down as usual with integer division).
        
        The types are defined as follows (you will probably want to derive from Eq and Show):

        data Color = Red | Black
        data Suit = Clubs | Diamonds | Hearts | Spades
        data Rank = Num Int | Jack | Queen | King | Ace
        data Card = Card { suit :: Suit, rank :: Rank }
        data Move = Draw | Discard Card

    - Compiling: ghc part2.hs -o part2
    - Run: ./part2
    
-}


-- The types are defined as follows like in the pdf file(also above):
data Color = Red | Black
             deriving (Eq, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Show)

data Rank = Num Int | Jack | Queen | King | Ace
            deriving (Eq, Show)

data Card = Card { suit :: Suit, rank :: Rank }
            deriving (Eq, Show)

data Move = Draw | Discard Card
            deriving (Eq, Show)

{- 
    - Question 7: Define a type for representing the state of the game. (What items make up the state of the game?)

    - Answer: State of the game is defined as follows. The current state consist of card list, 
        held card list and move list of the game. In each round state of the game is changed with
        corresponding changes in the card lists(held card list and card list) and move list. Game end condition must be
        checked with these lists, which reuqires extra checking operation. 
        The state itself does not contain game end flag, or something.
        However, this is a general way of presenting the state of the game in cs.        
-}
data State = State { moveList :: [Move], cardList :: [Card], heldCards :: [Card]}
            deriving (Eq, Show)


{- 
    - Question 1: Write a function cardColor, which takes a card and returns its color (spades and clubs are black, diamonds
        and hearts are red).

    - Answer: The code is given below.    

    - Function Parameters: cardColor function takes card(c), and returns its color. 

    - Standart Library Funtions:
        * There is no standart library function in this part.

    - Implementation: Necessary check for suit of card is applied and color of card is returned.

-}
-- spades and clubs are black, diamonds and hearts are red
-- hence, if suit of card is Spades of Clubs then return Black, otherwise it must be Red
cardColor :: Card -> Color
cardColor c = if suit c == Spades || suit c == Clubs then Black else Red


{- 
    - Question 2: Write a function cardValue, which takes a card and returns its value. 
        (numbered cards have their number as the value, aces are 11, everything else is 10).

    - Answer: The code is given below.    

    - Function Parameters: cardValue function takes card(c), and returns its value as described above.

    - Standart Library Funtions:
        * There is no standart library function in this part.

    - Implementation: Necessary check for rank of card is applied and value of card is returned.

-}
-- if card is Ace return 11
-- if card is numbered card then return its number
-- otherwise, for all conditions return 10
cardValue :: Card -> Int
cardValue c = case rank c of
                    Num x -> if x == 0 then error "rank is unknown" else x
                    Ace   -> 11
                    _     -> 10
                    --Jack    -> 10
                    --Queen   -> 10
                    --King    -> 10
              

{- 
    - Question 3: Write a function removeCard, which takes a list of cards cs, and a card c. 
        It returns a list that has all the elements of cs except c. 
        If c is in the list more than once, remove only the first one. 
        If c is not in the list, raise an error.


    - Answer: The code is given below.    

    - Function Parameters: removeCard function takes list of cards cs, and a card c, and returns updated card list.

    - Standart Library Funtions:
        * elem: checks that given element is in the given list. 

    - Implementation: Detailed explanations are given below.
        * Firsty, it is checked that is card list empty, then raise error because we cannot remove card from empty list
        * for other possibilities, if card(c) is a element of card list(cs) then myTakeWhile is called.
        * myTakeWhile is a helper function and it works like takeWhile function with small difference.
        * if card is not element of card list then raise error.
        * myTakeWhile takes a function and applies it to all elements on the parameter list(cs).
        * With this lamda function (\a -> a /= c), it returns a list that has all the elements of cs except c.
        * Also, prelude takeWhile function, cuts the list when it return false for given parameter function.
            However, myTakeWhile function returns rest of the list when lambda function returns false

    - Used resources: for myTakeWhile, it is used course slides:
        takeWhile :: (a -> Bool) -> [a] -> [a]
        takeWhile f [] = []
        takeWhile f (x:xs)
        | f x = x : takeWhile f xs
        | otherwise = []

-}                    
removeCard :: [Card] -> Card -> [Card]
-- empty card list is considered as error
removeCard [] _ = error "card not in list"
-- if card is a element of card list then call myTakeWhile else raise error
removeCard cs c = if elem c cs 
                    then myTakeWhile (\a -> a /= c) cs 
                        else error "card not in list"
    where
        myTakeWhile :: (a -> Bool) -> [a] -> [a]
        -- recursion end condition(pattern)
        myTakeWhile f [] = []
        -- list pattern is used and f is applied first element of list
        -- for the rest, myTakeWhile is called again
        -- in the end condition of recursion, myTakeWhile f cs returns [] and c:[] or c:[cs] (just example) operation 
        -- is performed hence, new list is generated with required condition
        myTakeWhile f (c:cs)
            -- if condition is true for lambda function, then card(c) is append the return value of myTakeWhile f cs
            | f c        = c : myTakeWhile f cs
            -- if condition is false for lambda function, then rest of the list is returned
            | otherwise  = cs


{- 
    - Question 4: Write a function allSameColor, which takes a list of cards and returns 
        True if all the cards in the list are the same color and False otherwise

    - Answer: The code is given below.    

    - Function Parameters: allSameColor function takes list of cards cs, and a card c, and 
        returns boolean values with condition of that is all cards have same color.

    - Standart Library Funtions:
        * There is no standart library function in this part.

    - Implementation: Detailed explanations are given below.
        * Firsty, empty card list condition is considered as True.
        * If there is one card in the list then function returns True. 
            This is a end condition for recursive function.
            It does not violate correct result of the program flow. 
            There is no empty card lsit call for recursive calls. 
            allSameColor [] pattern only possible when external call of the function.
        * For other cases, there can be at least two element in the list. 
            For each pair in the list is compared in terms of color 
            with cardColor function with order. 
        * In the case of exactly two card in the list, allSameColor (c2:cs) becomes 
            allSameColor (c2:[]) -> allSameColor [c2].
            This generate (cardColor c1 == cardColor c2) && True, which equals to (cardColor c1 == cardColor c2) 
            with some Boolean Algebra.
        * Hence, allSameColor completes required work succesfully.

-}    
allSameColor :: [Card] -> Bool
-- it is assumed that empty card list is return True for coloring
allSameColor []         = True
-- it is assumed if there is one card, then all cards have same color
-- it is also end contion because, each card is sent to allSameColor. 
-- At the end there will be one card and it should return True
-- which does not violate algorithm.
allSameColor [c]        = c == c -- instead of True it is used because some implementation bugs for test cases
-- for other patters, fisrt and second cards are sent to cardColor function and it is checked they have same color
-- it is applied and operation for returned results and 
-- allSameColor function is called again for second card and rest of the list
allSameColor (c1:c2:cs) = (cardColor c1 == cardColor c2) && allSameColor (c2:cs)


{- 
    - Question 5: Write a function sumCards, which takes a list of cards and returns the sum of their values. 
        Use a locally defined helper function that is tail .

    - Answer: The code is given below.    

    - Function Parameters: sumCards function takes list of cards cs, and returns the sum of their values as Int.

    - Standart Library Funtions:
        * There is no standart library function in this part.

    - Implementation: Detailed explanations are given below.
        * Firsty, empty card list condition is considered as zero as return value.
        * If card list is not empty, then tail recursive helper function sumCardsHelper is called.
        * In order to perform tail recursive structure, accumulator is used.
        * Zero is passed as acc value to helper function.
        * In the helper function empty, card list is end condition for recursion and acc value is returned as final result
        * In the case of not empty card list, card values + acc is passed as new accumulater of sumCardsHelper again 
            with the rest of the card list.

-}   
sumCards :: [Card] -> Int
-- Empty card list is considered as zero as return value
sumCards [] = 0
-- In case of not empty list sumCardsHelper is called with card list(cs) and acc variable as zero
sumCards cs = sumCardsHelper 0 cs
    where
        sumCardsHelper :: Int -> [Card] -> Int
        -- Recursion end condtion, acc is returned as function result
        sumCardsHelper acc []     = acc
        -- Card value of the head of the list is calculated and sum up with acc value which is initially zero
        -- In each iteration, new acc value is updated for called sumCardsHelper function. 
        -- At the end, it consist of all card values as summation
        sumCardsHelper acc (c:cs) = sumCardsHelper (acc + cardValue c) cs


{- 
    - Question 6: Write a function score, which takes a card list (the held-cards) and an int (the goal) 
        and computes the score as described above.

    - Answer: The code is given below.    

    - Function Parameters: score function takes list of cards(cs), goal(g) as Int and returns score as Int.

    - Standart Library Funtions:
        * There is no standart library function in this part.

    - Implementation: Detailed explanations are given below.
        * Firsty, in the case of sumCards cs > g, card colors of the list is checked
        * For both cases which all cards have same card or not, explained algorithm is applied
        * In the case of sumCards cs <= g, card colors of the list is checked
        * For both cases which all cards have same card or not, explained algorithm is applied
-}   
score :: [Card] -> Int -> Int
score cs g
    -- With guards, sum of the cards of cs is calculated and compared with the goal value
    -- Both cases are presented below.
    -- For both cases, condition of  all cards have same color or not, 
    -- different score calcutaion approach is used    
    | sumCards cs > g = if allSameColor cs 
                            then (3 * (sumCards cs - g)) `div` 2 
                                else 3 * (sumCards cs - g)
    | otherwise       = if allSameColor cs 
                            then (g - sumCards cs) `div` 2 
                                else (g - sumCards cs)


{- 
    - Question 8:  Write a function runGame that takes a card list (the card-list) a move list (what the player "does" at each
        point), and an int (the goal) and returns the score at the end of the game after processing (some or all of) the
        moves in the move list in order. Use a locally defined recursive helper function that takes the current state of
        the game as a parameter. As described above:
        * The game starts with the held-cards being the empty list.
        * The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
        * If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards not
            having c and the card-list unchanged. If c is not in the held-cards, raise an error.
        * If the player draws and the card-list is empty, the game is over. 
            Else if drawing causes the sum of the heldcards to exceed the goal, the game is over. 
            Else play continues with a larger held-cards and a smaller cardlist.

    - Answer: The code is given below.    

    - Function Parameters: runGame function takes list of cards(cs), move lsit(ms), goal(g) and returns score at end of the game as Int.

    - Standart Library Funtions:
        * null: checks that the list is empty

    - Implementation: Detailed explanations are given below.
        * There are three helper function in this part. They are explained in details below.
        * Firsty, initial state is created with given move list, card list and empty held card list.
        * It is passed as parameter to helper function playRound which does actual work
        * In the playRound function, if there is no move for current state, then score of the 
            current held cards(for current state) is calcluated and returned
        * If move list is not empty for current state, then head of the move list is checked.
        * Here, there are two option, which are Draw and Discard  
        * For Draw move, drawCard function is used, it takes current state and held card list with drawed card.
        * After Draw move, some of the game states are checked in this function.
        * If the game is not ended, then playRound function is called again with new state.
        * For Discard card move, playRound function is called with new state. 
            Here, new state is created with discardCard function.
        * discardCard function discard the card from held card list. This function might look unnecessary, 
            but to find which card is discarded, it is used.

-}   
runGame :: [Card] -> [Move] -> Int -> Int
-- Initial state is created as explained above
runGame cs ms g = playRound State {moveList=ms, cardList=cs, heldCards=[]}
    where 
        playRound :: State -> Int
        -- This function does main work, it takes current state and generate next state if game is not end
        playRound state
            -- If there is no move, then game is ended. The score of the game is calculated and returned.
            | null (moveList state) = score (heldCards state) g
            -- If game is not end, next move is checked.
            -- If next move is Draw, then drawCard is called, otherwise Discard c is applied to the current state.
            | otherwise             = if head (moveList state) == Draw 
                                        -- drawCard is called with held card list with drawed card
                                        then drawCard state (head(cardList state) : heldCards state) 
                                            -- Here, move is Discard card; 
                                            -- Hence, next state is created with discarded card in the held card list.
                                            -- Also, current move is expired and tail of that is sended to the next state
                                            else playRound State {moveList=tail (moveList state), cardList=cardList state, 
                                                                  heldCards=discardCard (head (moveList state)) (heldCards state)}

        drawCard :: State -> [Card] -> Int
        -- This function takes the current state and held card list with drawed card.
        drawCard state cs
            -- After draw, if card list of current state is empty, then game is over.
            -- The score of the game calculated and returned.
            | null (cardList state) = score (heldCards state) g
            -- If sum of the held cards is bigger than goal, then game is over. The score
            -- is calculated and returned.
            | sumCards cs > g       = score cs g
            -- Next state of the game is created and send to playRound function.
            -- New state consist of new move list which last move is expaired, new card list which one card is drawed from it.
            | otherwise             = playRound State {moveList=tail (moveList state), 
                                                       cardList=tail (cardList state), heldCards=cs}
        
        discardCard :: Move -> [Card] -> [Card]
        -- This function takes a move, held card list and return discarded held card list.
        -- The aim of the passing move is to extract of card which presented in the move as Discard c. 
        discardCard m cs = case m of
            Discard c -> removeCard cs c


{- 
    - Question 9:  Write a function convertSuit that takes a character c and returns the corresponding suit that starts with
        that letter. For example, if c is 'd' or 'D', it should return Diamonds. If the suit is unknown, 
        raise an errornd the card-list is empty, the game is over. Else if drawing causes the sum of the heldcards to exceed the goal, 
        the game is over. Else play continues with a larger held-cards and a smaller cardlist.

    - Answer: The code is given below.    

    - Function Parameters: convertSuit function takes a char(c), and returns the corresponding suit.

    - Standart Library Funtions:
        * elem: checks that given element is in the given list.

    - Implementation: Detailed explanations are given below.
        * Using elem function given char is search in the all possible strings using guards.
        * If elem returns True, then corresponding Suit is returned.
        * In case of undefined char, the function raise an error.

-}   
convertSuit :: Char -> Suit
-- guard structure is used and using elem, defined char is matched with defined corresponding Suit.
convertSuit c 
    | elem c "Cc" = Clubs
    | elem c "Dd" = Diamonds
    | elem c "Hh" = Hearts
    | elem c "Ss" = Spades
    | otherwise    = error "suit is unknown"


{- 
    - Question 10:  Write a function convertRank that takes a character c and returns the corresponding rank. For face cards,
        use the first letter, for “Ace” use '1' and for 10 use 't' (or 'T'). You can use the isDigit and digitToInt
        functions from the Data.Char module. If the rank is unknown, raise an error.

    - Answer: The code is given below.    

    - Function Parameters: convertRank function takes a char(c), and returns the corresponding rank.

    - Standart Library Funtions:
        * elem: Checks that given element is in the given list.
        * isDigit: True if the character is a decimal numeric character (0..9)
        * digitToInt: Convert between a single digit Char and the corresponding Int.

    - Implementation: Detailed explanations are given below.
        * Using elem function given char is search in the all possible strings using guards.
        * If elem returns True, then corresponding Rank is returned.
        * In case of 0-9 there are two option. One of them is Ace which is determined with digitToInt.
        * If it is not Ace, corresponding Num is returned with digitToInt function.
        * In case of undefined char, the function raise an error.

-}   
convertRank :: Char -> Rank
convertRank c
-- Case of King, Jack or Queen char is checked with corresponding letters, and corresponding Rank is returned
    | elem c "Jj" = Jack
    | elem c "Qq" = Queen
    | elem c "Kk" = King
    | elem c "Tt" = Num 10
    -- if c is digit then check is it Ace. If it is not, then return Num of card as Rank.
    | isDigit c   = if digitToInt c == 1 then Ace else Num (digitToInt c)
    -- if corresponding char is not found then raise an error
    | otherwise   = error "rank is unknown"


{- 
    - Question 11: Write a function convertCard that takes a suit name (char) and a rank name (char), and returns a card.

    - Answer: The code is given below.    

    - Function Parameters: convertCard function takes a suit(s) as char and rank(r) as char, and returns the corresponding card.

    - Standart Library Funtions:
        * There is no standart library function in this part.

    - Implementation: Detailed explanations are given below.
        * Using Card type required Card is created and returned after determining suit and rank of the card
            with following functions: convertSuit, convertRank

-}  
-- With the help of convertSuit and convertRank, suit and rank of the card is determined
-- and new card is cretated and returned
convertCard :: Char -> Char -> Card
convertCard s r = Card {suit=convertSuit s, rank=convertRank r}


{- 
    - Question 12: Write a function readCards that will read (and return) a list of cards from the user. On each line, the user
        will type a card as two letters (such as "hq" for "Queen of Hearts" or "s2" for "Two of Spades"). The user will
        end the sequence by typing a single dot.

    - Answer: The code is given below.    

    - Function Parameters: readCards function returns the card list after reading user inputs.

    - Standart Library Funtions:
        * getLine: using IO it reads a line from stdin
        * reverse: It reverses the list

    - Implementation: Detailed explanations are given below.
        * In this part, readCards' helper function is used.
        * Empty list is passed to this helper function.
        * The helper function reads card line by line until "." character.
        * If it reads "." then returns all read cards
        * If it does not read ".", then using convertCard new card is created and append to cs which holds previoes cards

-}  
readCards :: IO [Card]
-- readCards calls helper function readCards' with empty list.
-- Empty list will contain all cards in it.
readCards = readCards' []
    where
        readCards' :: [Card] -> IO [Card]
        -- readCards' is recursive function which runs until "." is read
        readCards' cs = do 
            -- line is read with getLine
            line <- getLine
            if line == "."
                -- in case of "." function returns current card list
                -- to get ordered card list reverse is used
                then return (reverse cs)
                    -- card is created and append to the previos card list
                    else readCards' (convertCard (line !! 0) (line !! 1):cs) 


{- 
    - Question 13: Write a function convertMove that takes a move name (char), a suit name (char), and a rank name (char) and
        returns a move. If the move name is 'd' or 'D' it should return Draw and ignore the other parameters. If the
        move is 'r' or 'R', it should return Discard c where c is the card created from the suit name and the rank
        name.

    - Answer: The code is given below.    

    - Function Parameters: convertMove function takes a move name (char), a suit name (char), and a rank name (char) and
        returns a move.

    - Standart Library Funtions:
        * elem: Checks that given element is in the given list.

    - Implementation: Detailed explanations are given below.
        * Using guards, move is determined with elem function.
        * If move is Discard, then Discard Card structure is created and returned.
        * To do that, convertSuit and convertRank functions are used.
        * If move is not one of the chars('D' or 'R'), then raise an error.

-}  
convertMove :: Char -> Char -> Char -> Move
convertMove m s r 
    -- If m is 'd', then Draw is returned
    | elem m "Dd" = Draw
    -- Case of Discard, using s and r, new card created and Move C is returned
    | elem m "Rr" = Discard Card {suit=convertSuit s, rank=convertRank r}
    -- If char is not found then raise an error
    | otherwise        = error "convertMove: Undefined Move!"


{- 
    - Question 14: Write a function readMoves that will read (and return) a list of moves from the user. On each line, the user
        will type a move as one or three letters (such as “d” for “Draw” or “rhq” for “Discard Queen of Hearts”). The
        user will end the sequence by typing a single dot.


    - Answer: The code is given below.    

    - Function Parameters: readMoves function returns the move list after reading user inputs.

    - Standart Library Funtions:
        * getLine: using IO it reads a line from stdin

    - Implementation: Detailed explanations are given below.
        * This implementation is similar to function readCards
        * Both functions same structure is used.

-}  
readMoves :: IO [Move]
-- readMoves calls helper function readMoves' with empty move list
-- After each iteration, it will contains moves after converting them to move 
readMoves = readMoves' []
    where
        readMoves' :: [Move] -> IO [Move]
        -- readMoves' is recursive function which runs until "." is read
        readMoves' cs = do 
            -- line is read with getLine
            line <- getLine
            if line == "."
                -- in case of "." function returns current move list
                -- move list is reversed, because now it is reversed, so without reverse, 
                -- it gives wrong list
                then return (reverse cs)
                    -- move is created and append to the previos move list
                    else readMoves' (convertMove (line !! 0) (line !! 1) (line !! 2):cs) 

{- 
    - Question 15: Bring all of it together so that the main function will work:
    
    - Answers: The code is given below.

    - Some Scenarios: after execution (./part2)
        Enter cards:
        c1
        s1
        c1
        s1
        .
        Enter moves:
        d
        d
        d
        d
        d
        .
        Enter goal:
        42
        Score: 3
        ---------- Another run ----------
        Enter cards:
        cj
        s8
        .
        Enter moves:
        d
        rhj
        .
        Enter goal:
        42
        part2: card not in list

-} 

main :: IO()
main = do 
    putStrLn "Enter cards:"
    cards <- readCards 
    --putStrLn (show cards)


    putStrLn "Enter moves:"
    moves <- readMoves
    -- putStrLn (show moves)

    putStrLn "Enter goal:"
    line <- getLine

    let goal = read line :: Int

    let score = runGame cards moves goal
    putStrLn ("Score: " ++ show score)
