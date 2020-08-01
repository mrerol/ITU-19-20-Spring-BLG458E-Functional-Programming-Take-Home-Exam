import Prelude hiding (Word)
-- Word is also in prelude, hence it is hided
import Data.Char
-- imported functions from Data.Char: toLower
import System.Environment
-- imported functions from System.Environment: getArgs
import Data.List 
-- imported functions from Data.List: sort, nub
import Data.Map hiding (map,foldr,filter)
-- imported functions from Data.Map: fromListWith, toList
-- to use prelude map, foldr and filter, (map,foldr,filter) in the Data.Map is hided


{-
    - Muhammed Raşit EROL
    - 150150023
    - 14/07/2020
    
    - Functional Programming Take Home Exam 
    - Part 3

    - Question: In this assignment, you will solve the combinatorial problem of finding all the anagrams of a sentence.
        An anagram of a word is a rearrangement of its letters such that a word with a different meaning is formed. For
        example, if we rearrange the letters of the word "Elvis" we can obtain the word "lives", which is one of its anagrams. In
        a similar way, an anagram of a sentence is a rearrangement of all the characters in the sentence such that a new
        sentence is formed. The new sentence consists of meaningful words, the number of which may or may not be the
        same as the number of words in the original sentence. For example, tan anagram of the sentence "I love you" is "You
        olive". In this exercise, we will consider permutations of words to be anagrams of the sentence. In the above example,
        "You I love" is considered to be a separate anagram. When producing anagrams we will ignore the character casing
        and the punctuation symbols.
        
        Your goal is to implement a program which takes a sentence and lists all its anagrams of that sentence. You will be
        given a dictionary that contains all meaningful words.
        
        The general idea is to examine the list of characters. To find the anagrams of a word, we will find all the words from the
        dictionary which have the same character list. To finding anagrams of a sentence, we will extract subsets of characters
        to see if we can form meaningful words. For the remaining characters we will solve the problem recursively and then
        combine the meaningful words we have found.
        
        For example, consider the sentence "You olive". The list of characters in this sentence is "eiloouvy". We start by
        selecting some subset of the characters, say "i". We are left with the characters "eloouvy". Checking the dictionary we
        see that "i" corresponds to the word "I", so we've found one meaningful word. We now solve the problem recursively
        for the rest of the characters "eloouvy". This will give us a list of solutions: [["love", "you"], ["you, "love"]]. We then
        combine "I" with that list to obtain the sentences "I love you" and "I you love", which are both valid anagrams.
        
        The types are as follows:
        * A "word" is a string that contains lowercase and uppercase characters, but no whitespace, punctuation or
        other special characters.
        * A "sentence" is a list of words.
        * A "character count" is a mapping from characters to integer numbers. It can be represented using a list of
        (Char,Int) pairs, or the Map type in Data.Map. Examine especially the fromList and fromListWith
        functions. Whether the order of the entries in the mapping is significant or not is up to your design.


    - Compiling: ghc part2.hs -o part3
    - Run: ./part3 "i love you"
    
-}

-- Defined types above are created here.
type Word = String
-- Ex.(Word) -> "love"

type Sentence = [Word]
-- Ex.(Sentence) -> ["I", "love", "you"]

type CharCount = [(Char, Int)]
-- Ex.(CharCount) -> [('e', 1), ('l', 1), ('o', 1), ('v', 1)]

type WordDictionary = [(Word, CharCount)]
-- Ex.(WordDictionary) -> [("love", [('e', 1), ('l', 1), ('o', 1), ('v', 1)])]

type CharCountDictionary = [(CharCount, Sentence)]
-- Ex.(CharCountDictionary) -> [([('a', 1), ('e', 1), ('t', 1)], ["eat", "tea"])]


{- 
    - Question 1: Write a function wordCharCounts that takes a word and returns its character counts. For example, if the
        word is "mississippi" the result should report that there are 1 of 'm', 4 of 'i', 4 of 's', and 2 of 'p'. Try to express
        this function as some combination of higher-order list functions like map and filter. Hint: You can use
        prelude functions like toLower or nub.

    - Answer: The code is given below.    

    - Function Parameters: wordCharCounts function takes word(w), and returns its CharCount. 

    - Standart Library Funtions:
        * zip: create tuple with given two list. Ex. zip "a" [1] -> [("a", 1)]
        * sort: sorts the list ascending order. sort "ba" -> "ab"
        * nub: prevent repetation on the list. Ex. "aab" -> "ab"
        * map: iterate on all elements of list and apply given function to the all elements. Ex. map (\x -> 1) xs -> all elements in the xs become 1
        * filter: filters the list with given function and returns a list. Ex. filter (>2) xs -> finds all elements bigger than 2 in the list xs
        * length: finds size of the list. Ex. lenght "aab" -> 3

    - Implementation: Detailed explanations are given below.
        * For explaining with example, it is considered as cs = "Lovee".
        * Firstly, all characters(cs) of word are converted to lower case chars with lower function.
            Because they will be used for finding repetation of characters, to do that exact match is necessary.
            Ex.: for cs = "Lovee", lower is "lovee".
        * repetition finds repeat count of characters in the given list and returns number of repetation for each char.
            Ex. for lower = "lovee", repetition "love" -> [1,1,1,2]
        * chars gives sorted un repetaed list for given list.
            Ex. for lower = "lovee", chars is "elov".

    - Example usage:
        wordCharCounts "love"   -> [('e',1),('l',1),('o',1),('v',1)]
        wordCharCounts "Lovee"  -> [('e',2),('l',1),('o',1),('v',1)]
        wordCharCounts ""       -> []
        wordCharCounts "aa"     -> [('a',2)]
-}
-- Using zip, tuple is created. 
-- First element of tuple is chars which are uniqe sorted characters of the word(cs)
-- Second element of tuple is number of these characters in the given word(cs)
-- Ex. for cs = "love",  zip "elov" [1,1,1,2] -> [('e',1),('l',1),('o',1),('v',1)]
wordCharCounts :: Word -> CharCount
wordCharCounts cs = zip chars (repetition chars)
    where
        -- chars creates a list without repetation and it is sorted after that
        chars = sort $ nub lower
        -- lower converts all characters of cs to lower case
        lower  = map toLower cs
        -- repetition finds number of characters in the word.
        -- (filter (==c) lower) finds chars which are equal to c in the list lower, this returns a list
        -- With length, the size of this list is found
        -- With map these sizes are placed on each character
        -- End of this opetation, each uniqe char is placed with char counts of that char.
        -- Example is igven above 
        repetition  = map (\c -> length (filter (==c) lower))


{- 
    - Question 2: Write a function sentenceCharCounts that takes a list and returns its character counts. Try to express it as
        a composition of functions.

    - Answer: The code is given below.    

    - Function Parameters: sentenceCharCounts function takes sentence(s), and returns its CharCount. 

    - Standart Library Funtions:
        * concat: accepts a list of lists and concatenates them. Ex. concat ["a", "b"] -> "ab"

    - Implementation: Detailed explanations are given below.
        * For explaining with example, it is considered as s = ["i", "love", "you"].
        * Here, function composition and curying are used.
        * Because of function composition, input of the sentenceCharCounts function is paseed to concat, firstly.
        * concat function returns concatenated elemets of input.
        * for s = ["i", "love", "you"], concat s -> "iloveyou"
        * This output is passed to wordCharCounts function as input.
        * wordCharCounts returns char counts of given sentence as a result.

    - Example usage:
        sentenceCharCounts ["i", "love", "you"] -> [('e',1),('i',1),('l',1),('o',2),('u',1),('v',1),('y',1)]
        sentenceCharCounts ["i"]                -> [('i',1)]
        sentenceCharCounts ["eat", "eat"]       -> [('a',2),('e',2),('t',2)]
        sentenceCharCounts []                   -> []
-}
sentenceCharCounts :: Sentence -> CharCount
sentenceCharCounts =  wordCharCounts . concat


{- 
    - Question 3: Write a function dictCharCounts that takes a list of dictionary words and returns a mapping from words to
        their character counts. For example, if the dictionary contains the words "eat", "all", and "tea", this should
        report that the word "eat" has 1 of 'e', 1 of 'a', 1 of 't'; the word "all" contains 1 of 'a', 2 of 'l'; the word "tea"
        contains 1 of 't', 1 of 'e', 1 of 'a'

    - Answer: The code is given below.    

    - Function Parameters: dictCharCounts function takes sentence(s), and returns its WordDictionary. 

    - Standart Library Funtions:
        * Used STL functions are explained above.

    - Implementation: Detailed explanations are given below.
        * For explaining with example, it is considered as s = ["i", "love"].
        * Here, it is needed key-value pair, which can be created with zip.
        * First element of tuple is word which is each element of sentence(s).
        * Second element of tuple is char count of the word, which can be created with wordCharCounts function

    - Example usage:
        dictCharCounts ["i", "love"]    -> [("i",[('i',1)]),("love",[('e',1),('l',1),('o',1),('v',1)])]
        dictCharCounts ["i"]            -> [("i",[('i',1)])]
        dictCharCounts ["eat", "eat"]   -> [("eat",[('a',1),('e',1),('t',1)]),("eat",[('a',1),('e',1),('t',1)])]
        dictCharCounts []               -> []
-}
-- (map wordCharCounts s) returns char counts of each word as list
-- (map wordCharCounts s) -> [[('i',1)],[('e',1),('l',1),('o',1),('v',1)]]
-- zip s xs creates word char count pair
-- input: s = ["i", "love"] 
-- zip s (map wordCharCounts s) -> [("i",[('i',1)]),("love",[('e',1),('l',1),('o',1),('v',1)])]
dictCharCounts :: Sentence -> WordDictionary
dictCharCounts s = zip s (map wordCharCounts s)


{- 
    - Question 4: Write a function dictWordsByCharCounts which takes in the result of the previous function and returns a
        map of words which have the same character counts. For the example above the result should map the
        character counts 1 of 'a', 2 of 'l' to the word list ["all"], and the character counts 1 of 'a', 1 of 'e', 1 of 't' to the
        word list ["tea", "eat"].

    - Answer: The code is given below.    

    - Function Parameters: dictWordsByCharCounts function takes WordDictionary(wd), and returns its CharCountDictionary. 

    - Standart Library Funtions:
        * fromListWith: Build a map from a list of key/value pairs with a combining function.
            Ex. fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
                fromListWith (++) [] == empty
        * toList: Convert to a list of key/value pairs
            Ex. toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
                toList empty == []
        * Other used STL functions are explained above.

    - Implementation: Detailed explanations are given below.
        * Here curying is used, to explain wihout curying it is considered input as wd = dictCharCounts $ ["eat", "tea", "love"].
        * wd is WordDictionary and it is ["i", "love"] -> [("i",[('i',1)]),("love",[('e',1),('l',1),('o',1),('v',1)])]
            for sentence ["i", "love"]. In short, dictCharCounts $ ["eat", "tea", "love"]
            call will be used to explain function.
        * map (\(w,cc) -> (cc, [w])) takes [("i",[('i',1)]),("love",[('e',1),('l',1),('o',1),('v',1)])] as input.
        * map (\(w,cc) -> (cc, [w])) function takes (w,cc) pairs and convert them to structure (cc, [w]). Here [w] is needed
            because all word with same charcount must be in the same list. In several step later, with (++) operator all words
            will be placed in one list. Without [w](just w), "ali" ++ "veli" -> "aliveli" which it is not wanted.
            Desired strucute is ["ali"] ++ ["veli"] -> ["ali", "veli"]
        * fromListWith (++) xs function crate a map from a list which has key-value pairs with combining function which is (++).
            Hence, same keys in the xs will be converted single key in the new list, and charcounts of them will be summed up.
        * fromListWith (++) xs function returns fromList type; hence, it is converted normal list with toList.

    - Example usage:
        dictWordsByCharCounts $ dictCharCounts $ ["eat", "tea", "love"] -> [([('a',1),('e',1),('t',1)],["tea","eat"]),([('e',1),('l',1),('o',1),('v',1)],["love"])] 
        dictWordsByCharCounts $ dictCharCounts $ ["i"]                  -> [([('i',1)],["i"])]
        dictWordsByCharCounts $ dictCharCounts $ ["eat", "eat"]         -> [([('a',1),('e',1),('t',1)],["eat","eat"])]
        dictWordsByCharCounts $ dictCharCounts $ []                     -> []
-}
-- There is curying here, but it is considered input wd = dictCharCounts $ ["eat", "tea", "love"] 
-- map (\(w,cc) -> (cc, [w])) wd returns [([('a',1),('e',1),('t',1)],["eat"]),([('a',1),('e',1),('t',1)],["tea"]),([('e',1),('l',1),('o',1),('v',1)],["love"])]
-- fromListWith (++) $ map (\(w,cc) -> (cc, [w])) wd returns fromList [([('a',1),('e',1),('t',1)],["tea","eat"]),([('e',1),('l',1),('o',1),('v',1)],["love"])]
-- toList $ fromListWith (++) $ map (\(w,cc) -> (cc, [w])) wd returns [([('a',1),('e',1),('t',1)],["tea","eat"]),([('e',1),('l',1),('o',1),('v',1)],["love"])] 
dictWordsByCharCounts :: WordDictionary -> CharCountDictionary
dictWordsByCharCounts = toList . fromListWith (++) . map (\(w,cc) -> (cc, [w]))


{- 
    - Question 5: . Write a function wordAnagrams which takes a word and the result of the previous function and returns a list
        of anagrams for the given word. For example, if the parameters are "ate" and the result given above, it
        should return the words "tea" and "eat".

    - Answer: The code is given below.    

    - Function Parameters: wordAnagrams function takes word(w) and CharCountDictionary(ccds), and returns a list
        of anagrams for the given word(w). 

    - Standart Library Funtions:
        * fst: returns first element of tuple
        * snd: returns second element of tuple
        * Other used STL functions are explained above.

    - Implementation: Detailed explanations are given below.
        * For explaining with example, it is considered as w = "eat" and ccds = dictWordsByCharCounts $ dictCharCounts $ ["eat", "tea", "love"].
            ccds = [([('a',1),('e',1),('t',1)],["tea","eat"]),([('e',1),('l',1),('o',1),('v',1)],["love"])] 
        * Firstly, char counts of word(w) is found with wordCharCounts w.
        * After that, using filter, in the ccds, all key-value pairs are found with given char count of word.
            These filtered elements have same char count with the given word.
        * After filtering, we have a list with key-value pairs; but, we need only words. 
            Hence, using map snd xs, we can obtain only words from filtered list(xs).
        * After map, we obtain list of list, we can concat these lists to one list.

    - Example usage:
        wordAnagrams "eat" $ dictWordsByCharCounts $ dictCharCounts $ ["eat", "tea", "love"] -> [([('a',1),('e',1),('t',1)],["tea","eat"]),([('e',1),('l',1),('o',1),('v',1)],["love"])] 
        wordAnagrams "eat" $ dictWordsByCharCounts $ dictCharCounts $ ["i"]                  -> [([('i',1)],["i"])]
        wordAnagrams "eat" $ dictWordsByCharCounts $ dictCharCounts $ ["eat", "eat"]         -> [([('a',1),('e',1),('t',1)],["eat","eat"])]
        wordAnagrams "eat" $ dictWordsByCharCounts $ dictCharCounts $ []                     -> []
-}
-- It is assumed that dictionary contains ["eat", "tea", "love"], so ccds = dictWordsByCharCounts $ dictCharCounts $ ["eat", "tea", "love"].
-- ccds = [([('a',1),('e',1),('t',1)],["tea","eat"]),([('e',1),('l',1),('o',1),('v',1)],["love"])] 
-- w = "eat"
-- (wordCharCounts w) -> [('a',1),('e',1),('t',1)]
-- filter (\x -> fst x == (wordCharCounts w)) ccds -> [([('a',1),('e',1),('t',1)],["tea","eat"])]
-- map snd (filter (\x -> fst x == (wordCharCounts w)) ccds) -> [["tea","eat"]]
-- concat $ map snd (filter (\x -> fst x == (wordCharCounts w)) ccds) -> ["tea","eat"]
wordAnagrams :: Word -> CharCountDictionary -> Sentence
wordAnagrams w ccds = concat $ map snd (filter (\x -> fst x == (wordCharCounts w)) ccds)


{- 
    - Question 6: Write a function charCountsSubsets that takes character counts and returns all possible subsets of these
        counts. For example, the character counts of the word "all" is 1 of 'a', 2 of 'l', and the subsets of the character
        counts should be (listed in no specific order):
        * empty
        * 1 of 'a'
        * 2 of 'l'
        * 1 of 'l'
        * 1 of 'a', 1 of 'l'
        * 1 of 'a', 2 of 'l'

    - Answer: The code is given below.    

    - Function Parameters: charCountsSubsets function takes CharCount(cc) [CharCount], 
        and returns powerset of given cc as [CharCount]

    - Standart Library Funtions:
        * replicate: creates a list of length given by the first argument and the items having value of the second argument
            Ex. Input: replicate 5 'a'
                Output: "aaaaa"
        * Other used STL functions are explained above.

    - Implementation: Detailed explanations are given below.
        * Here curying is used, to explain wihout curying it is considered input as cc = wordCharCounts "all"
        * For explaining with example, it is considered as cc = "all"
        * Firstly, given char count(cc) is converted to the string. 
            Ex.: ccToWord [("a", 1), ("l", 2)] -> "all"
        * After that, powerset function is called for "all", and it is returned powerset of the input.
            Ex.: powerset "all" -> ["all","al","al","a","ll","l","l",""] 
        * After powerset,  there are duplicate elements in the list. To avoid from that, nub is used.
            Ex. nub ["all","al","al","a","ll","l","l",""] -> ["all","al","a","ll","l",""]
        * With map wordCharCounts xs, char counts of all words in the xs is found. Here, xs is result of powerset "all",
            which is ["all","al","a","ll","l",""]
        * wordCharCounts iterates on powerset of "all" which is ["all","al","a","ll","l",""] and creates CharCount list
            with char-char count pair.

    - Used resources: for powerset implementation, i have inspired from following link:
        * https://stackoverflow.com/questions/32575630/powerset-of-a-set-with-list-comprehension-in-haskell

    - Example usage:
        charCountsSubsets $ wordCharCounts "eat" -> [[('a',1),('e',1),('t',1)],[('a',1),('e',1)],[('a',1),('t',1)],[('a',1)],[('e',1),('t',1)],[('e',1)],[('t',1)],[]]
        charCountsSubsets $ wordCharCounts "e"   -> [[('e',1)],[]]
        charCountsSubsets $ wordCharCounts "all" -> [[('a',1),('l',2)],[('a',1),('l',1)],[('a',1)],[('l',2)],[('l',1)],[]]
        charCountsSubsets $ wordCharCounts ""    -> [[]]
-}
-- curying is used in here, so it is considered input as cc = wordCharCounts "all"
-- cc = [('a',1),('l',2)]
-- ccToWord cc -> "all"
-- powerset $ ccToWord cc -> ["all","al","al","a","ll","l","l",""]
-- nub $ powerset $ ccToWord cc -> ["all","al","a","ll","l",""]
-- charCountsSubsets cc -> [[('a',1),('l',2)],[('a',1),('l',1)],[('a',1)],[('l',2)],[('l',1)],[]]
charCountsSubsets :: CharCount -> [CharCount]
charCountsSubsets = map wordCharCounts . nub . powerset  . ccToWord
    where
        powerset  :: Word -> [Word]
        -- Empty list is the end condition for recursion
        powerset  [] = [[]]
        -- powerset function iterates on string char by char
        -- It takes first character(c) and creates list with c and with powerset of the rest elements(cs) as [c:cs' | cs' <- powerset  cs]
        -- This operation is also applied other elements of the string(cs) with powerset cs
        -- And, returned result is appended overall list
        powerset  (c:cs) = ([c:cs' | cs' <- powerset  cs] ++ powerset cs)

        -- With (c,n) <- cc, all char count pairs are iterated
        -- In each iteration, using replicate n c, char(c) is multiplied with n, and new string is created
        -- This string is obtained with operation which is kind of reverse of finding char count
        -- For example [('a', 2)] -> "aa" is obtained
        -- At the last step, with concat single string is created with multiple strings which consist of 
        --  chars in number of character count. 
        ccToWord :: CharCount -> Word 
        ccToWord cc = concat [replicate n c | (c,n) <- cc]


{- 
    - Question 7: Write a function subtractCounts that takes two mappings of character counts and returns a new mapping
        where counts in the second map are subtracted from the counts in the first map. For example, if your first
        map is 1 of 'a', 3 of 'e', 2 of 'l', and your second map is 1 of 'a', 1 of 'l', the result should be 3 of 'e', 1 of 'l'. You
        can assume that the second map is a subset of the first map.

    - Answer: The code is given below.    

    - Function Parameters: subtractCounts function takes CharCount(cc1), CharCount(cc2) and returns 
        CharCount which is second map are subtracted from the counts in the first map.

    - Standart Library Funtions:
        * Used STL functions are explained above.

    - Implementation: Detailed explanations are given below.
        * For explaining with example, it is considered as cc1 = [('a', 1), ('e', 3), ('l', 2)]
            and cc2 = [('a', 1), ('l', 1)] and result should be [('e', 3), ('l', 1)]
        * Firstly, given char count(cc1) and cc2 is appended and one char count list is created with (cc2 ++ cc1)
        * Here, order is important, because, substraction is applied as cc1 - cc2. 
        * (cc2 ++ cc1) is resulted in [('a',1),('e',3),('l',2),('a',1),('l',1)]
        * After that, using fromListWith substraction opetation is performed on new created list which is (cc1 ++ cc2)
        * fromListWith returns fromList type, so, it is converted normal list with toList.
        * After that, we have list with also zero and negative char count values.
        * As a solution, filter is applied to avoid non-positive elements in the list. To do that, second element
            of tuple is filtered with snd x > 0.
        * It is assumed that second map is a subset of the first map.

    - Example usage:
        subtractCounts (wordCharCounts "eat") (wordCharCounts "ea") -> [('t',1)]
        subtractCounts (wordCharCounts "all") (wordCharCounts "ll") -> [('a',1)]
        subtractCounts (wordCharCounts "eat") (wordCharCounts "")   -> [('a',1),('e',1),('t',1)]
        subtractCounts (wordCharCounts "e") (wordCharCounts "ee")   -> [] -- This is violate our assumption
-}
-- cc1 = [('a', 1), ('e', 3), ('l', 2)]
-- cc2 = [('a', 1), ('l', 1)]
-- (cc2 ++ cc1) -> [('a',1),('e',3),('l',2),('a',1),('l',1)]
-- toList $ fromListWith (-) (cc2 ++ cc1) -> [('a',0),('e',3),('l',1)]
-- subtractCounts cc1 cc2 -> [('e',3),('l',1)]
subtractCounts :: CharCount -> CharCount -> CharCount
subtractCounts cc1 cc2 = filter (\x -> snd x > 0) $ toList $ fromListWith (-) (cc2 ++ cc1)


{- 
    - Question 8: Write a function sentenceAnagrams that takes a sentence and returns a list of sentences which are
        anagrams of the given sentence. Test your function on short sentences, no more than 10 characters. The
        search space gets huge very quickly as your sentence gets longer, so the program may run for a very long
        time. However, for short sentences such as "Linux rulez", "I love you", or "Mickey Mouse" the program should
        end fairly quickly.

    - Answer: The code is given below.    

    - Function Parameters: sentenceAnagrams function takes a word(w) as user input, sentence(dict) and returns 
        sentence anagrams of w. Here, I have changed function parameters, because, I want to keep this function as pure
        function. If I read dict in this function, sentenceAnagrams becomes unpure because of IO. Hence, I have just read
        dict in the main function and passed it here as dict. Also, user input is in the form of string, hence, Word type is used.

    - Standart Library Funtions: 
        * Used STL functions are explained above.

    - Implementation: Detailed explanations are given below.
        * For explaining with example, it is considered as w = "on" and dict is read from file.
        * Firstly, char count of sentence(w) is found with sentenceCC.
        * After that, subset of these char counts is found with sentenceSS.
        * with Map all subsets are iterated with function anagram'. So, with tihs way,
            it is possible to search all subsets of given sentence(w).
        * anagram' is a helper function and does actual work.
        * anagram' is called for each subset of given sentence(w).
        * If charcount and subset are eqaul which means given word is directly searched in the dic, then return the whole word
            if it is exist in the dic. For example, in dict , there exist "a" and user give the word as "a". Then a is returned.
            This is also end contion for recursion. It shows that there is no more subset to check.
        * If substraction of charcount and subset is not empty list, then main algorithm is worked.
        * subsetWord contains words which are found in the dict according to given subset. If subset is "on", then
            ["on", "no"] is returned.
        * Here, List Comprehension is used because it provides possiblity to work all possible combination of elements of lists.
            For example: [a ++ b | a <- ["a", "b"], b <- ["c", "d", "e"]] -> ["ac","ad","ae","bc","bd","be"]  
            Here, it can be seen that all possible combination are considered.  
        * Hence, subsetWord present all possible words with given subset.
        * In the beginning, we call anagram' with all subset of given word. So, all words which are derived from subsets are
            consired with subsetWord.
        * This words are appended with " " and subsetWord'.
        * subsetWord' presents rest of the anagrams of the subset which is subset' with rest of the char counts of rest subset.
        * subset' can be found with charCountsSubsets restCharCount. This also provides recursion, because rest of the subsets also
            are divided in here to subsets and anagram' is called with new subset(subset') and each char count of rest subset (restCharCount)
        * restCharCount present new subset with substraction of charcount and subset. charcount presents char couts of the given sentence(w).
            It is updated in each iteration but beginning it is used that way. With restCharCount, whole sentence(w) is divided parts and each
            part is again used with recursion.
        * Shorty, subsetWord, subset' and subsetWord' returns list. With list comprehension, all possibilites are considered with recursion.
        * Searchin in the dict is handled with wordAnagrams' which is implemented previous question. I have write it again, because
            this way it works much faster. Because all dict is not passed to this function, it is used locally.

    - Example usage: In order use this function, firstly, reading file must be performed. Do the followings:
        * file <- readFile "words.txt"
        * let dict = lines file
        * And call sentenceAnagrams with sentence and dict.
        * sentenceAnagrams "i love you" dict

        sentenceAnagrams "i love you" dict  -> [["olive you"],["Lev Io you","Lev you Io"],["Io Lev you","Io you Lev"],["you olive","you Lev Io","you Io Lev"]]
        sentenceAnagrams "eat" dict         -> ["tea","eat","ate"]
        sentenceAnagrams "on" dict          -> ["on","no"]
        sentenceAnagrams "a" dict           -> []
        sentenceAnagrams " " dict           -> [] 
-}
-- w = "on on"
-- dict = ["on","no", "eat", "tea", "love"]
-- sentenceCC -> [('n',2),('o',2)]
-- sentenceSS -> [[('n',2),('o',2)],[('n',2),('o',1)],[('n',2)],[('n',1),('o',2)],[('n',1),('o',1)],[('n',1)],[('o',2)],[('o',1)],[]]
-- allDictWordsByCharCounts = dictWordsByCharCounts $ dictCharCounts $ dict -> [([('a',1),('e',1),('t',1)],["tea","eat"]),([('e',1),('l',1),('o',1),('v',1)],["love"]),([('n',1),('o',1)],["no","on"])]
-- Here, for all sentenceSS anagram' is called with each subset and sentenceCC. Subset is different for each call,
--  but sentenceCC is same for all calls.
-- And with anagram' multiple recursion is applied and at the end the resut below is obtained.
-- map (\x -> anagram' x sentenceCC) sentenceSS retuns with some empty list like [["on","no"],[],[],[]], so concat is used
-- sentenceAnagrams w dict -> ["no no","no on","on no","on on"]
sentenceAnagrams :: Word -> Sentence -> Sentence
sentenceAnagrams w dict =  concat $ map (\x -> anagram' x sentenceCC) sentenceSS
    where
        -- sentenceCC presents all char counts of given sentence(w)
        sentenceCC = sentenceCharCounts $ map removePunctuation $ words w
        -- sentenceSS presents all subsets of given word(w)
        sentenceSS = charCountsSubsets sentenceCC
        -- allDictWordsByCharCounts presents words-char counts pairs in the dict
        allDictWordsByCharCounts = dictWordsByCharCounts $ dictCharCounts $ dict

        -- The main part of the sentenceAnagrams is this function.
        -- The detailed explanation is done above.
        -- Shortly, for each call, anagram' takes subset as char count. It is actualy one of the subsets of the sentence(w).
        -- And it takes charCount which is char count of the word. In each iteration, it is expired after some part of it used.
        -- for "on on", "on" is found and new charCount is determined with restCharCount. Amd new call for anagram' is made
        -- with new subsets which are subset' and new charCount which is restCharCount.
        anagram' :: CharCount -> CharCount -> Sentence
        anagram' subset charcount = if restCharCount == []
                                        -- this is end condition
                                        then wordAnagrams' charcount
                                            --  if word is found in the dict, then it is returned
                                            -- this is for one given word and corresponding word in exist in dict uniqely(without anagrams)
                                            else  [subsetWord ++ " " ++ subsetWord' | subsetWord  <- wordAnagrams' subset, subset' <- charCountsSubsets restCharCount, subsetWord' <- anagram' subset' restCharCount]
                                                                                      -- finds a word if it is in the dict
                                                                                      -- these are done for multiple words coming from wordAnagrams    
                                                                                      -- subset' <- charCountsSubsets restCharCount
                                                                                      -- new subset is created with new char counts
                                                                                      -- restCharCount is explained below and above with details.
                                                                                      -- subsetWord' <- anagram' subset' restCharCount
                                                                                      -- for rest of the sentence, anagram search continues
                                                                                      -- new anagram call is done with new subset and char counts
                                                                                      
            where
                -- Find new char counts of the word. In each iteration this is updated. 
                -- After some part of the char counts are used than it is recalculated.
                -- For example, partial anagram is found for given sentence(s), then restCharCount is updated 
                -- with substracting word char count with used word char count.
                -- This new char count is passed to again to anagram'
                restCharCount = subtractCounts charcount subset

        -- This function is same as the previosly implemented wordAnagrams function.
        -- It finds words of the given char count in the dict.
        -- It is implemented locally because, it is use dict and without passing dict like here is much faster than
        -- previous implementation
        wordAnagrams' :: CharCount -> Sentence
        wordAnagrams' cc = concat $ map snd (filter (\x -> fst x == cc) allDictWordsByCharCounts)



-- this is a helper function for removing punctions from the dictionary
-- it is added due to the test cases
removePunctuation :: String -> String
removePunctuation = filter (\x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))

{- 
    - Question 15: Write a main function that takes a short sentence as a command line parameter and prints its anagrams. The
        program should generate its dictionary by reading the given dictionary text file (extracted from the zip file).
    
    - Answers: The code is given below. I have inspired from http://learnyouahaskell.com/input-and-output when implementing 
        IO operations.

    - Standart Library Funtions: 
        * mapM_: it is like map but it applies sequence.
        * lines: creates an array of string from the original one, new line characters serving as separators
        * putStrLn: prints string to stdout with EOL.

    - Some Scenarios: after execution 
        * Run: ./part3 "i love you"
          Output:
            Io Lev you
            Io you Lev
            Lev Io you
            Lev you Io
            olive you
            you Io Lev
            you Lev Io
            you olive
-} 


main :: IO ()
main = do 
    -- reading sentence from user
    args <- getArgs
    -- reading file
    file <- readFile "words.txt"
    -- creating dict with seperating each line
    let dict = map removePunctuation $ lines file
    -- parsing sentence as user input
    let sentence = head args
    -- finding all anagrams of given sentence
    -- they are sorted for test
    let anagrams = sort $ sentenceAnagrams sentence dict
    -- printing all results
    --putStrLn $ foldr1 (\s1 s2 -> s1 ++ ", " ++ s2) anagrams
    -- mapM_ putStrLn anagrams
    let result = if anagrams == [] then "There is no anagram in the dictionary" else foldr1 (\s1 s2 -> s1 ++ "\n" ++ s2) anagrams
    putStrLn result

