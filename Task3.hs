module Task3 (
    sumEqValues,
    sumValuesBetweenRange,
    sumEvenNumbersBetweenIndexRange,
    sumLessThatFirst,
    sumLessThatLast,
    removeNthValueFromFirstHalf,
    removeValuesFromFirstHalf,
    removeNthValueBetweenRange,
    removeValuesBetweenRange,
    removeNthValuesLessThatFirst,
    removeValuesLessThatFirst,
    removeNthValueLessThatLast,
    removeValuesLessThatLast,
    replaceNthValueFromFirstHalf,
    replaceValuesFromFirstHalf,
    replaceNthValueBetweenRange,
    replaceValuesBetweenRange,
    replaceNthValuesLessThatFirst,
    replaceValuesLessThatFirst,
    replaceNthValueLessThatLast,
    replaceValuesLessThatLast,
    countEqValuesFromFirstHalf,
    countValuesBetweenRange,
    countEvenNumberFromIndexRange,
    countElementLessThatFirst,
    countElementLessThatLast,
    isLenghEven,
    isMoreEvenNumbers,
    isSortedToHuge,
    isUniqList,
    splitList,
    takeMinList,
    deleteValuesInIndexesRange
) where
  
import Data.List(partition, sortBy)
  

  
listMiddle :: Foldable t => t a -> Int
listMiddle list = div (length list) 2
  
  
takeFirstHalf :: [a] -> [a]
takeFirstHalf list = take (listMiddle list) list


getListSplit :: Int -> Int -> [a] -> [a]
getListSplit lowerIndex upperIndex list =
    take (upperIndex - lowerIndex) (drop lowerIndex list)


isValueInRange :: Ord a => a -> a -> a -> Bool
isValueInRange lower upper value = value > lower && value < upper


-- Remove N-th items which satisfy the `fn` for each element in given list.
-- 0nth used for remove all the finds.
removeNth :: (Ord a1, Num a1) => a1 -> (a -> Bool) -> [a] -> [a]
removeNth _ _ [] = []
removeNth nth fn (x:xs) | nth <= 0 && fn x = removeNth nth fn xs
                        | nth == 1 && fn x = xs
                        | fn x             = x : removeNth (nth - 1) fn xs
                        | otherwise        = x : removeNth nth fn xs


-- Remove all finds in list.
removeAll :: (a -> Bool) -> [a] -> [a]
removeAll = removeNth 0


-- Replace N-th items which satisfy the `fn` for each element in given list.
-- 0nth used for replace all the finds.
replaceNth :: (Ord a1, Num a1) => a1 -> (a -> Bool) -> a -> [a] -> [a]
replaceNth _ _ _ [] = []
replaceNth nth fn newValue (x:xs)
    | nth <= 0 && fn x = newValue : replaceNth nth fn newValue xs
    | nth == 1 && fn x = newValue : xs
    | fn x             = x : replaceNth (nth - 1) fn newValue xs
    | otherwise        = x : replaceNth nth fn newValue xs


-- Replace all the finds.
replaceAll :: (a -> Bool) -> a -> [a] -> [a]
replaceAll = replaceNth 0


-- Return the number of values in the list that pass a truth test (predicate).
size :: (a -> Bool) -> [a] -> Int
size fn list = length (filter fn list)


-- Знайти суму елементів з вказаним значенням у першій половині списку.
sumEqValues :: (Num a, Eq a) => [a] -> a -> a
sumEqValues list value = sum (filter (== value) (takeFirstHalf list))
-- sumEqValues [1, 2 , 1, 1, 5] 1
-- sumEqValues [] 1
-- sumEqValues [2, 2, 2, 2] 1


-- Знайти суму елементів із значеннями з указаного діапазону
sumValuesBetweenRange :: (Num t, Ord t) => t -> t -> [t] -> t
sumValuesBetweenRange _ _ [] = 0
sumValuesBetweenRange lower upper list = sum (filter fn list)
    where
        fn x = x > lower && x < upper
-- sumValuesBetweenRange 2 5 [1, 2, 3, 4, 5, 6 ,7, 8]
-- sumValuesBetweenRange 0 5 [1, 2, 3, 4]
-- sumValuesBetweenRange 0 2 []


-- Знайти суму парних елементів з індексами з указаного діапазону
sumEvenNumbersBetweenIndexRange :: Integral a => Int -> Int -> [a] -> a
sumEvenNumbersBetweenIndexRange lower upper list =
    sum (filter even (getListSplit lower upper list))
-- sumEvenNumbersBetweenIndexRange 2 5 [1, 2, 3, 4, 5, 6 ,7, 8]
-- sumEvenNumbersBetweenIndexRange 0 4 [1, 2, 3, 4, 5, 6 ,7, 8]



-- Знайти суму елементів, значення яких менше голови списку.
sumLessThatFirst :: (Num a, Ord a) => [a] -> a
sumLessThatFirst list = sum (filter (< head list) list)
-- sumLessThatFirst [3, 2, 2, 4, 5]
-- sumLessThatFirst [3, 1]
-- sumLessThatFirst [3, 4]


-- Знайти суму елементів, значення яких менше останнього елемента списку.
sumLessThatLast :: (Num a, Ord a) => [a] -> a
sumLessThatLast list = sum (filter (< last list) list)
-- sumLessThatLast [3, 2, 2, 4, 5]
-- sumLessThatLast [3, 1]
-- sumLessThatLast [3, 4]


-- Видалити зі списку i-те входження вказаного значення
-- елемента з першої половини списку.
removeNthValueFromFirstHalf :: (Eq a, Ord a1, Num a1) => a1 -> a -> [a] -> [a]
removeNthValueFromFirstHalf nth value list =
    removeNth nth (== value) (takeFirstHalf list) ++ drop (listMiddle list) list
-- removeNthValueFromFirstHalf 2 1 [1, 2, 1, 3, 1, 4, 5, 6, 7, 1, 1]
-- removeNthValueFromFirstHalf 4 1 [1, 2, 1, 3, 1, 4, 5, 6, 7, 1, 1]
-- removeNthValueFromFirstHalf 1 1 [1]
-- removeNthValueFromFirstHalf 1 1 []
-- removeNthValueFromFirstHalf 1 1 [2, 4, 1]
-- removeNthValueFromFirstHalf 2 0 [0, 1, 0, 1, 1, 0]


-- Видалити зі списку всі входження вказаного значення
-- елемента з першої половини списку.
removeValuesFromFirstHalf :: Eq a => a -> [a] -> [a]
removeValuesFromFirstHalf value list =
    removeAll (== value) (takeFirstHalf list) ++ drop (listMiddle list) list
-- removeValuesFromFirstHalf 1 [1, 2, 1, 3, 1, 4, 5, 6, 7, 1, 1]
-- removeValuesFromFirstHalf 0 [1, 2, 1, 3, 1, 4, 5, 6, 7, 1, 1]
-- removeValuesFromFirstHalf 3 [1, 2, 1, 3, 1, 4, 5, 6, 7, 1, 1]


-- Видалити зі списку всі входження елемента з індексами з
-- указаного діапазону
deleteValuesInIndexesRange start end list = take start list ++ drop end list
-- deleteValuesInIndexesRange 2 5 [1, 2, 3, 4, 5, 6, 7, 8]



-- Видалити зі списку i-те входження елемента із значеннями з
-- указаного діапазону.
removeNthValueBetweenRange :: (Num a1, Ord a1, Ord a) => a1 -> a -> a -> [a] -> [a]
removeNthValueBetweenRange nth lower upper =
    removeNth nth (isValueInRange lower upper)
-- removeNthValueBetweenRange 2 5 10 [2, 4, 6, 8, 10, 12, 14, 16]
-- removeNthValueBetweenRange 1 1 20 [2, 4, 6, 8, 10, 12, 14, 16]
-- removeNthValueBetweenRange 1 1 2 [2, 4, 6, 8, 10, 12, 14, 16]


-- Видалити зі списку всі входження елемента із значеннями з
-- указаного діапазону.
removeValuesBetweenRange :: Ord a => a -> a -> [a] -> [a]
removeValuesBetweenRange lower upper = removeAll (isValueInRange lower upper)
-- removeValuesBetweenRange 5 10 [2, 4, 6, 8, 10, 12, 14, 16]
-- removeValuesBetweenRange 1 20 [2, 4, 6, 8, 10, 12, 14, 16]
-- removeValuesBetweenRange 1 2 [2, 4, 6, 8, 10, 12, 14, 16]


-- Видалити зі списку i-те входження елемента, значення
-- якого менше голови списку.
removeNthValuesLessThatFirst :: (Num a1, Ord a1, Ord a) => a1 -> [a] -> [a]
removeNthValuesLessThatFirst nth list = removeNth nth (< head list) list
-- removeNthValuesLessThatFirst 2 [3, 2, 1, 3, 4, 1, 1]
-- removeNthValuesLessThatFirst 1 [1, 2, 3]
-- removeNthValuesLessThatFirst 1 [1, 2, 3, 0]
-- removeNthValuesLessThatFirst 5 [1, 2, 3, 0]


-- Видалити зі списку всі входження елемента, значення
-- якого менше голови списку.
removeValuesLessThatFirst :: Ord a => [a] -> [a]
removeValuesLessThatFirst list = removeAll (< head list) list
-- removeValuesLessThatFirst [3, 2, 1, 3, 4, 1, 5]
-- removeValuesLessThatFirst [1, 2, 3]
-- removeValuesLessThatFirst [5, 4, 3, 2, 1]
-- removeValuesLessThatFirst []


-- Видалити зі списку i-те входження елемента, значення
-- якого менше останнього елемента списку.
removeNthValueLessThatLast :: (Num a1, Ord a1, Ord a) => a1 -> [a] -> [a]
removeNthValueLessThatLast nth list = removeNth nth (< last list) list
-- removeNthValueLessThatLast 2 [3, 2, 1, 3, 4, 1, 3]
-- removeNthValueLessThatLast 1 [1, 2, 3]
-- removeNthValueLessThatLast 1 [1, 2, 3, 0]
-- removeNthValueLessThatLast 5 [1, 2, 3, 0]


-- Видалити зі списку всі входження елемента, значення
-- якого менше останнього елемента списку.
removeValuesLessThatLast :: Ord a => [a] -> [a]
removeValuesLessThatLast list = removeAll (< last list) list
-- removeValuesLessThatLast [3, 2, 1, 3, 4, 1, 3]
-- removeValuesLessThatLast [1, 2, 3]
-- removeValuesLessThatLast [1, 2, 3, 0]


-- У списку замінити на вказане значення i-те входження
-- вказаного значення елемента з першої половини списку.
replaceNthValueFromFirstHalf :: (Eq a, Ord a1, Num a1) => a1 -> a -> a -> [a] -> [a]
replaceNthValueFromFirstHalf nth oldValue newValue list =
    replaceNth nth (== oldValue) newValue (takeFirstHalf list)
        ++ drop (listMiddle list) list
-- replaceNthValueFromFirstHalf 2 0 4 [1, 0, 0, 1, 0, 0]
-- replaceNthValueFromFirstHalf 3 0 4 [1, 0, 0, 1, 0, 0]


-- У списку замінити на вказане значення всі входження
-- вказаного значення елемента з першої половини списку.
replaceValuesFromFirstHalf :: Eq a => a -> a -> [a] -> [a]
replaceValuesFromFirstHalf oldValue newValue list =
    replaceAll (== oldValue) newValue (takeFirstHalf list)
        ++ drop (listMiddle list) list
-- replaceValuesFromFirstHalf 0 4 [1, 0, 0, 1, 0, 0]
-- replaceValuesFromFirstHalf 3 4 [1, 0, 0, 1, 0, 0]


-- У списку замінити на вказане значення i-те входження
-- елемента із значеннями з указаного діапазону
replaceNthValueBetweenRange
:: (Num a1, Ord a1, Ord a) => a1 -> a -> a -> a -> [a] -> [a]
replaceNthValueBetweenRange nth lower upper =
replaceNth nth (isValueInRange lower upper)
-- replaceNthValueBetweenRange 2 5 10 99 [2, 4, 6, 8, 10, 12, 14, 16]
-- replaceNthValueBetweenRange 1 1 20 99 [2, 4, 6, 8, 10, 12, 14, 16]
-- replaceNthValueBetweenRange 1 1 2 99 [2, 4, 6, 8, 10, 12, 14, 16]


-- У списку замінити на вказане значення всі входження
-- елемента із значеннями з указаного діапазону
replaceValuesBetweenRange lower upper = replaceAll (isValueInRange lower upper)
-- replaceValuesBetweenRange 2 5 99 [2, 4, 6, 8, 10, 12, 14, 16]
-- replaceValuesBetweenRange 1 20 99 [2, 4, 6, 8, 10, 12, 14, 16]
-- replaceValuesBetweenRange 1 2 99 [2, 4, 6, 8, 10, 12, 14, 16]


-- У списку замінити на вказане значення i-те входження
-- елемента, значення якого менше голови списку
replaceNthValuesLessThatFirst :: (Num a1, Ord a1, Ord a) => a1 -> a -> [a] -> [a]
replaceNthValuesLessThatFirst nth newValue list =
    replaceNth nth (< head list) newValue list
-- replaceNthValuesLessThatFirst 2 99 [3, 2, 1, 3, 4, 1, 1]
-- replaceNthValuesLessThatFirst 1 99 [1, 2, 3]
-- replaceNthValuesLessThatFirst 1 99 [1, 2, 3, 0]
-- replaceNthValuesLessThatFirst 5 99 [1, 2, 3, 0]


-- У списку замінити на вказане значення всі входження
-- елемента, значення якого менше голови списку
replaceValuesLessThatFirst :: Ord a => a -> [a] -> [a]
replaceValuesLessThatFirst newValue list = replaceAll (< head list) newValue list
-- replaceValuesLessThatFirst 99 [3, 2, 1, 3, 4, 1, 5]
-- replaceValuesLessThatFirst 99 [1, 2, 3]
-- replaceValuesLessThatFirst 99 [5, 4, 3, 2, 1]
-- replaceValuesLessThatFirst 99 []


-- У списку замінити на вказане значення i-те входження (всі входження)
-- елемента, значення якого менше останнього елемента списку
replaceNthValueLessThatLast :: (Num a1, Ord a1, Ord a) => a1 -> a -> [a] -> [a]
replaceNthValueLessThatLast nth newValue list = replaceNth nth (< last list) newValue list
-- replaceNthValueLessThatLast 2 99 [3, 2, 1, 3, 4, 1, 3]
-- replaceNthValueLessThatLast 1 99 [1, 2, 3]
-- replaceNthValueLessThatLast 1 99 [1, 2, 3, 0]
-- replaceNthValueLessThatLast 5 99 [1, 2, 3, 0]


-- У списку замінити на вказане значення i-те входження (всі входження)
-- елемента, значення якого менше останнього елемента списку
replaceValuesLessThatLast :: Ord a => a -> [a] -> [a]
replaceValuesLessThatLast newValue list = replaceAll (< last list) newValue list
-- replaceValuesLessThatLast 99 [3, 2, 1, 3, 4, 1, 3]
-- replaceValuesLessThatLast 99 [1, 2, 3]
-- replaceValuesLessThatLast 99 [1, 2, 3, 0]


-- Знайти кількість елементів з вказаним значенням у першій половині списку
countEqValuesFromFirstHalf :: Eq a => a -> [a] -> Int
countEqValuesFromFirstHalf value list = size (== value) (takeFirstHalf list)
-- countEqValuesFromFirstHalf 1 [1, 1, 2, 4, 1, 1, 5]
-- countEqValuesFromFirstHalf 2 [1, 1, 2, 4, 1, 1, 5]
-- countEqValuesFromFirstHalf 100 [1, 1, 2, 4, 1, 1, 5]
-- countEqValuesFromFirstHalf 1 []


-- Знайти кількість елементів із значеннями з указаного діапазону.
countValuesBetweenRange :: Ord a => a -> a -> [a] -> Int
countValuesBetweenRange lower upper = size (isValueInRange lower upper)
-- countValuesBetweenRange 2 3 [1, 2, 3, 4, 5, 6, 7, 8]
-- countValuesBetweenRange 0 100 [1, 2, 3, 4, 5, 6, 7, 8]


-- Знайти кількість парних елементів з індексами з указаного діапазону.
countEvenNumberFromIndexRange :: Integral a => Int -> Int -> [a] -> Int
countEvenNumberFromIndexRange lower upper list = size even (getListSplit lower upper list)
-- countEvenNumberFromIndexRange 2 8 [1, 2, 3, 4, 5,6,7,8,9,10]
-- countEvenNumberFromIndexRange 0 3 [1, 2, 3, 4, 5,6,7,8,9,10]
-- countEvenNumberFromIndexRange 2 8 []


-- Знайти кількість елементів, значення яких менше голови списку
countElementLessThatFirst :: Ord a => [a] -> Int
countElementLessThatFirst list = size (< head list) list
-- countElementLessThatFirst [4, 2, 1, 5, 2, 5]
-- countElementLessThatFirst [1, 2]
-- countElementLessThatFirst [2, 1]


-- Знайти кількість елементів, значення яких менше останнього елемента
-- списку.
countElementLessThatLast :: Ord a => [a] -> Int
countElementLessThatLast list = size (< last list) list
-- countElementLessThatLast [4, 2, 1, 5, 2, 3]
-- countElementLessThatLast [1, 2]
-- countElementLessThatLast [2, 1]


-- Чи є парною кількість елементів списку?
isLenghEven list = even ((foldr (\_ n -> 1 + n) 0) list)
-- isLenghEven [1, 2, 3, 4]
-- isLenghEven [1, 2, 3]
-- isLenghEven [1, 2]
-- isLenghEven [1]
-- isLenghEven []


-- У списку більше парних, чи непарних значень?
isMoreEvenNumbers :: Integral a => [a] -> String
isMoreEvenNumbers list  | fn list == 0 = "eq even & odd numbers"
                        | fn list > 0  = "even more"
                        | fn list < 0  = "odd more"
    where
        fn list = (size even list) - (size odd list)
-- isMoreEvenNumbers [1, 1, 1]
-- isMoreEvenNumbers [1, 1, 2]
-- isMoreEvenNumbers [1, 1, 2, 2]
-- isMoreEvenNumbers [1, 2, 2]
-- isMoreEvenNumbers [2, 2, 2]
-- isMoreEvenNumbers [1, 2]
-- isMoreEvenNumbers []


-- Чи впорядкований список за зростанням?
isSortedToHuge list = list == sortBy comparator list 
    where
        comparator x y  | x > y = GT
                    | x < y = LT
                    | x == y = GT
-- isSortedToHuge [1, 2, 3]
-- isSortedToHuge [1, 1, 1]
-- isSortedToHuge [3, 2, 1]
-- isSortedToHuge [1]
-- isSortedToHuge []


-- Чи є у списку дублікати?
isUniqList :: Eq a => [a] -> Bool
isUniqList list = all (\x -> size (== x) list <= 1) list
-- isUniqList [1, 2, 3]
-- isUniqList [1, 2, 1]


-- Розділити список на список від'ємних елементів та решту?
splitList :: (Ord a, Num a) => [a] -> ([a], [a])
splitList = partition (< 0)
-- splitList [1, 2, 3, -2, -5, 1, 0, -8, 1, 1]
-- splitList [1, 2, 3]
-- splitList [-1, -2]
-- splitList []
-- splitList [0]


-- Визначити менший з двох впорядкованих за зростанням списків. Меншим
-- вважаємо коротший список, всі елементи якого не перевершують відповідних
-- елементів іншого.
takeMinList :: Ord a => [a] -> [a] -> [a]
takeMinList first second | isFirstLess = first
                         | otherwise   = second
    where isFirstLess = all (== True) (zipWith (<=) first second)
-- takeMinList [1, 2, 3] [1, 2, 4]
-- takeMinList [1, 2, 4] [1, 2, 3]
-- takeMinList [1, 2, 3] [1, 1, 1]
-- takeMinList [1, 2, 3] [1, 2, 2, 3]
