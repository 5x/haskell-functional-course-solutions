# Haskell functional course slutions

Solutions to course "Functional programming in Haskell"


## Some examples

Remove N-th items which satisfy the `fn` for each element in given list.
0nth used for remove all the finds.

```Haskell
removeNth :: (Ord a1, Num a1) => a1 -> (a -> Bool) -> [a] -> [a]
removeNth _ _ [] = []
removeNth nth fn (x:xs) | nth <= 0 && fn x = removeNth nth fn xs
                        | nth == 1 && fn x = xs
                        | fn x             = x : removeNth (nth - 1) fn xs
                        | otherwise        = x : removeNth nth fn xs
```


Знайти кількість елементів, значення яких менше значення наступного елемента.

```Haskell
countLessThatNext :: (Num t, Ord a) => [a] -> t
countLessThatNext []  = 0
countLessThatNext [_] = 0
countLessThatNext (prev:next:xs)
  | prev < next = 1 + countLessThatNext (next : xs)
  | otherwise   = countLessThatNext (next : xs)
```


Функція `isRectangular` – повертає `True`, якщо три точки на
площині є вершинами прямокутного трикутника. Координати
точок – аргументи функції.

```Haskell
isRectangular :: (Eq a, Floating a) => (a, a) -> (a, a) -> (a, a) -> Bool
isRectangular aXY bXY cXY = a * (b * b) == a * (c * c) + b * (c * c)
 where
  a = calcDistance aXY bXY
  b = calcDistance bXY cXY
  c = calcDistance aXY cXY
  calcDistance (x1, y1) (x2, y2) =
    sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
```


Функція `isParallel` – повертає `True`, якщо два відрізка,
кінці яких задаются як аргументи функції, паралельні
(або належать одній прямій). Наприклад, значення
`isParallel (1,1) (2,2) (2,0) (4,2)` дорівнює `True`,
позаяк відрізки `(1,1) − (2,2)` та `(2,0) − (4,2)` паралельні.

```Haskell
isParallel a1 b1 a2 b2 = k1 == k2
 where
  k1 = k a1 b1
  k2 = k a2 b2
  k (aX, aY) (bX, bY) = (bX - aX) / (bY - aY)

-- isParallel (1,1) (2,2) (2,0) (4,2)
-- isParallel (1,1) (2,2) (2,0) (4,4)
```


Чи є у списку дублікати?

```Haskell
isUniqList :: Eq a => [a] -> Bool
isUniqList list = all (\x -> size (== x) list <= 1) list

-- isUniqList [1, 2, 3]
-- isUniqList [1, 2, 1]
```


## License

The code is available under the [MIT license](LICENSE).
