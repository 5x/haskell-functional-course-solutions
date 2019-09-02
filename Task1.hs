module Task1 (
  max3,
  min3,
  sort2,
  bothTrue,
  solve2,
  isParallel,
  isIncluded,
  isRectangular,
  isTriangle,
  isSorted,
) where


-- Функція max3 – повертає найбільше із трьох чисел.
max3 x y = max (max x y)
-- max3 4 5 2
-- max3 4 4 4


-- Функція min3 – повертає найменше із трьох чисел.
min3 x y = min (min x y)
-- min3 4 5 2
-- min3 (-1) (-2) (-1)


-- Функція sort2 – з двох цілих повертає двійку (кортеж),
-- впорядковану за зростанням.
sort2 :: Ord t => t -> t -> (t, t)
sort2 x y | x > y     = (x, y)
          | otherwise = (y, x)
-- sort2 1 3
-- sort2 3 1
-- sort2 2 2


-- Функція bothTrue :: Bool -> Bool -> Bool – повертає True
-- тоді і лише тоді, коли обидва її аргументи дорівнюють
-- True. Не використовуйте стандартних логичічних операцій
-- (&&, || тощо).
bothTrue :: Bool -> Bool -> Bool
bothTrue True True = True
bothTrue _    _    = False


-- Функція solve2::Double->Double->(Bool,Double) – за двома
-- числами, коэффіцієнтами лінійного рівняння ax + b = 0,
-- повертає двійку (кортеж), перший елемент якої дорівнює True,
-- якщо рішення існує та False, якщо ні. Другий елемент або
-- дорівнює кореню рівняння, або 0.0.
solve2 :: Double -> Double -> (Bool, Double)
solve2 0 0 = (True, 0)
solve2 0 _ = (False, 0)
solve2 a b = (True, a / b)


-- Функція isParallel – повертає True, якщо два відрізка,
-- кінці яких задаются як аргументи функції, паралельні
-- (або належать одній прямій). Наприклад, значення
-- isParallel (1,1) (2,2) (2,0) (4,2) дорівнює True,
-- позаяк відрізки (1,1) − (2,2) та (2,0) − (4,2) паралельні.
isParallel a1 b1 a2 b2 = k1 == k2
 where
  k1 = k a1 b1
  k2 = k a2 b2
  k (aX, aY) (bX, bY) = (bX - aX) / (bY - aY)
-- isParallel (1,1) (2,2) (2,0) (4,2)
-- isParallel (1,1) (2,2) (2,0) (4,4)


-- Функція isIncluded – повертає True, якщо одне коло
-- повністю міститься у іншому. Аргументи функції – центри
-- та радіус кожного кола.
isIncluded :: (Ord b, Num b) => (b, b) -> b -> (b, b) -> b -> Bool
isIncluded (outerCenterX, outerCenterY) outerRadius (innerCenterX, innerCenterY) innerRadius
  = (maxXOuter >= maxXInner)
    && (maxYOuter >= maxYInner)
    && (minXOuter <= minXInner)
    && (minYOuter <= minYInner)
 where
  maxXOuter = outerCenterX + outerRadius
  minXOuter = outerCenterX - outerRadius
  maxYOuter = outerCenterY + outerRadius
  minYOuter = outerCenterY - outerRadius
  maxXInner = innerCenterX + innerRadius
  minXInner = innerCenterX - innerRadius
  maxYInner = innerCenterY + innerRadius
  minYInner = innerCenterY - innerRadius
-- isIncluded (1, 5) 100 (10, 5) 30
-- isIncluded (1, 5) 10 (10, 5) 30


-- Функція isRectangular – повертає True, якщо три точки на
-- площині є вершинами прямокутного трикутника. Координати
-- точок – аргументи функції.
isRectangular :: (Eq a, Floating a) => (a, a) -> (a, a) -> (a, a) -> Bool
isRectangular aXY bXY cXY = a * (b * b) == a * (c * c) + b * (c * c)
 where
  a = calcDistance aXY bXY
  b = calcDistance bXY cXY
  c = calcDistance aXY cXY
  calcDistance (x1, y1) (x2, y2) =
    sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))


-- Функція isTriangle – визначає, чи можна з відрізків
-- довжиною a, b та c побудувати трикутник.
isTriangle :: (Ord a, Num a) => a -> a -> a -> Bool
isTriangle a b c = (a + b > c) && (a + c > b) && (b + c > a)


-- Функція isSorted– визначає, чи впорядковані три
-- числа за зростанням.
isSorted :: Ord a => a -> a -> a -> Bool
isSorted a b c = a <= b && b <= c
-- isSorted 1 2 3
-- isSorted 1 1 1
-- isSorted 1 2 1
-- isSorted 3 2 1
