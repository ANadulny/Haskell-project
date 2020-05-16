{- SPOP. Projekt -}
module Algorithm where
import Pyramids
import Data.List

-- funkcja rozwiązująca łamigłówkę (pobiera wskazówki i tablicę, na której mają zostac rozmieszczone piramidy)
-- rozpoczyna rozwiązanie próbując umieścić na polu (0,0) piramidę o max wysokości
solvePuzzle :: Pyramids -> Board -> Int-> Board
solvePuzzle pyramids board steps = let highestPyramid = getSize board in
                               transss(nextStep pyramids board highestPyramid (0,0) steps)

transss :: Board -> Board
transss (Board rows) = Board (transpose (rows))

-- -- funkcja wywoływana po umieszczeniu na tablicy piramidy
nextStep :: Pyramids -> Board -> Int -> Cell -> Int -> Board
nextStep pyramids board height cell tmp= 
    if (isOnBoard (getSize board) cell) then --sprawdza czy wypełniono cały wiersz
        pyramidOnCell pyramids board cell height tmp-- jeśli nie to próbuje wstawić piramidę na wskazanym polu
    else if (isLineOk pyramids board cell) then -- sprawdza, czy poprawnie wypełniono wiersz
        if (isNextRow cell) then
            nextStep pyramids board height (startNextRow cell) (tmp-1)
        else board
    else prevStep pyramids board prevHeight prevCell (tmp-1)-- jeśli niepoprawnie wypełniono wiersz to zmienia wartość wstawioną dla ostatniej komórki
        where
        isNextRow (x, _) = not(x == ((getSize board) - 1))
        startNextRow (x,y) = (x+1, 0)
        prevCell = previousCell board cell
        prevHeight = getCell board prevCell

-- -- funkcja próbująca umieścić piramidę na polu
-- -- zaczyna od umieszczenia piramidy o max wysokości, jeśli niemożliwe to w kolejnych krokach zmniejsza wysokość
pyramidOnCell :: Pyramids -> Board -> Cell -> Int -> Int -> Board
pyramidOnCell pyramids board (x,y) h tmp =
 if (tmp>0) then 
    if (h > 0) && (tmp>0) then
        if ((isUnique board (x,y) h) && (pyramidConstraint pyramids board (x,y) h)) then --sprawdza czy można umieścić piramidę
            nextStep pyramids (placePyramidOnBoard board (x,y) h) (getSize board) (x, y+1) (tmp-1)
        else
            pyramidOnCell pyramids board (x,y) (h-1) tmp
    else prevStep pyramids (placePyramidOnBoard board (x,y) 0) prevHeight prevCell (tmp-1)-- jeśli żadna wysokość nie pasuje na danej komórce, to umieszcza 0 i zmniejsza wartość poprzedniej komórki
 else board
 where 
        prevCell = previousCell board (x,y)
        prevHeight = getCell board prevCell
-- -- funkcja wywoływana, gdy trzeba było cofnąć się do poprzedniej komórki
prevStep :: Pyramids -> Board -> Int -> Cell -> Int ->Board
prevStep pyramids board height cell tmp = 
    if (height == 1) then -- jeśli była 1 to umieszcza 0 i probuje umiescic w poprzedniej komorce wartość o 1 mniejszą
        nextStep pyramids (placePyramidOnBoard board cell 0) (prevHeight-1) prevCell tmp
    else nextStep pyramids board (height-1) cell tmp -- jeśli była >1 to próbuje umieścić na tym polu wartość o 1 mniejszą
        where 
        prevCell = previousCell board cell
        prevHeight = getCell board prevCell

-- -- funkcja sprawdzająca, czy dana wartość występuje już w wierszu lub kolumnie, na skrzyżowaniu których leży komórka
isUnique :: Board -> Cell -> Int -> Bool
isUnique (Board rows) (x,y) h = let
    inRow = elem h (rows !! x)
    inCol = elem h $ (transpose rows) !! y in
    if (inRow || inCol) 
        then False
    else True
        
-- -- funkcja sprawdzająca, czy piramida o danej wysokości nie jest za wysoka by stanąć w danym miejscu
pyramidConstraint :: Pyramids -> Board -> Cell -> Int -> Bool
pyramidConstraint (Pyramids t b l r) board (x,y) h = (checkLeft && checkRight && checkTop && checkBottom)
    where 
    size = getSize board
    checkLeft = canBeVisible (l !! y) h x size
    checkRight = canBeVisible (r !! y) h (size - 1 - x) size
    checkTop = canBeVisible (t !! x) h (size - 1 - y) size
    checkBottom = canBeVisible (b !! x) h y size

canBeVisible :: Maybe Int -> Int -> Int -> Int -> Bool 
canBeVisible Nothing _ _ _ = True
canBeVisible (Just howMany) h place max = howMany <= (max - h + place + 1)


-- TODO - repair
--  isLineOk (Pyramids [Just 3,Nothing,Just 1,Nothing] [Nothing, Nothing, Nothing, Nothing] [Nothing, Nothing, Just 4, Nothing] [Nothing, Just 3, Nothing, Nothing]) (Board [[4,3,1,2],[0,0,0,0],[0,0,0,0],[0,0,0,0]]) (0,4)
-- -- funkcja sprawdzająca, czy uzupelniony cały wiersz jest zgodny ze wszystkimi ograniczeniami narzuconymi przez wskazówki
isLineOk :: Pyramids -> Board -> Cell -> Bool
isLineOk (Pyramids t b l r) (Board rows) (x,y) = fromTop && fromBottom && fromLeft  && fromRight 
    where
    fromTop = isColOk (t !! x) (countVisible 1 (reverse(rows !! x)))
    fromBottom = isColOk (b !! x) (countVisible 1 (rows !! x))
    fromLeft = isRowOk l (transpose rows)
    fromRight = isRowOk r (map reverse(transpose rows))
    
-- column -- sprawdzenie, czy w danym wierszu widać tyle piramid ile narzucają ograniczenia
isColOk :: Maybe Int -> Int -> Bool
isColOk Nothing _ = True
isColOk (Just constraint) actualNum = constraint == actualNum

-- -- sprawdzenie, czy w danej kolumnie możliwe jest spełnienie ograniczeń
-- co gdy mamy przypadek koncowy ???
isRowOk :: [Maybe Int] -> [[Int]] -> Bool
isRowOk [] _ = True
isRowOk (x:xs) rows | getPyramidNumber (x) == 0 = isRowOk xs rows
                    | ((countVisible 1 $ rows !! ( (getSize $ Board rows) - (length(x:xs)) )) <= (getPyramidNumber (x)) ) == True = isRowOk xs rows
                    | otherwise = False

getPyramidNumber :: Maybe Int -> Int
getPyramidNumber Nothing = 0
getPyramidNumber (Just constraint) = constraint

--     actualNums = map (countVisible 1) cols
--     checkCol :: Maybe Int -> Int -> Bool
--     checkCol Nothing _ = True
--     checkCol (Just constraint) actualNum = 
--         if (up) then constraint <= actualNum else constraint >= actualNum  -- jeśli liczymy od dołu to sprawdzamy, czy po uzupełnieniu pustych wartości rosnąco, nie widzimy za mało piramid
-- -- -- jeśli od góry to czy nie widzimy więcej piramid niż można w danej kolumnie

--  -- obliczenie ile piramid jest widocznych w danym wierszu / kolumnie
countVisible :: Int -> [Int] -> Int
countVisible i [x] = i
countVisible i (x:xs) 
    |x == 0     = countVisible i (fillAsc (xs) (length (0:xs))) -- jeśli na początku sprawdzanej listy są zera, to sa uzupełniane
    |x < head xs    = countVisible (i+1) xs
    |head xs == 0   = i+1 -- jeśli na końcu listy są zera, to zwraca wartość piramid widocznych do tego momentu
    |otherwise  = countVisible i ([x] ++ tail xs)

-- -- usupełnia zera na danej liście rosnąco, wartościami, które jeszcze w nim nie wystąpiły
fillAsc [x] n        = sort([1..n] \\ [x]) ++ [x]
fillAsc (x:xs) n
    |x == 0     = fillAsc xs n
    |otherwise  = sort([1..n] \\ (x:xs)) ++ (x:xs)