{- SPOP. Projekt -}
module Algorithm where
import Pyramids
import Data.List

-- funkcja rozwiązująca łamigłówkę (pobiera wskazówki i tablicę, na której mają zostac rozmieszczone piramidy)
-- rozpoczyna rozwiązanie próbując umieścić na polu (0,0) piramidę o max wysokości
solvePuzzle :: Pyramids -> Board -> Board
solvePuzzle pyramids board = let highestPyramid = getSize board in
                               transss(nextStep pyramids board highestPyramid (0,0))

transss :: Board -> Board
transss (Board rows) = Board (transpose (rows))

-- funkcja wywoływana po umieszczeniu na tablicy piramidy
nextStep :: Pyramids -> Board -> Int -> Cell -> Board
nextStep pyramids board height cell = 
    if (isOnBoard (getSize board) cell) then --sprawdza czy wypełniono cały wiersz
        pyramidOnCell pyramids board cell height -- jeśli nie to próbuje wstawić piramidę na wskazanym polu
    else if (isLineOk pyramids board cell) then -- sprawdza, czy poprawnie wypełniono wiersz
        if (isNextRow cell) then
            nextStep pyramids board height (startNextRow cell)
        else board
    else prevStep pyramids board prevHeight prevCell -- jeśli niepoprawnie wypełniono wiersz to zmienia wartość wstawioną dla ostatniej komórki
        where
        isNextRow (x, _) = not(x == ((getSize board) - 1))
        startNextRow (x,y) = (x+1, 0)
        prevCell = previousCell board cell
        prevHeight = getCell board prevCell

-- funkcja próbująca umieścić piramidę na polu
-- zaczyna od umieszczenia piramidy o max wysokości, jeśli niemożliwe to w kolejnych krokach zmniejsza wysokość
pyramidOnCell :: Pyramids -> Board -> Cell -> Int -> Board
pyramidOnCell pyramids board (x,y) h =
    if h > 0 then
        if ((isUnique board (x,y) h) && (pyramidConstraint pyramids board (x,y) h)) then --sprawdza czy można umieścić piramidę
            nextStep pyramids (placePyramidOnBoard board (x,y) h) (getSize board) (x, y+1)
        else
            pyramidOnCell pyramids board (x,y) (h-1)
    else prevStep pyramids (placePyramidOnBoard board (x,y) 0) prevHeight prevCell -- jeśli żadna wysokość nie pasuje na danej komórce, to umieszcza 0 i zmniejsza wartość poprzedniej komórki
    where 
        prevCell = previousCell board (x,y)
        prevHeight = getCell board prevCell

-- funkcja wywoływana, gdy trzeba było cofnąć się do poprzedniej komórki
prevStep :: Pyramids -> Board -> Int -> Cell ->Board
prevStep pyramids board height cell = 
    if (height == 1) then -- jeśli była 1 to umieszcza 0 i probuje umiescic w poprzedniej komorce wartość o 1 mniejszą
        nextStep pyramids (placePyramidOnBoard board cell 0) (prevHeight-1) prevCell
    else nextStep pyramids board (height-1) cell -- jeśli była >1 to próbuje umieścić na tym polu wartość o 1 mniejszą
        where 
        prevCell = previousCell board cell
        prevHeight = getCell board prevCell

-- funkcja sprawdzająca, czy dana wartość występuje już w wierszu lub kolumnie, na skrzyżowaniu których leży komórka
isUnique :: Board -> Cell -> Int -> Bool
isUnique (Board rows) (x,y) h = let
    inRow = elem h (rows !! x)
    inCol = elem h $ (transpose rows) !! y in
    if (inRow || inCol) 
        then False
    else True
        
-- funkcja sprawdzająca, czy piramida o danej wysokości nie jest za wysoka by stanąć w danym miejscu
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

-- funkcja sprawdzająca, czy uzupelniony cały wiersz jest zgodny ze wszystkimi ograniczeniami narzuconymi przez wskazówki
isLineOk :: Pyramids -> Board -> Cell -> Bool
isLineOk (Pyramids t b l r) (Board rows) (x,y) = fromTop && fromBottom && fromLeft  && fromRight 
    where
    fromTop = isColOk (t !! x) (countVisiblePyramids (reverse(rows !! x)))
    fromBottom = isColOk (b !! x) (countVisiblePyramids (rows !! x))
    fromLeft = isRowOk l (transpose rows) (x == ((getSize $ Board rows)-1))
    fromRight = isRowOk r (map reverse(transpose rows)) (x == ((getSize $ Board rows)-1))
    
-- sprawdzenie, czy w danyej kolumnie widać tyle piramid ile narzucają ograniczenia
isColOk :: Maybe Int -> Int -> Bool
isColOk Nothing _ = True
isColOk (Just constraint) actualNum = constraint == actualNum

-- sprawdzenie, czy w danym wierszu możliwe jest spełnienie ograniczeń
isRowOk :: [Maybe Int] -> [[Int]] -> Bool -> Bool
isRowOk [] _ _ = True
isRowOk (x:xs) rows isLastCol | isNotNothing && (isTooManyPyramids || isNotEnoughPyramidsInLastColumn) = False
                              | otherwise = isRowOk xs rows isLastCol
                              where 
                                isNotNothing = x /= Nothing
                                size = getSize $ Board rows
                                row = rows !! (size - length (x:xs))
                                conditionPyramidNumber = getPyramidNumber (x)
                                visiblePyramidsNumber = countVisiblePyramids row
                                isTooManyPyramids = visiblePyramidsNumber > conditionPyramidNumber
                                isNotEnoughPyramidsInLastColumn = isLastCol && (visiblePyramidsNumber /= conditionPyramidNumber)

-- wyciągnięcie liczby z wartości (Maybe Int)
getPyramidNumber :: Maybe Int -> Int
getPyramidNumber Nothing = 0
getPyramidNumber (Just constraint) = constraint

-- odpowiada za obliczenie ile piramid jest widocznych w danym wierszu lub kolumnie
countVisiblePyramids :: [Int] -> Int
countVisiblePyramids row = countVisible 0 row
                        where countVisible maxFound [] = 0
                              countVisible maxFound (x:xs) | maxFound >= x = countVisible maxFound xs -- wystapienia kolejnych zer sa pomijane w tym warunkiem
                                                           | otherwise = 1 + countVisible x xs