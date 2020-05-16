{- SPOP. Projekt -}
module Algorithm where
import Pyramids
import Data.List

-- funkcja rozwiazujaca lamiglowkę, ktora dostaje w parametrach wskazowki i tablice, na ktorej maja zostac rozmieszczone piramidy
-- rozpoczyna rozwiazanie probując umiescic na polu (0,0) piramide o max wysokosci
solvePuzzle :: Pyramids -> Board -> Board
solvePuzzle pyramids board = transposeBoard(nextStep pyramids board highestPyramid (0,0))
    where 
    highestPyramid = getSize board
    transposeBoard (Board rows) = Board (transpose (rows))

-- funkcja wywolywana po umieszczeniu na tablicy piramidy
nextStep :: Pyramids -> Board -> Int -> Cell -> Board
nextStep pyramids board height cell =
    if (isOnBoard (getSize board) cell) then --sprawdza czy wypelniono caly wiersz
        pyramidOnCell pyramids board cell height -- jesli nie to probuje wstawic piramide na wskazanym polu
    else if (isLineOk pyramids board cell) then -- sprawdza, czy poprawnie wypelniono wiersz
        if (isNextRow cell) then
            nextStep pyramids board height (startNextRow cell)
        else board
    else prevStep pyramids board prevHeight prevCell -- jesli niepoprawnie wypelniono wiersz to zmienia wartosc wstawiona dla ostatniej komorki
        where
        isNextRow (x, _) = not(x == ((getSize board) - 1))
        startNextRow (x,y) = (x+1, 0)
        prevCell = previousCell board cell
        prevHeight = getCell board prevCell

-- funkcja probujaca umiescic piramide na polu
-- zaczyna od umieszczenia piramidy o max wysokosci, jesli niemozliwe to w kolejnych krokach zmniejsza wysokosc
pyramidOnCell :: Pyramids -> Board -> Cell -> Int -> Board
pyramidOnCell pyramids board (x,y) h =
    if h > 0 then
        if ((isUnique board (x,y) h) && (pyramidConstraint pyramids board (x,y) h)) then --sprawdza czy mozna umiescic piramide
            nextStep pyramids (placePyramidOnBoard board (x,y) h) (getSize board) (x, y+1)
        else
            pyramidOnCell pyramids board (x,y) (h-1)
    else prevStep pyramids (placePyramidOnBoard board (x,y) 0) prevHeight prevCell -- jesli zadna wysokosc nie pasuje na danej komorce, to umieszcza 0 i zmniejsza wartosc poprzedniej komorki
    where
        prevCell = previousCell board (x,y)
        prevHeight = getCell board prevCell

-- funkcja wywolywana, gdy trzeba było cofnac się do poprzedniej komorki
prevStep :: Pyramids -> Board -> Int -> Cell -> Board
prevStep pyramids board height cell = 
    if (height == 1) then -- jesli wysokosc byla rowna 1 to umieszcza 0 i probuje umiescic w poprzedniej komorce wartosc o 1 mniejsza
        nextStep pyramids (placePyramidOnBoard board cell 0) (prevHeight-1) prevCell
    else nextStep pyramids board (height-1) cell -- jesli wysokosc >1 to probuje umiescic na tym polu wartosc o 1 mniejsza
        where 
        prevCell = previousCell board cell
        prevHeight = getCell board prevCell

-- funkcja sprawdzajaca, czy dana wartosc wystepuje już w wierszu lub kolumnie, na skrzyzowaniu ktorych lezy sprawdzana komorka
isUnique :: Board -> Cell -> Int -> Bool
isUnique (Board rows) (x,y) h | inRow || inCol = False
                              | otherwise = True
    where
    inRow = elem h (rows !! x)
    inCol = elem h $ (transpose rows) !! y

-- funkcja sprawdzajaca, czy piramida o danej wysokosci nie jest za wysoka by stanac w danym miejscu
pyramidConstraint :: Pyramids -> Board -> Cell -> Int -> Bool
pyramidConstraint (Pyramids t b l r) board (x,y) h = (checkLeft && checkRight && checkTop && checkBottom)
    where
    size = getSize board
    checkLeft = canBeVisible (l !! y) x
    checkRight = canBeVisible (r !! y) (size - 1 - x)
    checkTop = canBeVisible (t !! x) (size - 1 - y)
    checkBottom = canBeVisible (b !! x) y
    canBeVisible howMany place = (getPyramidNumber howMany) <= (size - h + place + 1)

-- funkcja sprawdzajaca, czy uzupelniony caly wiersz jest zgodny ze wszystkimi ograniczeniami narzuconymi przez wskazowki
isLineOk :: Pyramids -> Board -> Cell -> Bool
isLineOk (Pyramids t b l r) (Board rows) (x,y) = fromTop && fromBottom && fromLeft  && fromRight 
    where
    size = getSize $ Board rows
    isLastColumn = x == (size - 1)
    fromTop = isColOk (t !! x) (countVisiblePyramids (reverse(rows !! x)))
    fromBottom = isColOk (b !! x) (countVisiblePyramids (rows !! x))
    fromLeft = isRowOk l (transpose rows) isLastColumn
    fromRight = isRowOk r (map reverse(transpose rows)) isLastColumn
    
-- sprawdzenie, czy w danyej kolumnie widać tyle piramid ile narzucają ograniczenia
isColOk :: Maybe Int -> Int -> Bool
isColOk Nothing _ = True
isColOk (Just constraint) actualNum = constraint == actualNum

-- sprawdzenie, czy w danym wierszu mozliwe jest spelnienie ograniczen
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

-- wyciagniecie liczby z wartosci (Maybe Int)
getPyramidNumber :: Maybe Int -> Int
getPyramidNumber Nothing = 0
getPyramidNumber (Just constraint) = constraint

-- odpowiada za obliczenie ile piramid jest widocznych w danym wierszu lub kolumnie
countVisiblePyramids :: [Int] -> Int
countVisiblePyramids row = countVisible 0 row
    where 
    countVisible maxFound [] = 0
    countVisible maxFound (x:xs) | maxFound >= x = countVisible maxFound xs -- wystapienia kolejnych zer sa pomijane w tym warunkiem
                                 | otherwise = 1 + countVisible x xs