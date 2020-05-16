{- SPOP. Projekt -}
module Pyramids where
-- import Data.List

-- utowrzenie typów danych dla wskazówek, tablicy z rozwiązaniem i pola na tablicy
data Board = Board [[Int]] deriving Show
data Pyramids = Pyramids [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int] deriving Show
type Cell = (Int, Int)
type Assumption = (Char, (Int, Int))

-- utworzenie pustej tablicy
createEmptyBoard :: Int -> Board
createEmptyBoard size = Board $ replicate dim $ replicate dim $ 0
                      where dim = size

-- -- sprawdzenie wymiarów tablicy (jest to jednocześnie max wysokość piramidy)
getSize :: Board -> Int
getSize (Board rows)  = length $ rows !! 0

inputAssumptionsList :: Int -> IO Pyramids
inputAssumptionsList size = do result <- inputAssumptions (initialAssumptions size)
                               return result

initialAssumptions :: Int -> Pyramids
initialAssumptions size = Pyramids list list list list -- top bottom left right assumption list
                        where list = replicate size $ Nothing 

-- sprawdzenie czy jest nowe zalozenie podane przez uzytkownika
inputAssumptions :: Pyramids -> IO Pyramids
inputAssumptions pyramids = do 
    assumption <- newAssumption
    if fst assumption == 'c'
        then return pyramids
    else do
        putStrLn $ "Your input assumption: " ++ show (fst assumption) ++ ":" ++ show (snd assumption)
        result <- inputAssumptions $ insertNewPyramid pyramids (fst assumption) (snd assumption)
        return result

-- wczytanie nowego zalozenia od uzytkownika
newAssumption :: IO Assumption
newAssumption = do 
    decision <- getLine
    if decision == "c" 
       then return ('c',(0,0))
    else do
        index <- getLine
        pyramidsNumber <- getLine
        return (head decision, (read index, read pyramidsNumber))

insertNewPyramid :: Pyramids -> Char -> (Int, Int) -> Pyramids
insertNewPyramid (Pyramids top bottom left right) 't' (position, value) = Pyramids (replace top (position, (Just value))) bottom left right
insertNewPyramid (Pyramids top bottom left right) 'b' (position, value) = Pyramids top (replace bottom (position, (Just value))) left right
insertNewPyramid (Pyramids top bottom left right) 'l' (position, value) = Pyramids top bottom (replace left (position, (Just value))) right
insertNewPyramid (Pyramids top bottom left right) 'r' (position, value) = Pyramids top bottom left (replace right (position, (Just value)))

replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
    if n < 0
        then (x:xs)
    else x: replace xs (n-1,a)

-- umieszczenie piramidy o danej wysokości we wskazanej komórce tablicy
placePyramidOnBoard :: Board -> Cell -> Int -> Board
placePyramidOnBoard (Board oldRows) (x,y) num = newBoard where
    divBoard = splitAt x oldRows
    oldRows1 = fst divBoard
    oldRows2 = tail (snd divBoard)
    row = head (snd divBoard)
    divRow = splitAt y row
    oldCol1 = fst divRow
    oldCol2 = tail (snd divRow)
    newRow = oldCol1 ++ [num] ++ oldCol2
    newBoard = Board (oldRows1 ++ [newRow] ++ oldRows2)

-- -- sprawdzenie czy wskazana komórka należy do tablicy
isOnBoard :: Int -> Cell -> Bool
isOnBoard size (x,y) = 
    if ((x < size && x >= 0) && (y < size && y >= 0)) then True
    else False

-- -- sprawdzenie wysokości piramidy znajdującej się na wskazanej komórce
getCell :: Board -> Cell -> Int
getCell (Board rows) (x,y) = (rows !! x) !! y

-- -- pobranie współrzędnych poprzedniej komórki
previousCell :: Board -> Cell -> Cell
previousCell board (0,0) = ((getSize board - 1), (getSize board - 1)) 
previousCell board (x,0) = (x-1, (getSize board) -1)
previousCell board (x,y) = (x, y-1)