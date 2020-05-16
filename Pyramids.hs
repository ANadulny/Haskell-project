{- SPOP. Projekt -}
module Pyramids where

-- utowrzenie typow danych dla wskazowek, tablicy z rozwiązaniem, pola na tablicy i zalozenia/wskazowki wprowadzanej przez uzytkownika
data Pyramids = Pyramids [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int] deriving Show
data Board = Board [[Int]] deriving Show
type Cell = (Int, Int)
type Assumption = (Char, (Int, Int))

-- utworzenie pustej tablicy
createEmptyBoard :: Int -> Board
createEmptyBoard size = Board $ replicate dim $ replicate dim $ 0
                      where dim = size

-- sprawdzenie wymiarow tablicy, ktore sa rowniez maksymalna wartoscia wysokosci jaka mozna wprowadzic do tablicy
getSize :: Board -> Int
getSize (Board rows)  = length $ rows !! 0

-- odpowiada za utworzenie pustej struktury do przechowywania zalozen i pobranie wskazowek podanych przez uzytkownika
inputAssumptionsList :: Int -> IO Pyramids
inputAssumptionsList size = do assumptionLists <- inputAssumptions (initialAssumptions size)
                               return assumptionLists

-- utworzenie pustych tablic zawierajacej wartosc Nothing
initialAssumptions :: Int -> Pyramids
initialAssumptions size = Pyramids list list list list -- top bottom left right - odpowiadaja w podanej kolejnosci kolejnym listom w ktorych trzymane sa zalozenia
                        where list = replicate size $ Nothing 

-- dodawanie nowych zalozen podanych przez uzytkownika
inputAssumptions :: Pyramids -> IO Pyramids
inputAssumptions pyramids = do 
    assumption <- newAssumption -- jesli uzytkownik wpisal znak c to przejdz do rozwiazania lamiglowki
    if fst assumption == 'c'
        then return pyramids
    else do
        putStrLn $ "Your input assumption: " ++ show (fst assumption) ++ ":" ++ show (snd assumption)
        result <- inputAssumptions $ insertNewPyramid pyramids (fst assumption) (snd assumption)
        return result

-- wczytanie nowego zalozenia od uzytkownika, gdzie podanie symbolu "c" oznacza rozpoczecie algorytmu rozwiazywania lamiglowki
newAssumption :: IO Assumption
newAssumption = do 
    decision <- getLine
    if decision == "c" 
       then return ('c',(0,0))
    else do
        index <- getLine -- index wybranej kolumny/wiersza, gdzie ma byc wprowadzona liczba piramid widzianych z danego miejsca. Moze przyjac wartosc od 0 do (maxWysokosc - 1).
        pyramidsNumber <- getLine -- liczba piramid jaka jest widoczna
        return (head decision, (read index, read pyramidsNumber))

-- w zaleznosci jakiego typu jest zalozenie/wskazowka wprowadzana przez uzytkownika to dodajemy ja do odpowiedniej tabeli
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

-- umieszczenie piramidy o danej wysokosci we wskazanej komorce tablicy
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

-- sprawdzenie czy wskazana komorka nalezy do tablicy
isOnBoard :: Int -> Cell -> Bool
isOnBoard size (x,y) = 
    if ((x < size && x >= 0) && (y < size && y >= 0)) then True
    else False

-- sprawdzenie wysokosci piramidy znajdujacej sie na wskazanej komorce
getCell :: Board -> Cell -> Int
getCell (Board rows) (x,y) = (rows !! x) !! y

-- pobranie wspolrzednych poprzedniej komorki
previousCell :: Board -> Cell -> Cell
previousCell board (0,0) = ((getSize board - 1), (getSize board - 1)) 
previousCell board (x,0) = (x-1, (getSize board) -1)
previousCell board (x,y) = (x, y-1)