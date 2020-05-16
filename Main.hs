{- SPOP. Projekt -}
module Main where
import Pyramids
import Algorithm

readInt :: IO Int
readInt = do
    int <- getLine
    return (read int)

{- Glowna funkcja pobierajaca od uzytkownika dane dotyczace wielkosci tablicy oraz wskazowki wykorzystywane 
   do rozwiazania lamiglowki. Wskazowki pobierane sa tak dlugo az uzytkownik nie poda znaku "c".
   Nastepnie uruchamiany jest algorytm odpowiadajacy za rozwiazanie lamiglowki. Wynik wykonania
   algorytmu jest wyswietlany w konsoli gdzie w lewym dolnym rogu znajduje sie komorka (0,0). 
 -}
puzzle :: IO ()
puzzle = do
    putStrLn "Welcome to the pyramid puzzle"
    putStrLn "Please choose puzzle size"
    size <- readInt

    putStrLn "\nPlease provide input assumptions\nFirstly choose one of the following char signs"
    putStrLn "t - top\tb - bottom\nl - left\tr - right"
    putStrLn "Than in the new lines write column/row index and proper number of pyramids"
    putStrLn "Calculating the index relative to bottom left corner"
    putStrLn "if you want to continue write 'c' sign"
    assumptions <- inputAssumptionsList size
    
    putStrLn "\nPuzzle solving:"
    let board = createEmptyBoard size
    let solution = solvePuzzle assumptions board
    showSolutionResults solution
    putStrLn "\n\tPuzzle End"

-- funkcja wypisujaca wiersze w odpowiedniej kolejnosci
showSolutionResults :: Board -> IO ()
showSolutionResults (Board []) = do return ()
showSolutionResults (Board rows) = do 
    showSolutionResults $ Board $ tail rows
    putStrLn $ "\t" ++ show (head rows)

main = puzzle
