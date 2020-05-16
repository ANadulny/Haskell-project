{- SPOP. Projekt -}
module Main where
import Pyramids
import Algorithm

{- główna funkcja: pobiera nazwę pliku z łamigłówką i wczytuje jego zawartość (wskazówki)
   wypełnia tablicę zerami i rozwiązuje łamigłówkę
   jeśli znajdzie rozwiązanie to je wyświetla -}

readInt :: IO Int
readInt = do
    int <- getLine
    return (read int)

puzzle :: IO ()
puzzle = do
    putStrLn "Welcome to the pyramid puzzle"
    putStrLn "Please choose puzzle size"
    size <- readInt

    putStrLn "\nPlease provide input assumptions\nFirstly choose one of the following char signs"
    putStrLn "t - top\tb - bottom\nl - left\tr - right"
    putStrLn "Than in the new lines write column/row index and proper number of pyramids"
    putStrLn "if you want to continue write 'c' sign"
    assumptions <- inputAssumptionsList size
    
    putStrLn $ "Assumptions lists: " ++ show assumptions
    putStrLn "\nPuzzle solving:"
    let board = createEmptyBoard size
    let solution = solvePuzzle assumptions board 60
    putStrLn $ show solution
    showSolutionResults solution
    putStrLn "\n\tPuzzle End"
main = puzzle

showSolutionResults :: Board -> IO ()
showSolutionResults (Board []) = do return ()
showSolutionResults (Board rows) = do 
    showSolutionResults $ Board $ tail rows
    putStrLn $ "\t" ++ show (head rows)
    
-- showSolutionResults(transss())