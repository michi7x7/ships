-- | Main entry point to the application.
module Main where

import Control.Monad.State
import Data.List (intersperse)

data Cell = Empty | Ship | Splash | Hit

type Field = [[Cell]]

data Gamestate = Gamestate {
    field :: Field
}

gameStart :: Gamestate
gameStart = Gamestate [ [Empty, Empty, Ship, Empty, Empty],
                    [Empty, Empty, Ship, Empty, Empty],
                    [Empty, Empty, Ship, Empty, Empty],
                    [Empty, Empty, Ship, Empty, Empty],
                    [Empty, Empty, Ship, Empty, Empty]]

symb c = case c of
    Empty  -> ' '
    Ship   -> '#'
    Splash -> '.'
    Hit    -> '*'

type Game a = StateT Gamestate a

io :: IO a -> Game IO a
io = liftIO

printField' :: Field -> IO()
printField' f = do
    let vs = length f
        hs = length (f!!0)

        vt = take vs ['A'..]
        ht = take hs ['1'..]

    putStrLn $ "  " ++ (intersperse ' ' vt)

    forM (zip [1..] f) $ \(n, r) -> do
        putStrLn $ (show n) ++ " " ++ (intersperse ' ' $ map symb r)

    return ()

printField :: Game IO ()
printField = gets field >>= io . printField'

game :: Game IO ()
game = do
    put gameStart

    printField


-- | The main entry point.
main :: IO ()
main = do
    runStateT game gameStart
    return ()
