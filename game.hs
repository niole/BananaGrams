module Game where
import System.Random
import Trie

fullBag = Pile "aaaaaaaabbbcccddddddeeeeeeeeeeeeeeeeeefffgggghhhiiiiiiiiiiiijjkklllllmmmnnnnnnnnooooooooooopppqqrrrrrrrrrsssssstttttttttuuuuuuvvvwwwxxyyyzz"

randomlyPick :: Int -> Game -> Player -> IO Game
randomlyPick n (Game pile _ rules) player
        | n == 0 = return $ Game pile [player] rules
        | otherwise = randomChar >>=
          (\(chr, restOfPile) ->
            randomlyPick (n - 1) (Game (Pile restOfPile) [] rules) $ Player (Hand (chr:hand)) board)
                      where (Pile letters) = pile
                            randomChar = getRandomChar letters
                            (Player (Hand hand) board) = player

getRandomChar :: String -> IO (Char, String)
getRandomChar s = let charIndex = getStdRandom (randomR (0,(length s) - 1))
                   in (\index -> (last $ take (index + 1) s, (take index s) ++ (drop (index + 1) s))) <$> charIndex

dealHand :: Game -> Player -> IO Game
dealHand game player = (\new -> Game (pile new) ((head $ players new) : players game) $ rules game) <$> newGame
                                 where totalTiles = splitNumber $ rules game
                                       newGame = randomlyPick totalTiles game player

doPeel :: Game -> Player -> IO Game
doPeel game player = (\new -> Game (pile new) ((head $ players new) : players game) $ rules game) <$> newGame
                                         where totalTiles = peelNumber $ rules game
                                               newGame = randomlyPick totalTiles game player

gameWithoutPlayers :: Game -> Game
gameWithoutPlayers (Game pile _ rules) = Game pile [] rules

-- plays a word according to what's possible to play on the board
playWord :: Player -> Trie -> String
playWord player dictionary = "sdf"

peel :: Game -> IO Game
peel game = foldl (\g -> \p -> g >>= (\i -> doPeel i p)) (return $ gameWithoutPlayers game) $ players game

split :: Game -> IO Game
split game = foldl (\g -> \p -> g >>= (\i -> dealHand i p)) (return $ gameWithoutPlayers game) $ players game

data Pile = Pile [Char]
instance Show Pile where
 show (Pile cs) = "Pile(" ++ (show cs) ++ ")"

data Rules = Rules { peelNumber :: Int, splitNumber :: Int }
instance Show Rules where
 show rules = "Rules(" ++ (show $ splitNumber rules) ++ (show $ peelNumber rules) ++ ")"

data Tile = Tile (Maybe Char)
instance Show Tile where
 show (Tile Nothing) = "EmptyTile"
 show (Tile (Just c)) = "Tile(" ++ (show c) ++ ")"

data Hand = Hand [Char]
instance Show Hand where
 show (Hand letters) = "Hand(" ++ (show letters) ++ ")"

data Board = Board [[Tile]]
instance Show Board where
 show (Board tiles) = "Board(" ++ (show tiles) ++ ")"

data Player = Player Hand Board
instance Show Player where
 show (Player hand board) = "Player(" ++ (show hand) ++ (show board) ++ ")"

data Game = Game { pile :: Pile, players :: [Player], rules :: Rules }
instance Show Game where
 show game = "Game(" ++ (show $ pile game) ++ (show $ players game) ++ (show $ rules game) ++ ")"

makeDictionary :: IO()
makeDictionary = do
                content <- readFile "./engmix.txt"
                print $ Trie.makeTrie $ lines content
