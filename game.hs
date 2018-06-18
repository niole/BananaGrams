module Game where
import Data.List
import System.Random
import Trie

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

fullBag :: Pile
fullBag = Pile "aaaaaaaabbbcccddddddeeeeeeeeeeeeeeeeeefffgggghhhiiiiiiiiiiiijjkklllllmmmnnnnnnnnooooooooooopppqqrrrrrrrrrsssssstttttttttuuuuuuvvvwwwxxyyyzz"

emptyHand :: Hand
emptyHand = Hand []

emptyBoard :: Board
emptyBoard = Board [[]]

shuffle :: [t] -> IO [t]
shuffle [] = return []
shuffle ts = index >>= (\i -> conc (ts !! i) $ shuffle $ (take i ts) ++ (drop (i + 1) ts))
            where index = getRandomNumber 0 $ (length ts) - 1
                  conc h t = (\ts -> h:ts) <$> t

makeDictionary :: IO Trie
makeDictionary = do
                content <- readFile "./engmix.txt"
                return $ Trie.makeTrie $ lines content

randomlyPick :: Int -> Game -> Player -> IO Game
randomlyPick n (Game pile _ rules) player
        | n == 0 = return $ Game pile [player] rules
        | otherwise = randomChar >>=
          (\(chr, restOfPile) ->
            randomlyPick (n - 1) (Game (Pile restOfPile) [] rules) $ Player (Hand (chr:hand)) board)
                      where (Pile letters) = pile
                            randomChar = getRandomChar letters
                            (Player (Hand hand) board) = player

getRandomNumber :: Int -> Int -> IO Int
getRandomNumber lb ub = getStdRandom $ randomR (lb, ub)

getRandomChar :: String -> IO (Char, String)
getRandomChar s = let charIndex = getRandomNumber 0 $ (length s) - 1
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

shouldPeel :: Game -> Bool
shouldPeel (Game _ players _) = f $ find (\(Player (Hand h) _) -> (length h) == 0) players
                                where f Nothing = True
                                      f (Just _) = False

optionalPeel :: Game -> IO Game
optionalPeel game
        | shouldPeel game = peel game
        | otherwise = return game

play :: Player -> Player
play player = player

{--
 - just play in a random circle, if there is a peel, deal with it after everyone has played in whatever random order
 - play in a circle
 - -}
playRound :: Game -> IO Game
playRound game = (\(Game pile players rules) -> Game pile (play <$> players) rules) <$> optionalPeel game >>= playNextRound
                where playNextRound (Game (Pile letters) players rules)
                        | length letters == 0 = return $ Game (Pile letters) players rules
                        | otherwise = playRound $ Game (Pile letters) players rules

start :: Int -> Rules -> IO Game
start totalPlayers rules = (return $ Game fullBag players rules) >>= Game.split >>= playRound
                        where players = take totalPlayers $ repeat $ Player emptyHand emptyBoard
