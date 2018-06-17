module Trie (Trie, containsWord, makeTrie) where
import System.IO

data Node = Node Char (Maybe Trie)
instance Show Node where
  show (Node c (Just t)) = "Trie(" ++ (show c ) ++ ",\t" ++ (show t) ++ ")"
  show (Node c Nothing) = "Node(" ++ (show c) ++ ")"
data Trie = Trie [Node]
instance Show Trie where
  show (Trie nodes) = "Trie(\n" ++ (show nodes) ++ ")"

addWord :: Trie -> String -> Trie
addWord (Trie nodes) [] = Trie []
addWord (Trie nodes) (h:hs) = Trie $ putLetter h nodes
                    where putLetter c [] = Node c (add Nothing): []
                          putLetter c (Node nextChar choices : rest)
                            | c > nextChar = Node nextChar choices : (putLetter c rest)
                            | c < nextChar = Node c (add choices) : Node nextChar choices : rest
                            | c == nextChar = Node nextChar (add choices) : rest
                          add (Just choices) = Just $ addWord choices $ hs
                          add Nothing = Just $ addWord (Trie []) $ hs


makeTrie :: [String] -> Trie
makeTrie = foldl addWord (Trie [])

containsWord :: Trie -> String -> Bool
containsWord trie [] = True
containsWord (Trie []) _ = False
containsWord (Trie ((Node n Nothing):ns)) (c:[]) = c == n
containsWord (Trie ((Node n Nothing):ns)) cs = False
containsWord (Trie ((Node n (Just nextTrie)):ns)) (c:cs)
        | c == n = containsWord nextTrie cs
        | otherwise = containsWord (Trie ns) (c:cs)
