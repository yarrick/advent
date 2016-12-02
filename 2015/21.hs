import Data.List (delete)

data Item = Item {
	cost :: Int,
	damage :: Int,
	armor :: Int
} deriving (Show, Eq)

weapons = [
	Item 8   4 0,
	Item 10  5 0,
	Item 25  6 0,
	Item 40  7 0,
	Item 74  8 0]

protection = [
	Item 0   0 0, --null armor
	Item 13  0 1,
	Item 31  0 2,
	Item 53  0 3,
	Item 75  0 4,
	Item 102 0 5]

rings = [
	Item 0   0 0, --null ring 1
	Item 0   0 0, --null ring 2
	Item 25  1 0,
	Item 50  2 0,
	Item 100 3 0,
	Item 20  0 1,
	Item 40  0 2,
	Item 80  0 3]

tworings :: [[Item]]
tworings = [x:y:[] | x <- rings, y <- (delete x rings)]

combinations = [w:a:rings | w <- weapons, a <- protection, rings <- tworings ]

combine :: [Item] -> Item
combine x = Item (sumi cost) (sumi damage) (sumi armor)
	where sumi fn = sum $ map fn x

data Player = Player {
	gold :: Int,
	health :: Int,
	attack :: Int,
	defend :: Int
} deriving (Show, Eq)

player :: Item -> Player
player i = Player (cost i) 100 (damage i) (armor i)

hit :: Player -> Player -> Player
hit attacker defender = Player (gold defender) hurt (attack defender) (defend defender)
	where hurt = (health defender) - max 1 (attack attacker - defend defender)

fight :: Player -> Player -> (Bool, Int)
fight boss me
	| health boss2 <= 0 = (True, gold me)
	| health me2 <= 0 = (False, gold me)
	| otherwise = fight boss2 me2
	where
		boss2 = hit me boss
		me2 = hit boss2 me

players = map (player.combine) combinations

run :: Int -> Int -> Int -> Int
run hitpoints dam arm = minimum $ map snd $ filter fst $ map (fight (Player 0 hitpoints dam arm)) players

run2 :: Int -> Int -> Int -> Int
run2 hitpoints dam arm = maximum $ map snd $ filter (not.fst) $ map (fight (Player 0 hitpoints dam arm)) players
