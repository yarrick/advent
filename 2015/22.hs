import Data.List

data Spell = Spell {
  sid :: Int,
  cost :: Int,
  action :: (Game -> Game)
}

data Effect = Effect {
  eid :: Int,
  duration :: Int,
  shield :: Int,
  poison :: Int,
  recharge :: Int
} deriving (Show, Eq)

data Game = Game {
  health :: Int,
  mana :: Int,
  armor :: Int,
  penalty :: Int,
  boss_health :: Int,
  boss_damage :: Int,
  effects :: [Effect],
  used_mana :: Int,
  used_spells :: [Int]
} deriving (Show, Eq)

spells = [
  -- Magic missile
  Spell {sid=0, cost=53, action=(\g -> g { boss_health = boss_health g - 4 })},
  -- Drain
  Spell {sid=1, cost=73, action=(\g -> g { boss_health = boss_health g - 2, health = health g + 2 })},
  -- Shield
  Spell {sid=2, cost=113, action=(\g ->
    (addeffect g (Effect {eid=2, duration = 6, shield = 7, poison = 0, recharge = 0}))
       { armor = armor g + 7 } )},
  -- Poison
  Spell {sid=3, cost=173, action=(\g ->
    addeffect g (Effect {eid=3, duration = 6, shield = 0, poison = 3, recharge = 0}))},
  -- Recharge
  Spell {sid=4, cost=229, action=(\g ->
    addeffect g (Effect {eid=4, duration = 5, shield = 0, poison = 0, recharge = 101}))}]

addeffect :: Game -> Effect -> Game
addeffect g e = g { effects = effects g ++ [e] }

effect :: Game -> Effect -> Game
effect g e
  | duration e == 0 = g
  | shield e > 0 && duration e == 1 = g { armor = armor g - shield e }
  | shield e > 0 = g
  | poison e > 0 = g { boss_health = boss_health g - poison e }
  | recharge e > 0 = g { mana = mana g + recharge e }

ageeffects :: [Effect] -> [Effect]
ageeffects [] = []
ageeffects (e:es)
  | duration e == 1 = ageeffects es
  | otherwise = e { duration = duration e - 1 } : ageeffects es

tickeffects :: Game -> Game
tickeffects g = applied { effects = ageeffects (effects applied) }
  where applied = foldl effect g (effects g)

bossattack :: Game -> Game
bossattack g = g { health = health g - damage - penalty g }
  where damage = maximum [boss_damage g - armor g, 1]

bossround :: Game -> [Game]
bossround g
  | health g <= penalty g = []
  | gamewon gg = [gg]
  | gamelost ggg = []
  | otherwise = playerround ggg
  where
    gg = tickeffects g
    ggg = bossattack gg

playerround :: Game -> [Game]
playerround g
  | gamewon gg = [gg]
  | length magic == 0 = []
  | gamelost gg = []
  | otherwise = concat $ map bossround $ map (\s -> (action s) (paid s)) magic
  where
    gg = tickeffects g
    magic = filter (affordable gg) spells
    paid sp = (paymana gg (cost sp)) { used_spells = used_spells gg ++ [sid sp] }

gamewon :: Game -> Bool
gamewon g = boss_health g <= 0

gamelost :: Game -> Bool
gamelost g = health g <= 0 || used_mana g > 1500

affordable :: Game -> Spell -> Bool
affordable g s = cost s <= mana g && not ( elem (sid s) $ map eid (effects g))

paymana :: Game -> Int -> Game
paymana g cost = g { mana = mana g - cost, used_mana = used_mana g + cost }

newgame :: Int -> Int -> Int -> Int -> Int -> Game
newgame phealth pmana bhealth bdamage pen = Game { health = phealth, mana = pmana, armor = 0,
  penalty = pen, boss_health = bhealth, boss_damage = bdamage, effects = [], used_mana = 0, used_spells = [] }

cheaper :: Game -> Game -> Ordering
cheaper a b = compare (used_mana a) (used_mana b)

run bhealth bdamage penalty = used_mana $ head $ sortBy cheaper $ playerround game
  where game = newgame 50 500 bhealth bdamage penalty

process rows = map show [run hit dmg 0, run hit dmg 1]
    where (hit:dmg:_) = map (read.last.words) rows

main :: IO ()
main = interact (unlines . process . lines)
