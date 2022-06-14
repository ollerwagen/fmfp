type Value = Char
type Identifier = Char

universe :: [Value]
universe = ['a', 'b', 'c']

universeAsArgs :: Int -> [[Value]]
universeAsArgs 0 = [[]]
universeAsArgs n = [x:y | x <- universe, y <- universeAsArgs (n - 1)]

data AExp = Val Value | Id Identifier
type Pred = (Int, [Value] -> Bool)

evalArithmetic :: [(Identifier, Value)] -> AExp -> Value
evalArithmetic _          (Val x) = x
evalArithmetic []         (Id  i) = error "Identifier not found"
evalArithmetic ((n,v):xs) (Id i)  = if n == i then v else evalArithmetic xs (Id i)

evalPred :: Pred -> [Value] -> Bool
evalPred p args
  | length args == fst p = snd p args
  | otherwise            = error "Incorrect Argument Count"

data BExp = And BExp BExp | Or BExp BExp | Impl BExp BExp | Not BExp
            | Forall Identifier BExp | Exists Identifier BExp
            | Predicate Pred [AExp]

write :: [(Identifier, Value)] -> Identifier -> Value -> [(Identifier, Value)]
write [] n v            = [(n, v)]
write ((n', v'):xs) n v
  | n == n'   = (n, v) : xs
  | otherwise = (n', v') : write xs n v

verify :: BExp -> Bool
verify e = aux e []
  where
    aux :: BExp -> [(Identifier, Value)] -> Bool
    aux (And     e1 e2) defs = aux e1 defs && aux e2 defs
    aux (Or      e1 e2) defs = aux e1 defs || aux e2 defs
    aux (Impl    e1 e2) defs = not (aux e1 defs) || aux e2 defs
    aux (Not         e) defs = not (aux e defs)
    aux (Forall    x e) defs = foldr (&&) True $ map (aux e) [write defs x v | v <- universe]
    aux (Exists    x e) defs = foldr (||) False $ map (aux e) [write defs x v | v <- universe]
    aux (Predicate p a) defs = evalPred p $ map (evalArithmetic defs) a

-- Exercise Sheet 2

-- Task 2.a)
{--
    pT = (1, (\x -> if x == ['a'] then True else False))
    qT = pT

    pF = pT
    qF = (1, not . snd qT)

    p = pT
    q = qT

    ex1 = Exists 'x' $ Predicate p [Id 'x']
    ex2 = Exists 'y' $ Predicate q [Id 'y']
    ex3 = Exists 'x' $ And (Predicate p [Id 'x']) (Predicate q [Id 'x'])
    formula = Impl (And ex1 ex2) ex3
--}

-- Task 2.b)
{--
    rT :: (Int, [Value] -> Bool)
    rT = (2, (\x -> if x == ['a', 'a'] || x == ['a', 'b'] then True else False))

    qT :: (Int, [Value] -> Bool)
    qT = (1, (\x -> if x == ['c'] then False else True))

    rF = rT
    qF = (1, (\x -> if x == ['a'] then True else False))

    r = rF
    q = qF

    ex1 = Exists 'y' (And (Predicate r [Id 'x', Id 'y']) (Predicate q [Id 'y']))
    fa1 = Forall 'y' (Impl (Predicate r [Id 'x', Id 'y']) (Predicate q [Id 'y']))
    formula = Forall 'x' $ Impl ex1 fa1
--}