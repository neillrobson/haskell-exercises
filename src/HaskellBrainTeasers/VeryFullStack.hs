module HaskellBrainTeasers.VeryFullStack where

data RoseTree a = RoseTree a [RoseTree a]

breadthFirst :: RoseTree a -> [a]
breadthFirst (RoseTree a trees) = a : go trees
  where
    go [] = []
    go subtrees =
      let (vals, subs) = foldr accumRoseTree ([], []) subtrees
       in vals <> go subs
    accumRoseTree ~(RoseTree v t) (vs, ts) = (v : vs, t <> ts)

main :: IO ()
main =
  if "Maple" `elem` breadthFirst forest
    then putStrLn "pancake time"
    else putStrLn "no syrup"
  where
    forest =
      RoseTree
        "Aspen"
        [ RoseTree
            "Baobab"
            [ RoseTree "Paw Paw" [],
              RoseTree "Basswood" []
            ],
          RoseTree
            "Cherry"
            [ RoseTree "Maple" [],
              error "boom"
            ]
        ]

-- badPatternMatch :: Maybe String -> String
-- badPatternMatch ~(Just message) = message
-- badPatternMatch Nothing = "Nothing to see here"
