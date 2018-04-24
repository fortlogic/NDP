module Resources.Constraints where

data Constraints = Constraints {
  rawConstraints :: [String],
  netConstraints :: [(String, [NetParameter])]
  } deriving (Read, Show)

data NetParameter = NetKV String String -- Arbitrary key/value pair
                  | NetFlag String      -- unary flag (like PULLUP)
                  | NetLoc String       -- Net location
                  deriving (Read, Show)

-- Takes a net parameter irons out the sugar, converting it to NetKV or NetFlag
flattenNetParameter :: NetParameter -> NetParameter
flattenNetParameter p@(NetKV _ _) = p
flattenNetParameter p@(NetFlag _) = p
flattenNetParameter (NetLoc l) = NetKV "LOC" ("\"" ++ l ++ "\"")

