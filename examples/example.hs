data File  = File String [Entry]
data Entry = Key String Value

data Value = Str  { getStr  :: String }
           | Int  { getInt  :: Int }
           | List { getList :: [Value] }
           | Map  { getMap  :: [(String, Value)] }

instance Show Value where
  show (Str s)  = s
  show (Int i)  = show i
  show (List l) = show l
  show (Map m)  = show m

instance Show Entry where
  show (Key k v) = k ++ " = " ++ show v

instance Show File where
  show (File p s) = unlines $ show <$> s

nvimConfig = File "~/.config/nvim/init.lua" [
    Key "line_numbers"   (Int 1),
    Key "syntax"         (Str "on"),
    Key "expandtab"      (Int 0),
    Key "telescope-tabs" (Map [("enabled", Int 1)])
  ]

disable :: File -> String -> File
disable (File pa s) pl = File pa (helpMe <$> s)
  where
    helpMe (Key k v)
      | k == pl   = Key k $ Map $ toggle <$> getMap v
      | otherwise = Key k v
    toggle ("enabled", _) = ("enabled", Int 0)
    toggle x = x

nvimConfig' = disable nvimConfig "telescope-tabs"
