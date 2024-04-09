data File = File {
  name    :: String,
  entries :: [Entry]
}
data Entry = Entry {
  key   :: String,
  value :: Value
}
data Value = Value {
  curr :: String,
  def  :: String
}

instance Show File where
  show (File name entries) = unlines $ show <$> entries

instance Show Entry where
  show (Entry key value) = key ++ " -> " ++ show value

instance Show Value where
  show (Value "" defaultValue) = defaultValue
  show (Value actualValue _) = actualValue

nvimConfig = File "~/.config/nvim/init.lua" [
    Entry "expandtab" (Value "" "true"),
    Entry "cmdheight" (Value "0" "1"),
    Entry "textwidth" (Value "88" "")
  ]

getEntry :: String -> File -> Entry
getEntry k = head . filter ((==) k . key) . entries

getCurrentValue :: Entry -> String
getCurrentValue = def . value

setCurrentValue :: String -> Entry -> Entry
setCurrentValue newValue entry = entry {
  value = (value entry) {
    curr = newValue
  }
}

newValue = setCurrentValue "false" $ getEntry "expandtab" nvimConfig

modifyCurrentValue :: (String -> String) -> Value -> Value
modifyCurrentValue f value = value {
  curr = f $ curr value
}

modifyEntriesValue :: (Value -> Value) -> Entry -> Entry
modifyEntriesValue f entry = entry {
  value = f $ value entry
}

modifyEntriesCurrentValue :: (String -> String) -> Entry -> Entry
modifyEntriesCurrentValue = modifyEntriesValue . modifyCurrentValue

setCurrentValue' = modifyEntriesCurrentValue . const

data Lens s a = Lens {
  get :: s -> a,
  modify :: (a -> a) -> s -> s
}

compose :: Lens a b -> Lens b c -> Lens a c
compose (Lens g m) (Lens g' m') = Lens {
  get = g' . g,
  modify = m . m'
}

currentValueL :: Lens Value String
currentValueL = Lens {
  get = curr,
  modify = \f value -> value { curr = f $ curr value }
}

entryValueL :: Lens Entry Value
entryValueL = Lens {
  get = value,
  modify = \f entry -> entry { value = f $ value entry }
}

entryCurrentValueL :: Lens Entry String
entryCurrentValueL = entryValueL `compose` currentValueL

setCurrentValue'' = modify entryCurrentValueL . const

newValue'' = setCurrentValue'' "false" $ getEntry "expandtab" nvimConfig
