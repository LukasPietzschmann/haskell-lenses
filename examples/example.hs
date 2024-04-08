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

nvimConfig' = setCurrentValue "false" $ getEntry "expandtab" nvimConfig

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
