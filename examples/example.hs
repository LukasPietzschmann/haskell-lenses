data File  = File {
    name    :: String,
    entries :: [Entry]
}
data Entry = Entry {
    key   :: String,
    value :: Value
}
data Value = Value {
    actualValue  :: Maybe String,
    defaultValue :: String
}

instance Show File where
    show (File name entries) = unlines $ show <$> entries

instance Show Entry where
    show (Entry key value) = key ++ " -> " ++ show value

instance Show Value where
    show (Value Nothing defaultValue) = defaultValue
    show (Value (Just actualValue) _) = actualValue

nvimConfig = File "~/.config/nvim/init.lua" [
        Entry "expandtab" (Value Nothing "true"),
        Entry "cmdheight" (Value (Just "0") "1"),
        Entry "textwidth" (Value (Just "88") "")
    ]

getEntry :: String -> File -> Entry
getEntry k = head . filter ((==) k . key) . entries

getActualValue :: Entry -> String
getActualValue = defaultValue . value

setActualValue :: String -> Entry -> Entry
setActualValue newValue entry = entry {
    value = (value entry) {
        actualValue = Just newValue
    }
}

nvimConfig' = setActualValue "false" $ getEntry "expandtab" nvimConfig
