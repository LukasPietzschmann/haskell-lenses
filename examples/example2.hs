{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data File  = File  {_name :: String, _entries :: [Entry]}
data Entry = Entry {_key  :: String, _value   :: Value  }
data Value = Value {_curr :: String, _def     :: String }

makeLenses ''File
makeLenses ''Entry
makeLenses ''Value

instance Show File where
  show (File name entries) = unlines $ show <$> entries

instance Show Entry where
  show (Entry key value) = key ++ " -> " ++ show value

instance Show Value where
  show (Value "" defaultValue) = defaultValue
  show (Value actualValue _) = actualValue

config = File "~/.config/nvim/init.lua" [
    Entry "expandtab" (Value "" "true"),
    Entry "cmdheight" (Value "0" "1"),
    Entry "textwidth" (Value "88" "")
  ]
