{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

-- | Exercise 0 | --
test = ((1, 0), 3, 4) & _1 . _2 .~ 2



-- | Exercise 1 | --
v = (1, ("Hi", "Ho"), 2)
-- a)
l :: Lens (Int, (a, String), Int) (Int, (b, String), Int) a b
l = _2 . _1
-- b)
partb = v ^. l
partb' = view l v
-- c)
vc = v & l .~ 42
vc' = set l 42 v
-- d)
vd = vc & l *~ 11
-- e)
v' = (["Hi", "Ho"], ["He", "Hu"])
-- This type is like Lens' ([String], [String]) String" with f = Const String
l' :: (String -> Const String String) -> ([String], [String]) -> Const String ([String], [String])
l' = _1 . _head
-- f)
-- This type is like Lens' ([String], [String]) String" with f = Const String
l'' :: (String -> Const String String) -> ([String], [String]) -> Const String ([String], [String])
l'' = _1 . ix 0
-- g)
-- This type is like Lens' ([String], [String]) String" but with an additional
-- Applicative constraint for "both".
l''' :: Applicative f => (String -> f String) -> ([String], [String]) -> f ([String], [String])
l''' = both . _head
-- h)
vh = v' & l''' .~ "Boo"



-- | Exercise 2 | --
data DocumentType = Text | PDF | Image
  deriving (Show, Eq)

data Metadata = Metadata {
    _title :: String,
    _author :: String
  } deriving Show

data Document = Doc {
    _docType :: DocumentType,
    _metadata :: Metadata,
    _content :: String
  } deriving Show

-- In good operating systems, "everything is a file"
-- We don't specify an inner type, as we later want to fold over it, but we
-- will only use "File Document"
data File a = Folder String [File a] | File a
  deriving Show

makePrisms ''DocumentType
makeLenses ''Document
makePrisms ''File

-- a)
title :: Lens' Metadata String
title = lens (\(Metadata t _ ) -> t) (\(Metadata _ a) t -> Metadata t a)
author :: Lens' Metadata String
author = lens (\(Metadata _ a ) -> a) (\(Metadata t _) a -> Metadata t a)
-- b)
example :: File Document
example = Folder "root" [
    Folder "home" [
        Folder "luke" [
            File $ Doc Text (Metadata ".zshenv" "Luke") "export EDITOR=nvim",
            File $ Doc Text (Metadata ".zsh_history" "Luke") "dnf rm java"
          ]
      ]
  ]
-- example ^. _File . metadata
-- it throws a type error, as we are using prisms with the lens-view-operator and the
-- type of the resulting value is not a Monoid.
-- c)
instance Semigroup Metadata where
  (Metadata t1 a1) <> (Metadata t2 a2) = Metadata (t1 <> t2) (a1 <> a2)
instance Monoid Metadata where
  mempty = Metadata mempty mempty
-- d)
-- example ^? _File . metadata
-- or
-- example ^.. _File . metadata
-- e)
p :: String -> Document -> Bool
p s (Doc _ m _) = _title m == s
searchFile :: String -> [Document] -> Maybe Document
searchFile s fs = case filter (p s) fs of
    [] -> Nothing
    (x:_) -> Just x
-- f)
instance Foldable File where
  foldMap :: Monoid m => (a -> m) -> File a -> m
  foldMap f (Folder _ fs) = foldMap (foldMap f) fs
  foldMap f (File doc) = f doc
-- g)
-- Nice, there's folded :)
-- h)
find s = folded . filtered (p s)
