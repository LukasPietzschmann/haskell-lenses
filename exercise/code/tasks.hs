{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

-- | Exercise 0 | --
test = ((1, 0), 3, 4) & _1 . _2 .~ 2

todo :: a
todo = error "Not yet implemented"



-- | Exercise 1 | --
v = (1, ("Hi", "Ho"), 2)
-- a)
l :: Lens (Int, (a, String), Int) (Int, (b, String), Int) a b
l = todo
-- b)
partb = todo -- use operator
partb' = todo -- use function
-- c)
vc = todo -- use operator
vc' = todo -- use function
-- d)
vd = todo
-- e)
v' = (["Hi", "Ho"], ["He", "Hu"])
l' :: Traversal' ([String], [String]) String
l' = todo
-- f)
-- This type is like Lens' ([String], [String]) String" with f = Const String
l'' :: Traversal' ([String], [String]) String
l'' = todo
-- g)
l''' :: Traversal' ([String], [String]) String
l''' = todo
-- h)
vh = todo



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

-- b)
example :: File Document
example = Folder "root" [
    File $ Doc Text (Metadata ".zshenv" "root") "",
    Folder "home" [
        Folder "luke" [
            File $ Doc Text (Metadata ".zshenv" "luke") "export EDITOR=nvim",
            File $ Doc Text (Metadata ".zsh_history" "luke") "sudo dnf rm java"
          ]
      ]
  ]

-- c)

-- d)

-- e)
searchFiles :: String -> [Document] -> [Document]
searchFiles = todo
-- f)
flattenFolders :: File Document -> [Document]
flattenFolders = todo
-- g)
searchFiles' :: String -> File Document -> [Document]
searchFiles' = todo

-- h)

-- i)

-- j)
searchFiles'' = todo
-- k)
searchAuthor = todo
-- l)
filesWithAuthor = todo



-- | Exercise 3 | --
-- a)
infixr 4 .~.
(.~.) :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
(.~.) = todo
-- b)
infixr 4 %~.
(%~.) :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
(%~.) = todo
-- c)
infixr 4 *~.
(*~.) :: Num a => ((a -> Identity a) -> s -> Identity t) -> a -> s -> t
(*~.) = todo
-- d)
infixr 4 .^.
(.^.) :: s -> ((a -> Const a b) -> s -> Const a t) -> a
(.^.) = todo
