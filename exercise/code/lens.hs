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
data DocumentType = TextDocument | PDFDocument | ImageDocument
  deriving (Show, Eq)

data Metadata = DocumentMetadata {
    _title :: String,
    _author :: String,
    _creationDate :: String
  } deriving Show

data Document = Document {
    docType :: DocumentType,
    metadata :: Metadata,
    content :: String
  } deriving Show

data FolderContent = Doc Document
                   | SubFolder Folder
  deriving Show

data BinaryTree = Leaf FolderContent
                | Node FolderContent BinaryTree BinaryTree
  deriving Show

data Folder = Folder {
    folderName :: String,
    contents :: BinaryTree
  } deriving Show

newtype FileSystem = FileSystem Folder
  deriving Show

makeLenses ''Metadata
makeLenses ''Document
makeLenses ''FolderContent
makeLenses ''BinaryTree
