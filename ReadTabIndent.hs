
{-# LANGUAGE TupleSections #-}


import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import qualified Text.PrettyPrint.HughesPJClass as P


type Children = [HierarchyTree]
type Name = String 

data HierarchyTree = Parent Name Children | Child Name --deriving (Show)
instance P.Pretty HierarchyTree where
  pPrint (Parent name children) = P.text "Parent:"
                            P.$+$ P.text "Name =" P.<+> P.pPrint name
                            P.$+$ P.text "Children =" P.<+> P.pPrint children
  pPrint (Child name) = P.text "Child:"
                    P.$+$ P.text "Name =" P.<+> P.pPrint name
instance Show HierarchyTree where
  show = P.render.P.pPrint


lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItem :: Parser String
pItem = lexeme $ some (alphaNumChar <|> char '-')

parseParent :: Parser HierarchyTree
parseParent = L.indentBlock scn p
  where
    mkHiTree :: Name -> [HierarchyTree] -> HierarchyTree
    mkHiTree n [] = Child n
    mkHiTree n children = Parent n children
    p = do name <- pItem
           return $ L.IndentMany Nothing (return.(mkHiTree name)) parseParent

input_text :: String
input_text = "first-chapter \n\tparagraph-one \n\tnote-A # an important note here! \n\t\tnote-B \n\tparagraph-two \n\t\tnote-1 \n\t\tnote-2\n\t\t\tFOOBAR\n\tparagraph-three"

foo = parseTest parseParent input_text


