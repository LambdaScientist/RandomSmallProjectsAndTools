{-# LANGUAGE TupleSections #-}

module ReadTabIndent where 

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import qualified Text.PrettyPrint.HughesPJClass as P


type ChildName = Name
type ID = String
type ChildrenNames = [ChildName]
type ParentName = Name
type ParentID = ID
type Children = [HierarchyTree]
type Name = String 

data HierarchyTree = Parent Name ID Children | Child Name ID --deriving (Show)
instance P.Pretty HierarchyTree where
  pPrint (Parent name id children) = P.text "Parent:"
                            P.$+$ P.text "Name =" P.<+> P.pPrint name
                            P.$+$ P.text "ID =" P.<+> P.pPrint id
                            P.$+$ P.text "Children =" P.<+> P.pPrint children
  pPrint (Child name id) = P.text "Child:"
                    P.$+$ P.text "Name =" P.<+> P.pPrint name
                    P.$+$ P.text "ID =" P.<+> P.pPrint id
instance Show HierarchyTree where
  show = P.render.P.pPrint



data TreeNode = Node Name ParentName ParentID ChildrenNames | Childless Name ParentName ParentID --deriving (Show)
instance P.Pretty TreeNode where
  pPrint (Node name parent parentID child) = P.text "\n\nParent:"
                            P.$+$ P.text "Name =" P.<+> P.pPrint name
                            P.$+$ P.text "Parent Name =" P.<+> P.pPrint parent
                            P.$+$ P.text "Parent ID  =" P.<+> P.pPrint parentID
                            P.$+$ P.text "Children =" P.<+> (P.vcat  $ P.pPrint <$> child)
  pPrint (Childless name parent parentID) = P.text "\n\nChildless:"
                             P.$+$ P.text "Name =" P.<+> P.pPrint name
                             P.$+$ P.text "Parent Name =" P.<+> P.pPrint parent
                             P.$+$ P.text "Parent ID =" P.<+> P.pPrint parentID
instance Show TreeNode where
  show = P.render.P.pPrint

getName :: HierarchyTree -> Name
getName (Parent n _ _) = n
getName (Child n _) = n

flattenHT :: ParentName -> ParentID ->  HierarchyTree -> [TreeNode]
flattenHT parent parentID (Child name id ) = [Childless name parent parentID ]
flattenHT parent parentID (Parent name id children) = [(nodeParent nodes)] ++ childNodes
  where
    nodeParent = Node name parent parentID 
    nodes = [getName c | c <- children ]
    childNodes = concat [flattenHT name id c | c <- children ]


lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItem :: Parser String
pItem = lexeme $ some (alphaNumChar <|> oneOf " -()/.&,[]")

parseParent :: Parser HierarchyTree
parseParent = L.indentBlock scn p
  where
    mkHiTree :: Name -> [HierarchyTree] -> HierarchyTree
    mkHiTree n [] = Child n "temp"
    mkHiTree n children = Parent n "temp" children
    p = do name <- pItem
           return $ L.IndentMany Nothing (return.(mkHiTree name)) parseParent

input_text :: String
input_text = "first-chapter \n\tparagraph-one \n\tnote-A # an important note here! \n\t\tnote-B \n\tparagraph-two \n\t\tnote-1 \n\t\tnote-2\n\t\t\tFOOBAR\n\tparagraph-three"

inputText2 = "hi\n\tI am Luc\n\tThis is a computer\n\t\tFoo\n\t\t\tBar\n\t\tMeh\n\t\tMeh 2\n\t\t\tBye\n\tThis\n\tis \n\tstill\n\ta \n\tcomputer\n\t\tqqqqqqqqq\n\t\tBaz"

parseTabStr = parse parseParent "foo" inputText2
convertTest = flattenHT "root" "A" <$> parseTabStr


foo = parseTest parseParent inputText2
fooFile = parseTest parseParent <$> readFile tabFile

tabFile = "exampleTab.txt"

parseFromFile p file = runParser p file <$> readFile file

parseTabFile = parseFromFile parseParent tabFile

convertTabFile = do result <- parseTabFile
                    case result of
                         Left err -> return $ [Childless "" "" ""]
                         Right par -> return $ flattenHT "Root" "Root" $ addID "Root" 0 par 

makeID :: String -> Int -> String
makeID base num = base ++ "." ++ show num

addID :: String -> Int -> HierarchyTree -> HierarchyTree
addID root count (Child n _) = Child n (makeID root count)
addID root count (Parent n _ children) = Parent n myID $ zipWith (addID myID) [1..] children
  where myID = makeID root count





