import Text.PrettyPrint.HughesPJClass
import Prelude
import ReadTabIndent


makeLatex :: HierarchyTree -> Doc
makeLatex par@(Parent n i _) = text "\\begin{scope}[ocg={ref=" <> text rootName <> text ", status=on}]"
                          $$ nest 2 ( text "\\draw (0,0) node[show ocg=" <> text rootStart <> text "] {" <> text n <> text"};")
                          $$ text "\\end{scope}"
                          $$ (vcat (map buildCell tnHt))
    where 
        rootName = "Root"
        rootStart = makeID rootName 0
        tnHt = flattenHT "Root" rootStart  $ addID rootStart 0 par --flattenHT rootName i par
        -- newBuildCell x y = buildCell x rootName (makeID "A" y) 
        -- newCells = zipWith newBuildCell tnHt [1..20]



buildCell :: TreeNode ->  Doc
buildCell (Node n p pid c) = text "\\begin{scope}[ocg={ref=" <> text pid <> text  ", status=off}]"
                          $$ nest 2 (vcat makeMulti)
                          $$ text "\\tikzset{upnode=" <> text pid <> text "}"
                          $$ text "\\end{scope}\n"
    where
        makeMultiple childID child = text "\\draw (0,0) node[show ocg=" <> text childID <> text "] {" <> text child <> text "};"
                           
        makeMulti = zipWith makeMultiple (map (makeID pid)  [0..]) c

buildCell (Childless n p pid) = text "\\begin{scope}[ocg={ref=" <> text pid <> text ", status=off}]"
                             $$ text "\\draw (0,0) node[show ocg=" <> text pid <> text "] {" <> text n <> text "};"
                             $$ text "\\tikzset{upnode=" <> text pid <> text "}"
                             $$ text "\\end{scope}\n"
    
printLatex :: HierarchyTree -> Doc
printLatex (Parent n i c) = braces $  name n <> comma
                     $+$ children c
printLatex (Child i n) = braces $ name n <> comma <> varValue 1

newline :: Doc
newline = text "\n"

makeTag :: String -> Doc
makeTag = doubleQuotes.text

name :: String -> Doc
name = valTag "name" . makeTag 

children :: [HierarchyTree] -> Doc
children = valTag "children" . brackets 
                             . vcat 
                             . punctuate comma
                             . map printLatex

valTag :: String -> Doc -> Doc
valTag tag value = makeTag tag <+> colon <+> value

varValue :: Int -> Doc
varValue = valTag "value" . text . show

createLatexTab = do result <- parseTabFile
                    case result of
                         Left err -> print err
                         Right par -> writeFile "coverted.json" $ (render.makeLatex) par 


createJSONTabFile input = do result <- parseFromFile parseParent input 
                             case result of
                                  Left err -> print err
                                  Right par -> writeFile "coverted.json" $ (render.makeLatex) par 
