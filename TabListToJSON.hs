import Text.PrettyPrint.HughesPJClass
import Prelude
import ReadTabIndent


printJSON :: HierarchyTree -> Doc
printJSON (Parent n c) = braces $  name n <> comma
                     $+$ children c
printJSON (Child n) = braces $ name n <> comma <> varValue 1

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
                             . map printJSON

valTag :: String -> Doc -> Doc
valTag tag value = makeTag tag <+> colon <+> value

varValue :: Int -> Doc
varValue = valTag "value" . text . show

createJSONTab = do result <- parseTabFile 
                   case result of
                        Left err -> print err
                        Right par -> writeFile "coverted.json" $ (render.printJSON) par 


createJSONTabFile input = do result <- parseFromFile parseParent input 
                             case result of
                                  Left err -> print err
                                  Right par -> writeFile "coverted.json" $ (render.printJSON) par 