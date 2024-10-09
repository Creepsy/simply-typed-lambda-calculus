module ParseTree (
    LCType(..),
    WithContext(..),
    ParseTree(..)
) where

type Offset = Int

data LCType = Boolean | LCType :-> LCType deriving (Show)
data WithContext a = a :@ Offset deriving (Show)
data ParseTree = Constant Bool 
    | Variable String
    | IfExpr {condition :: WithContext ParseTree, onTrue :: WithContext ParseTree, onFalse :: WithContext ParseTree}
    | Application (WithContext ParseTree) (WithContext ParseTree)
    | Abstraction {var :: WithContext ParseTree, boundType :: WithContext LCType, body :: WithContext ParseTree}
    deriving (Show)