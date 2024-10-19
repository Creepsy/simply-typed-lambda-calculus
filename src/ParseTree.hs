{-# LANGUAGE UndecidableInstances #-}
module ParseTree (
    Offset,
    LCType(..),
    ParseTree'(..),
    ParseTree,
    Positioned(..),
    Typed(..)
) where

import Context (Context(..))

type Offset = Int

data LCType = Boolean | LCType :-> LCType deriving (Show, Eq, Ord)
data ParseTree' w = Constant Bool
    | Variable String
    | IfExpr {condition :: w (ParseTree' w), onTrue :: w (ParseTree' w), onFalse :: w (ParseTree' w)}
    | Application (w (ParseTree' w)) (w (ParseTree' w))
    | Abstraction {var :: (String, LCType), body :: w (ParseTree' w)}
type ParseTree w = w (ParseTree' w)

data Positioned a = Positioned a Offset
data Typed a = Typed a LCType

instance Context Positioned Offset where
    intoContext = Positioned
    context (Positioned _ c)= c
    value (Positioned v _)= v

instance Context Typed LCType where
    intoContext = Typed
    context (Typed _ c)= c
    value (Typed v _)= v

instance Show a => Show (Positioned a) where
    show (Positioned val pos) = "[" ++ show val ++ " at " ++ show pos ++ "]"

instance Show a => Show (Typed a) where
    show (Typed val t) = show val ++ " :: " ++ show t

instance (Context w b, Show (w (ParseTree' w)), Show (w LCType)) => Show (ParseTree' w) where  
    show (Constant b) = show b
    show (Variable v) = v
    show (IfExpr cond oT oF) = "if {" ++ show cond ++ ", " ++ show oT ++ ", " ++ show oF ++ "}" 
    show (Application a b) = "apply " ++ show a ++ " to " ++ show b
    show (Abstraction (v, vT) body) = show v ++ ":" ++ show vT ++ " -> " ++ show body
