{-# LANGUAGE FunctionalDependencies #-}
module Context (
    Context(..)
) where

class Context c b | c -> b where
    intoContext :: a -> b -> c a
    intoContext = (<@>)

    (<@>) :: a -> b -> c a
    (<@>) = intoContext

    context :: c a -> b
    value :: c a -> a