module TypeError (
    LCError(UndefinedVariableError, ArgumentTypeError, IFDivergingTypesError, IFConditionError, NotApplicableError)
) where
import ParseTree (LCType (Boolean))
import Text.Megaparsec (ShowErrorComponent (showErrorComponent))

data LCError = UndefinedVariableError String
    | ArgumentTypeError LCType LCType -- found expected
    | IFDivergingTypesError LCType LCType -- left right
    | IFConditionError LCType -- found
    | NotApplicableError LCType --
    deriving (Ord, Eq)

instance Show LCError where
    show (UndefinedVariableError var) = "The variable " ++ var ++ " is undefined."
    show (ArgumentTypeError found expected) = "Expected an argument of type " ++ show found ++ ", but the argument is of type " ++ show expected ++ "."
    show (IFDivergingTypesError left right) = "The branches of the if condition are of conflicting types: " ++ show left ++ " and " ++ show right
    show (IFConditionError found) = "The condition must be of type " ++ show Boolean ++ " but is of type " ++ show found ++ "."
    show (NotApplicableError found) = "A value of type " ++ show found ++ " can not be used as a function."

instance ShowErrorComponent LCError where
    showErrorComponent err = "TypeError: " ++ show err
    


