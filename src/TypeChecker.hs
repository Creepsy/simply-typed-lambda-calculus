module TypeChecker (
    typeCheck
) where
import ParseTree (ParseTree, Positioned (Positioned), Typed (Typed), LCType (Boolean, (:->)), ParseTree' (Constant, Variable, IfExpr, Application, Abstraction), Offset)
import Parser (ErrorBundle)
import qualified Data.Map as M
import Data.Map ((!?))
import Context (Context(context))
import Text.Megaparsec (PosState, TraversableStream, ParseErrorBundle (ParseErrorBundle, bundleErrors, bundlePosState), ParseError (FancyError), ErrorFancy (ErrorCustom))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import TypeError (LCError (UndefinedVariableError, ArgumentTypeError, IFDivergingTypesError, IFConditionError, NotApplicableError))

type TypeMap = M.Map String LCType

typeCheck :: PosState String -> ParseTree Positioned -> Either ErrorBundle (ParseTree Typed)
typeCheck pS = typeCheck' pS M.empty

typeCheck' :: PosState String -> TypeMap -> ParseTree Positioned -> Either ErrorBundle (ParseTree Typed)
typeCheck' _ _ (Positioned (Constant b) _) = pure $ Typed (Constant b) Boolean
typeCheck' pS tM (Positioned (Variable var) off) = case tM !? var of
    Just t -> pure $ Typed (Variable var) t
    Nothing -> Left . makeError pS off $ UndefinedVariableError var
typeCheck' pS tM (Positioned (IfExpr cond onT onF) off) = do
    cond' <- typeCheck' pS tM cond
    onT' <- typeCheck' pS tM onT
    onF' <- typeCheck' pS tM onF
    if context cond' /= Boolean then
        Left . makeError pS off $ IFConditionError (context cond')
    else if context onT' /= context onF' then
        Left . makeError pS off $ IFDivergingTypesError (context onT') (context onF')
    else
        pure $ Typed (IfExpr cond' onT' onF') (context onT')
typeCheck' pS tM (Positioned (Application operator operand) off) = do
    operator' <- typeCheck' pS tM operator
    operand' <- typeCheck' pS tM operand
    case (context operator', context operand') of
        (in1 :-> res, in2) | in1 == in2 -> pure $ Typed (Application operator' operand') res
                           | otherwise -> Left . makeError pS off $ ArgumentTypeError in2 in1
        (app, _) -> Left . makeError pS off $ NotApplicableError app
typeCheck' pS tM (Positioned (Abstraction (v, vT) b) _) = do
    let tM' = M.insert v vT tM
    b' <- typeCheck' pS tM' b
    pure $ Typed (Abstraction (v, vT) b') (vT :-> context b')

makeError :: TraversableStream s => PosState s -> Offset -> LCError -> ParseErrorBundle s LCError
makeError pS offset err = ParseErrorBundle {
    bundleErrors = NonEmpty.fromList [FancyError offset (Set.singleton $ ErrorCustom err)],
    bundlePosState = pS
}