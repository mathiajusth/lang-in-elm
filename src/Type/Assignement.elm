module Type.Assignement exposing (..)

import Context exposing (Context)
import Maybe.Extra as Maybe
import Type exposing (Type)
import Value exposing (Value)


type alias Contextual =
    { contextQuotient : Context.Quotient
    , assignedType : Type
    }


infer : Value -> Maybe Contextual
infer =
    inferHelp ""


inferHelp : String -> Value -> Maybe Contextual
inferHelp variablePrefix value =
    let
        newType : Type
        newType =
            Type.variable <| variablePrefix ++ "_var"
    in
    case value of
        Value.Variable symbol ->
            Just
                { contextQuotient =
                    Context.qutientSingletion
                        { valueSymbol = symbol
                        , type_ = newType
                        }
                , assignedType = newType
                }

        Value.Left leftValue ->
            Maybe.map
                (\leftSumInferred ->
                    { contextQuotient = leftSumInferred.contextQuotient
                    , assignedType = Type.Or leftSumInferred.assignedType (Type.variable <| variablePrefix ++ "_rightSum")
                    }
                )
                (inferHelp (variablePrefix ++ "L-sum ") leftValue)

        Value.Right rightValue ->
            Maybe.map
                (\rightSumInferred ->
                    { contextQuotient = rightSumInferred.contextQuotient
                    , assignedType = Type.Or (Type.variable <| variablePrefix ++ "_leftSum") rightSumInferred.assignedType
                    }
                )
                (inferHelp (variablePrefix ++ "R-sum ") rightValue)

        Value.Tuple leftValue rightValue ->
            Maybe.andThen2
                (\leftProdInferred rightProdInferred ->
                    Context.quotientMerge leftProdInferred.contextQuotient rightProdInferred.contextQuotient
                        |> Maybe.map
                            (\mergedContextQuotients ->
                                { contextQuotient = mergedContextQuotients
                                , assignedType =
                                    Type.And leftProdInferred.assignedType rightProdInferred.assignedType
                                }
                            )
                )
                (inferHelp (variablePrefix ++ "L-prod ") leftValue)
                (inferHelp (variablePrefix ++ "R-prod ") rightValue)
