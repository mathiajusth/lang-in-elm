module Type.Assignement exposing (..)

import AssocList as Dict exposing (Dict)
import Context as Context exposing (Context)
import Context.Quotient
import Data.Dict.Extra as Dict
import Data.Symbol as Symbol
import List.Extra as List
import Maybe.Extra as Maybe
import Type exposing (Type)
import Type.Substitutions exposing (Substitutions)
import Value exposing (Value)


type alias Assignement =
    { contextQuotient : Context.Quotient.Quotient
    , assignedType : Type
    }


infer : Value -> Maybe Assignement
infer =
    inferHelp ""


inferHelp : String -> Value -> Maybe Assignement
inferHelp variablePrefix value =
    case value of
        Value.Variable symbol ->
            let
                newType : Type
                newType =
                    Type.variableFromString <| variablePrefix ++ Symbol.toString symbol
            in
            Just
                { contextQuotient =
                    Context.Quotient.singleton
                        symbol
                        newType
                , assignedType = newType
                }

        Value.Left leftValue ->
            Maybe.map
                (\leftSumInferred ->
                    { contextQuotient = leftSumInferred.contextQuotient
                    , assignedType = Type.or leftSumInferred.assignedType (Type.variableFromString <| variablePrefix ++ "a")
                    }
                )
                (inferHelp (variablePrefix ++ "+L ") leftValue)

        Value.Right rightValue ->
            Maybe.map
                (\rightSumInferred ->
                    { contextQuotient = rightSumInferred.contextQuotient
                    , assignedType = Type.or (Type.variableFromString <| variablePrefix ++ "a") rightSumInferred.assignedType
                    }
                )
                (inferHelp (variablePrefix ++ "+R ") rightValue)

        Value.Tuple leftValue rightValue ->
            Maybe.andThen2
                (\leftProdInferred rightProdInferred ->
                    Context.Quotient.merge leftProdInferred.contextQuotient rightProdInferred.contextQuotient
                        |> Maybe.map
                            (\mergedContextQuotients ->
                                { contextQuotient = mergedContextQuotients
                                , assignedType =
                                    Type.and leftProdInferred.assignedType rightProdInferred.assignedType
                                }
                            )
                )
                (inferHelp (variablePrefix ++ "&L ") leftValue)
                (inferHelp (variablePrefix ++ "&R ") rightValue)

        Value.ProjLeft body ->
            Maybe.andThen
                (\({ contextQuotient } as bodyInferred) ->
                    let
                        leftAndType =
                            Type.variableFromString <| variablePrefix ++ "a"
                    in
                    Type.unify
                        bodyInferred.assignedType
                        (Type.and
                            leftAndType
                            (Type.variableFromString <| variablePrefix ++ "b")
                        )
                        |> Maybe.andThen (Type.Substitutions.merge bodyInferred.contextQuotient.substitutions)
                        |> Maybe.map
                            (\newSubstitutions ->
                                { contextQuotient =
                                    { contextQuotient | substitutions = newSubstitutions }
                                , assignedType = leftAndType
                                }
                            )
                )
                (inferHelp (variablePrefix ++ "proj-L ") body)

        Value.ProjRight body ->
            Maybe.andThen
                (\({ contextQuotient } as bodyInferred) ->
                    let
                        rightAndType =
                            Type.variableFromString <| variablePrefix ++ "b"
                    in
                    Type.unify
                        bodyInferred.assignedType
                        (Type.and
                            (Type.variableFromString <| variablePrefix ++ "a")
                            rightAndType
                        )
                        |> Maybe.andThen (Type.Substitutions.merge bodyInferred.contextQuotient.substitutions)
                        |> Maybe.map
                            (\newSubstitutions ->
                                { contextQuotient =
                                    { contextQuotient | substitutions = newSubstitutions }
                                , assignedType = rightAndType
                                }
                            )
                )
                (inferHelp (variablePrefix ++ "proj-R ") body)

        Value.Lambda symbol body ->
            Maybe.map
                (\bodyInferred ->
                    Maybe.unwrap
                        { contextQuotient = bodyInferred.contextQuotient
                        , assignedType =
                            Type.arrow (Type.variableFromString <| variablePrefix ++ Symbol.toString symbol) bodyInferred.assignedType
                        }
                        (\symbolType ->
                            { contextQuotient =
                                let
                                    oldContextQuotient =
                                        bodyInferred.contextQuotient
                                in
                                { oldContextQuotient
                                    | context =
                                        Dict.remove symbol oldContextQuotient.context
                                }
                            , assignedType = Type.arrow symbolType bodyInferred.assignedType
                            }
                        )
                        (Dict.get symbol <| bodyInferred.contextQuotient.context)
                )
                (inferHelp (variablePrefix ++ "body ") body)

        Value.Application lambdaValue argumentValue ->
            Maybe.andThen2
                (\functionInferred argumentInferred ->
                    Context.Quotient.merge functionInferred.contextQuotient argumentInferred.contextQuotient
                        |> Maybe.andThen
                            (\newContextQuotients ->
                                let
                                    domainType : Type
                                    domainType =
                                        Type.variableFromString <| variablePrefix ++ "domain"
                                in
                                Type.unify
                                    functionInferred.assignedType
                                    (Type.arrow argumentInferred.assignedType domainType)
                                    |> Maybe.andThen (Type.Substitutions.merge newContextQuotients.substitutions)
                                    |> Maybe.map
                                        (\newSubstitutions ->
                                            { contextQuotient =
                                                { newContextQuotients | substitutions = newSubstitutions }
                                            , assignedType = domainType
                                            }
                                        )
                            )
                )
                (inferHelp (variablePrefix ++ "function ") lambdaValue)
                (inferHelp (variablePrefix ++ "argument ") argumentValue)


normalize : Assignement -> { context : Context, assignedType : Type }
normalize { assignedType, contextQuotient } =
    let
        representedAssignedType =
            assignedType
                |> Type.Substitutions.represent contextQuotient.substitutions

        uniqueSumbols : List Type.Symbol
        uniqueSumbols =
            representedAssignedType
                |> Type.toSymbolList
                |> List.map Symbol.toString
                -- TODO implement List.unique with for any type
                |> List.unique
                |> List.map Symbol.fromString

        simplifyingSubstitutions : Substitutions
        simplifyingSubstitutions =
            List.zip uniqueSumbols
                (List.range 97 (97 + List.length uniqueSumbols)
                    |> List.map (Char.fromCode >> String.fromChar >> Type.variableFromString)
                )
                |> Dict.fromList
    in
    { context =
        contextQuotient.context
            |> Context.mapTypes
                (Type.Substitutions.represent contextQuotient.substitutions
                    >> Type.Substitutions.represent simplifyingSubstitutions
                )
    , assignedType =
        representedAssignedType
            |> Type.Substitutions.represent simplifyingSubstitutions
    }


inferNice : Value -> Maybe { value : String, assignedType : Type, context : Context }
inferNice value =
    value
        |> infer
        |> Maybe.map normalize
        |> Maybe.map
            (\{ assignedType, context } ->
                { value = Value.toString value
                , assignedType = assignedType
                , context = context
                }
            )
