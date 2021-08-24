module ASTTools exposing
    ( commandName2
    , commandString
    , findSectionByLabel
    , getRaw
    , getText
    , getTextFromText
    , stringListOfConfig
    , stringOfLocVal
    , stringOfMarkup
    , textFromMarkup
    )




import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Parse.Syntax as Syntax
import Camperdown.Problem as Problem


{-|

This is test.

-}
findSectionByLabel : String -> List Syntax.Section -> Maybe Syntax.Section
findSectionByLabel label_ sections =
    List.filter (\sec -> getLabel sec == label_) sections
        |> List.head


getLabel { label } =
    case label of
        Syntax.Named ( _, s ) ->
            s

        Syntax.Anonymous n ->
            String.fromInt n


getRaw text =
    case text of
        Syntax.Raw str ->
            Just str

        _ ->
            Nothing


commandName2 : ( Syntax.Mark, String ) -> String
commandName2 command =
    case command of
        ( Syntax.Bang, str ) ->
            str

        ( Syntax.Huh, str ) ->
            str


getText : Syntax.Element -> List Syntax.Text
getText element =
    case element of
        Syntax.Paragraph data ->
            List.map (getTextFromText >> Syntax.Raw) data.contents

        _ ->
            []


stringListOfConfig : Syntax.Config -> List String
stringListOfConfig ( locVal, _ ) =
    List.map stringOfLocVal locVal |> List.concat


stringOfLocVal : Loc Syntax.Value -> List String
stringOfLocVal locVal =
    case Loc.value locVal of
        Syntax.Variable str ->
            [ str ]

        Syntax.String str ->
            [ str ]

        Syntax.Int k ->
            [ String.fromInt k ]

        Syntax.Markup markup ->
            stringOfMarkup markup


stringOfMarkup : List Syntax.Text -> List String
stringOfMarkup texts =
    List.map getTextFromText texts


commandString ( mLocStr, config ) =
    case mLocStr of
        Nothing ->
            ""

        Just locStr ->
            Loc.value locStr


getTextFromText : Syntax.Text -> String
getTextFromText text =
    case text of
        Syntax.Raw str ->
            str

        Syntax.Verbatim _ locStr ->
            Loc.value locStr

        Syntax.Annotation locStr texts mlocStr mLocCmd ->
            List.map getTextFromText texts |> String.join " "

        Syntax.InlineProblem inline ->
            Problem.inlineToString inline |> Tuple.second


textFromMarkup : List Syntax.Text -> List String
textFromMarkup texts =
    List.map getTextFromText texts
