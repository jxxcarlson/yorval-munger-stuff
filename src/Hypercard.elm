module Hypercard exposing (Format, HyperCardMsg(..), viewSection)

{-| -}

import ASTTools
import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Parse.Syntax as Syntax exposing (Command, Divert, Element(..), Text(..), Value(..))
import Camperdown.Problem as Problem
import Element as E exposing (alignTop, column, el, fill, height, htmlAttribute, moveDown, paddingEach, paddingXY, paragraph, px, rgb255, row, spacing, spacingXY, width, wrappedRow)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes
import List.Extra
import Maybe.Extra



-- WHAT IS EXPOSED


viewSection : Format -> String -> Maybe Syntax.Section -> E.Element HyperCardMsg
viewSection format source maybeSection =
    case maybeSection of
        Nothing ->
            E.text "No section to show"

        Just section ->
            viewSection_ format source section


type HyperCardMsg
    = GoToSection String
    | NoMsg


type alias Format =
    -- Units = pixels
    { imageHeight : Int
    , lineWidth : Int
    , leftPadding : Int
    , bottomPadding : Int
    , topPadding : Int
    }



-- THE REST
-- Formatting


formatPadding : Format -> E.Attribute msg
formatPadding format =
    E.paddingEach { left = format.leftPadding, right = 0, top = 0, bottom = format.bottomPadding }


monospace : E.Attribute msg
monospace =
    Font.family [ Font.typeface "Source Code Pro", Font.monospace ]


sans : E.Attribute msg
sans =
    Font.family [ Font.typeface "Soleil", Font.typeface "Arial", Font.sansSerif ]



-- WALKING THROUGH THE AST


viewSection_ : Format -> String -> Syntax.Section -> E.Element HyperCardMsg
viewSection_ format source { level, contents, label } =
    let
        title =
            case label of
                Syntax.Named ( _, s ) ->
                    s

                Syntax.Anonymous n ->
                    "(Passage beginning on line " ++ String.fromInt n ++ ")"

        attrs =
            styleOfLevel level
    in
    column [ width fill ]
        [ el (paddingBelow :: attrs) (E.text title)
        , paragraph [ sans ] [ viewElements format "nada" source contents ]
        ]


viewDivert : Format -> String -> String -> Divert -> E.Element HyperCardMsg
viewDivert format commandName_ source divert =
    case divert of
        Syntax.Nested elems ->
            case commandName_ of
                "row" ->
                    viewElements format commandName_ source elems

                "code" ->
                    let
                        rawCode =
                            List.head (List.map (ASTTools.getText >> List.map ASTTools.getRaw >> Maybe.Extra.values) elems) |> Maybe.withDefault []
                    in
                    column
                        [ Font.color (rgb255 101 13 209)
                        , monospace
                        , Font.size 15
                        , spacing 8
                        , paddingEach { top = 18, bottom = 18, left = 18, right = 0 }
                        , htmlAttribute (Html.Attributes.style "white-space" "pre")
                        ]
                        (List.map E.text rawCode)

                _ ->
                    row [ formatPadding format, width fill ]
                        [ el [ width fill, formatPadding { format | leftPadding = 2 * format.leftPadding } ] (viewElements format commandName_ source elems)
                        ]

        Syntax.Immediate elems ->
            row [ formatPadding format, width fill ]
                [ el [ Background.color (rgb255 245 245 245), width fill, formatPadding format ] (viewElements format commandName_ source elems)
                ]

        Syntax.Reference locString ->
            el [] (E.text <| Loc.value locString)


viewElements : Format -> String -> String -> List Syntax.Element -> E.Element HyperCardMsg
viewElements format commandName_ source elements =
    case commandName_ of
        "row" ->
            row [ width (px format.lineWidth) ] <| List.map (\element_ -> viewElement format 0 commandName_ source element_) elements

        _ ->
            column [ width (px format.lineWidth) ] <| List.indexedMap (\k element_ -> viewElement format k commandName_ source element_) elements


itemSymbol : Int -> String -> E.Element msg
itemSymbol k commandName_ =
    case commandName_ of
        "list" ->
            el [ Font.bold, alignTop, moveDown 3 ] (E.text "•")

        "numbered" ->
            el [ Font.size 15, alignTop, moveDown 3 ] (E.text (String.fromInt k ++ "."))

        _ ->
            E.none


viewElement : Format -> Int -> String -> String -> Syntax.Element -> E.Element HyperCardMsg
viewElement format k commandName_ source elem =
    case elem of
        Syntax.Paragraph contents ->
            case commandName_ of
                "code" ->
                    column [ width fill, paddingEach { top = 3, bottom = 8, left = 0, right = 0 } ]
                        [ column [ width fill, spacingXY 6 3 ]
                            (List.concat <| List.map (viewText format (paragraphStyle commandName_) baseStyle) contents.contents)
                        ]

                _ ->
                    row [ width fill, paddingEach { top = 3, bottom = 8, left = 0, right = 0 } ]
                        [ wrappedRow [ width fill, spacingXY 6 3 ]
                            (List.concat <| List.map (viewText format (paragraphStyle commandName_) baseStyle) contents.contents)
                        ]

        Syntax.Preformatted { contents } ->
            let
                -- pfPadding is computed according to whether there are following newlines.
                -- The idea is to provide a weakened facsimile of the original format.
                pfPadding =
                    if String.right 1 contents == "\n" then
                        { top = 0, bottom = format.bottomPadding, left = 0, right = 0 }

                    else
                        { top = 0, bottom = 0, left = 0, right = 0 }
            in
            -- row [ width fill, paddingEach { top = format.topPadding, bottom = 0, left = 0, right = 0 } ]
            row [ width fill, paddingEach pfPadding ]
                [ el
                    [ monospace
                    , Font.size 15
                    , Font.color codeColor
                    , htmlAttribute (Html.Attributes.style "white-space" "pre")
                    ]
                    (E.text <| contents)
                ]

        Syntax.Item { children } ->
            row [ width fill ]
                [ el [ height fill ] E.none
                , column [ width fill ]
                    [ row [ formatPadding format, width fill, spacing 8 ]
                        [ itemSymbol (k + 1) commandName_
                        , el [ Background.color (rgb255 245 245 245), width fill ] (viewElements format commandName_ source children)
                        ]
                    ]
                ]

        Syntax.Command { lines, mark, command, child } ->
            row [ width fill ]
                [ column [ width fill ]
                    [ viewCommand format command
                    , case child of
                        Nothing ->
                            E.none

                        Just divert ->
                            viewDivert format (ASTTools.commandString command) source divert
                    ]
                ]

        Syntax.Problem { loc, problem, lines } ->
            E.none


viewCommand : Format -> Command -> E.Element HyperCardMsg
viewCommand format ( mLocStr, config ) =
    case mLocStr of
        Nothing ->
            E.none

        Just locStr ->
            let
                commandName =
                    Loc.value locStr

                args =
                    ASTTools.stringListOfConfig config

                arg k =
                    List.Extra.getAt k args |> Maybe.withDefault "noArg"
            in
            case commandName of
                "image" ->
                    E.image [ width fill, height (px format.imageHeight) ] { src = arg 0, description = "Image" }

                _ ->
                    E.text commandName


viewValue : Format -> List (E.Attribute msg) -> Loc Value -> E.Element HyperCardMsg
viewValue format attr val =
    case val of
        ( _, Syntax.Markup markup ) ->
            el [ paddingXY 0 2, width fill ] <|
                -- wrappedRow ([ spacingXY 3 6, Background.color (rgb255 245 245 245), width fill, padding 3 ] ++ attr)
                wrappedRow []
                    (List.concat <| List.map (viewText format attr baseStyle) markup)

        _ ->
            el (attr ++ [ Font.size 15, monospace ]) (E.text <| valueToString val) |> E.map (\_ -> NoMsg)


{-| TODO: think about the below re Rob's remark:
--I think we'll want to use a deeper transformation here, producing a single Element
--rather than a list of elements that then get paragraph-concatenated together with
--spacing, in order to have an actually satisfying markup viewer.'
-}
viewText : Format -> List (E.Attribute msg) -> Style -> Text -> List (E.Element HyperCardMsg)
viewText format attr style elem =
    case elem of
        Syntax.Raw str ->
            List.map (\word -> el (styleAttributes style ++ attr) (E.text word)) (String.split " " str |> List.filter (\s -> String.trim s /= "")) |> List.map (E.map (\_ -> NoMsg))

        --TODO: re the below:
        --I'd just turn inline verbatim comments into preformatted text sections.
        --This'll probably(???) remove the need for RandomColor in the ViewHyperCard module.
        Syntax.Verbatim ch ( _, str ) ->
            let
                newStyle =
                    styleOfMark (String.fromChar ch) baseStyle

                attr_ =
                    styleAttributes newStyle
            in
            [ row [ monospace, height fill, Font.size 15 ]
                [ el ([ height fill, paddingEach { top = 0, bottom = 0, left = 4, right = 4 } ] ++ attr_) (E.text str)
                ]
            ]

        Syntax.Annotation locStr texts mLocString mLocCommand ->
            let
                label =
                    List.map ASTTools.getTextFromText texts |> List.head |> Maybe.withDefault "LABEL"
            in
            case mLocCommand of
                Nothing ->
                    [ E.none ]

                Just locCommand ->
                    case Loc.value locCommand |> commandData of
                        Nothing ->
                            [ E.none ]

                        Just data ->
                            case data.name of
                                "link" ->
                                    [ E.link [ E.moveDown 1.5, Font.size 14, funnyLinkColor ] { url = getArg 0 data.args, label = el [] (E.text label) } ]

                                _ ->
                                    [ E.none ]

        Syntax.InlineProblem inline ->
            [ el [] (E.text (Problem.inlineToString inline |> Tuple.second)) ]


viewInlineProblem : Problem.Inline -> List (E.Element HyperCardMsg)
viewInlineProblem problem =
    [ row [ Font.size 15, height fill ]
        (el [ height fill, width (px 1) ] E.none
            :: el [ height fill, width (px 4) ] E.none
            :: (case problem of
                    _ ->
                        let
                            ( loc, str ) =
                                Problem.inlineToString problem
                        in
                        [ E.text <| str ++ " (Line " ++ String.fromInt loc.start.line ++ ", position " ++ String.fromInt loc.start.column ++ ".)" ]
               )
            ++ [ el [ height fill, width (px 4) ] E.none
               , el [ height fill, width (px 1) ] E.none
               ]
        )
    ]



-- HELPERS


unquote : String -> String
unquote str =
    String.replace (String.fromChar '"') "" str


valueToString : Loc Value -> String
valueToString v =
    case Loc.value v of
        Variable s ->
            s

        String s ->
            "\"" ++ s ++ "\""

        Int i ->
            String.fromInt i

        Markup _ ->
            "[(markup)]"


valuesToString : List (Loc Value) -> String
valuesToString vs =
    List.map valueToString vs
        |> List.intersperse " "
        |> String.concat


filteredCommandName commandName =
    if commandName == "(E.none)" then
        E.none

    else
        el [ Font.size 15, monospace, Font.bold ] (E.text <| commandName)



-- MORE FORMAT STUFF


type alias Style =
    { bold : Bool
    , strike : Bool
    , italic : Bool
    , under : Bool
    , code : Bool
    }


baseStyle : Style
baseStyle =
    { bold = False
    , strike = False
    , italic = False
    , under = False
    , code = False
    }


{-| TODO: why is this not rendering inline code in codeColor?
-}
styleAttributes : Style -> List (E.Attribute msg)
styleAttributes { bold, strike, italic, under, code } =
    (if bold then
        [ Font.bold ]

     else
        []
    )
        ++ (if strike then
                [ Font.strike ]

            else
                []
           )
        ++ (if italic then
                [ Font.italic ]

            else
                []
           )
        ++ (if under then
                [ Font.underline ]

            else
                []
           )
        ++ (if code then
                [ Font.color codeColor ]

            else
                []
           )
        ++ [ sans, Font.size 15 ]


getArg : Int -> List String -> String
getArg k args =
    List.Extra.getAt k args |> Maybe.withDefault "noArg"


commandData : Command -> Maybe { name : String, args : List String }
commandData ( mLocStr, config ) =
    case mLocStr of
        Nothing ->
            Nothing

        Just locStr ->
            let
                commandName =
                    Loc.value locStr

                args =
                    ASTTools.stringListOfConfig config
            in
            Just { name = commandName, args = args }


funnyLinkColor =
    Font.color (rgb255 167 54 247)



-- HELPERS, added by JC


codeColor =
    E.rgb255 104 53 181


paragraphStyle : String -> List (E.Attribute msg)
paragraphStyle commandName_ =
    case commandName_ of
        "quote" ->
            [ Font.italic ]

        "code" ->
            [ monospace, Font.size 15, htmlAttribute (Html.Attributes.style "white-space" "pre") ]

        _ ->
            []


styleOfMark : String -> Style -> Style
styleOfMark mark style =
    case mark of
        "*" ->
            { style | bold = True }

        "_" ->
            { style | italic = True }

        "~" ->
            { style | strike = True }

        "`" ->
            { style | code = True }

        _ ->
            style


styleOfLevel : Int -> List (E.Attribute msg)
styleOfLevel k =
    case k of
        1 ->
            -- original 18
            [ Font.size 20, Font.bold, paddingEach { top = 10, bottom = 5, left = 0, right = 0 } ]

        2 ->
            -- original 17
            [ Font.size 18, Font.bold, Font.italic, paddingEach { top = 9, bottom = 4, left = 0, right = 0 } ]

        3 ->
            [ Font.size 16, Font.bold, Font.italic, Font.color (rgb255 50 50 50), paddingEach { top = 8, bottom = 4, left = 0, right = 0 } ]

        _ ->
            [ Font.size 16, Font.bold, Font.italic, Font.color (rgb255 100 100 100), paddingEach { top = 8, bottom = 4, left = 0, right = 0 } ]


paddingBelow =
    paddingEach { top = 0, bottom = 18, right = 0, left = 0 }
