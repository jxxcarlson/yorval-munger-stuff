module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import ASTTools
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Camperdown.Config.Config as Config
import Camperdown.Parse
import Camperdown.Parse.Syntax exposing (Document, Label(..), Section)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Hypercard as HyperCard exposing (HyperCardMsg(..))
import Task
import Url exposing (Url)
import Markdown
import Docs


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { input : String
    , doc : Document
    , currentSection : Maybe Section
    , key : Key
    , url : Url
    , viewMode : ViewMode
    }


type Msg
    = NoOp
    | UrlClicked UrlRequest
    | UrlChanged Url
    | InputText String
    | HyperCard HyperCardMsg
    | ShowSource
    | HideSource
    | About
    | Dummy String
    | HyperCardRequested
    | HyperCardLoaded File
    | HyperCardLoad String


type ViewMode
    = ViewWithSource
    | ViewDefault
    | ViewAbout


type alias Flags =
    {}


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        doc =
            Camperdown.Parse.parse Config.config sourceText
    in
    ( { key = key
      , url = url
      , input = sourceText
      , doc = doc
      , currentSection = ASTTools.findSectionByLabel "The beginning" doc.sections
      , viewMode = ViewDefault
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputText str ->
            ( { model | input = str, viewMode = ViewWithSource }, Cmd.none )

        HyperCard hypercardMsg ->
            case hypercardMsg of
                GoToSection destination ->
                    ( { model | currentSection = ASTTools.findSectionByLabel destination model.doc.sections }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( { model | currentSection = ASTTools.findSectionByLabel (Debug.log "PATH" (String.dropLeft 1 url.path)) model.doc.sections }, Cmd.none )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        ShowSource ->
            ( { model | viewMode = ViewWithSource }, Cmd.none )

        HideSource ->
            ( { model | viewMode = ViewDefault }, Cmd.none )

        About -> ({model | viewMode = ViewAbout}, Cmd.none)

        Dummy _ ->
            ( { model | viewMode = ViewDefault }, Cmd.none )

        HyperCardRequested ->
            ( model, Select.file [ "text/plain" ] HyperCardLoaded )

        HyperCardLoaded file ->
            ( model, Task.perform HyperCardLoad (File.toString file) )

        HyperCardLoad str ->
            let
                doc =
                    Camperdown.Parse.parse Config.config str
            in
            ( { model | input = str, doc = doc, currentSection = ASTTools.findSectionByLabel "The beginning" doc.sections }, Cmd.none )



-- HELPERS


requestHyperCard : Cmd Msg
requestHyperCard =
    Select.file [ "text/plain" ] HyperCardLoaded



--
-- VIEW
--


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Browser.Document Msg
view model =
    { title = "HyperCard Demo"
    , body = [ Element.layout [ bgGray 0.2, centerX, centerY ] (mainColumn model) ]
    }


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 36, width (px 1400), height (px 800)]
            [ row [ spacing 12, centerX, centerY ] [ 
            el [Font.size 24] (text "Camperdown-Hypercard Demo")
            , showSourceButton model.viewMode, requestFileButton , aboutButton model.viewMode]
            , case model.viewMode of
                ViewDefault ->
                    viewHyperCard model

                ViewWithSource ->
                    viewWithSourceAndHypercard model

                ViewAbout -> 
                   viewAbout model
            ]
        ]

viewAbout model =
  column [Font.size 14, width (px 500), height (px 600), scrollbarY, centerX, centerY] [ 
     Markdown.toHtml [] Docs.aboutDoc |> Element.html
     ]

viewWithSourceAndHypercard model =
    row [ width fill, spacing 12, centerX, centerY ]
        [ Input.multiline [ scrollbarY, height (px 580), alignTop, Font.size 14 ] { onChange = Dummy, text = model.input, placeholder = Nothing, label = Input.labelHidden "", spellcheck = False }
        , HyperCard.viewSection format sourceText model.currentSection |> Element.map HyperCard
        ]


viewHyperCard model =
    row [ alignTop, centerX, centerY ] [ HyperCard.viewSection format sourceText model.currentSection |> Element.map HyperCard ]


format =
    { imageHeight = 500
    , lineWidth = 600
    , leftPadding = 20
    , bottomPadding = 10
    , topPadding = 10
    }


title : String -> Element msg
title str =
    row [ centerX, Font.bold, fontGray 0.9 ] [ text str ]


inputText : Model -> Element Msg
inputText model =
    Input.text []
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Input")
        }


showSourceButton : ViewMode -> Element Msg
showSourceButton viewMode =
    case viewMode of
        ViewDefault ->
            row []
                [ Input.button buttonStyle
                    { onPress = Just ShowSource
                    , label = el [ centerX, centerY ] (text "Show Source")
                    }
                ]

        ViewWithSource ->
            row []
                [ Input.button buttonStyle
                    { onPress = Just HideSource
                    , label = el [ centerX, centerY ] (text "Hide Source")
                    }
                ]

        ViewAbout -> 
          Element.none


requestFileButton : Element Msg
requestFileButton =
    row []
        [ Input.button buttonStyle
            { onPress = Just HyperCardRequested
            , label = el [ centerX, centerY ] (text "Open file")
            }
        ]


aboutButton : ViewMode -> Element Msg
aboutButton viewMode =
  case viewMode of 
    ViewAbout -> row []
            [ Input.button buttonStyle
                { onPress = Just HideSource
                , label = el [ centerX, centerY ] (text "Hypercard")
                }
            ]
    _ -> 
        row []
            [ Input.button buttonStyle
                { onPress = Just About
                , label = el [ centerX, centerY ] (text "About")
                }
            ]

--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 1.0
    , paddingXY 20 0
    , width (px 1400), height (px 800)
    ]


buttonStyle =
    [ Background.color (Element.rgb 0.5 0.5 0.5)
    , Font.color (rgb255 255 255 255)
    , Font.size 14
    , paddingXY 15 8
    ]



-- DATA


sourceText =
    """

# The beginning

Go around Europe!

! image "https://europa.eu/european-union/sites/default/files/easy_to_read/european-map_en.jpg"

You start in Dover, and hop aboard the Chunnel
to go to [France](link "france-entry").

# france-entry

Welcome to France!

! image "https://upload.wikimedia.org/wikipedia/en/c/c3/Flag_of_France.svg"

From here you can
[eat a baguette](link "france-munch"), or you can get on
the train to [Italy](link "italy-entry" ), or you can
take a hot air balloon to [Germany](link "germany-hot-air").



# germany-hot-air

! image "https://artprojectsforkids.org/wp-content/uploads/2018/02/Hot-Air-Balloon-1.jpg"

You are entering Germany by hot air balloon!

Your only option now is to go to [Italy](link "italy-entry").

# france-munch

Mmm tasty baguette.

! image "https://images.food52.com/svbmUpu4t64ynw5tCXQ0b1aKW6I=/71f851b1-f927-4fd5-a0c9-c418e74f428c--13408906735_82b0d0499e_b.jpg"

From here you can get on
the train to [Italy](link "italy-entry" ), or you can
take a hot air balloon to [Germany](link "germany-hot-air").

# italy-entry

Welcome to Italy!

! image "https://foodwineclick.files.wordpress.com/2016/09/winestudio_amatriciana_umbria-20160830-65.jpg?w=640"

The wine, pasta, and conversation were too good ...  [time for a nap](link "naptime").

# naptime

! image "https://i1.wp.com/katzenworld.co.uk/wp-content/uploads/2020/03/close-up-photography-of-gray-tabby-cat-sleeping-on-yellow-1440918.jpg?fit=1020%2C680&ssl=1"

Italy is a good place to take a nap. We hope
you had a pleasant journey.
"""
