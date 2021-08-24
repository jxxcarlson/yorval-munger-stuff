module Docs exposing (aboutDoc)

aboutDoc = """

# A HyperCard App in Camperdown

Way back when, in 1987, Apple released [Hypercard](https://en.wikipedia.org/wiki/HyperCard), an app that
presented the user with a "deck of cards" containing text,
images and other media, as well as links to other cards in
the deck. Clicking on such a link would bring the target
card into view.

![](https://cdn.arstechnica.net//wp-content/uploads/2012/05/HyperCardbird-e1338220256722.jpg)

Our goal here is to describe how a hypercard-like app can
be implemented using Camperdown. To see the app in action,
select *Tour* in the *Document* menu of this app and select
*Hypercard* in the *View Mode* menu to view *Tour* as a
Hypercard deck.

For the implementation, consider the general structure
of a typical Camperdown app. This will be an Elm app that
reads a document written in Camperdown markup, parses it,
stores the AST in the model, and then renders the AST in
using its view function. The app you are using right now
is a species of this general structure in which the entire
AST is rendered with headings, bold and italic text,
links and images, but with no active elements that require
interaction with the update function. In a Camperdown
HyperCard app, one displays just one section of the document
at a time. Each section or "card," contains one or more
named links, each pointing to some other section of the
document. When the user clicks on a link, the "target"
card is displayed.

The app, which relies on the Elm `brilliantorg/backpacker-below"`
package, is quite slim. The `Hypercard` module (360 loc) is the 
interface between the app code in `Main` (260 loc). It makes use 
of a small module, `ASTTools` (80 loc) that manipulates
the AST obtained by parsing a Hypercard source file.
That's it!

## Implementation

A Camperdown document has a _prelude_ and a bunch of _sections_:

```
This is the prelude.  It is optional
and consists of whatever comes before the
first section

# First section
Stuff

# Second section
More stuff
```

Parsing the text yields a value of type `Document`, where

```
type alias Document =
    { prelude : List Element
    , sections : List Section
    }
```

and where

```
type alias Section =
    { level : Int
    , contents : List Element
    , label : Label
    }
```

In order to work with a homogeneous type, we assume that
a Camperdown Hypercard document has an empty prelude. When
such a document is loaded or modified in the editor, the
content is parsed and stored as the `currentDocument`
field of the model, with `currentSection` set to the head
of the list of sections.

```
type alias Model =
   {  contents : String
    , currentDocument : Maybe Document
    , currentSection : Maybe Section
    ...
   }
```

The current section is displayed to the user via function
`viewSection` in module `ViewHyperCard.` As an example,
consider the *Tour* document:

```
    # The beginning

    Go around Europe!

    You start in Dover, and hop aboard the Chunnel
    to go to [France](go "france-entry" ).

    # france-entry

    Welcome to France!

    ...
```

When this document is opened, the section _The beginning_
is displayed with a link `France.` Referring to the source
code, we find the annotation `[France](go "france-entry" )`
with a following command. The command has name `go` and
arguument `#france-entry"`. The intent is for a click on
this link to cause the section entitled `france-entry` to
be displayed. The main work, then, is implementing this command.

## The go command

The `go` command will be implemented as part of the view
function defined in module `ViewHyperCard.` As a starting
point for writing `ViewHyperCard,` we take the module
`ViewMarkup` which is used to render an entire document
much as if it were a Markdown document. The first task,
then, is to find the part of the code where we can handle
the go command. To that end, we search our newly renamed
copy of `ViewMarkup` for `Annotation.` There is just one
hit, as a clause of `viewText.` There we find the code

```
( _, ( ( start, end, cmd ), ( arguments, parameters ) ) ) =
        annotation
```

The `cmd` snippet looks promising, especially in view of
code we find a bit further down.

```
case cmd of
   Just "link" ->
      [ newTabLink
        [ Font.color (Element.rgb 0 0 0.8) ]
        { url = arg, label = el [] (text label) } ]

   _ ->
      List.concat <| List.map (viewText attr newStyle)
         Loc.value markup)
```

So it seems that we should add some code like this:

```
Just "go" ->
       [ goToSectionButton arg label ]
```

We confirm the wisdom of this choice by stubbing out the
function `goToSectionButton` with some `Debug.log` statements
to ensure that we really do capture the correct values
with `arg` and `label.` The finished code the button is

```
goToSectionButton arg label =
      Input.button []
        { onPress = Just (GoToSection arg),
          label = el [ ... ] (text label) }
```

where `arg` is the name of the destination section. The
real action occurs is in the clause `GoToSection arg ->`
of the `update` function:

```
HyperCard hyperCardMsg ->
      case hyperCardMsg of
         GoToSection dest ->
            case model.currentDocument of
               Nothing -> ( model, Cmd.none )
               Just doc ->
                  ( { model | currentSection =
                      findSectionByLabel dest doc.sections }
                    , Cmd.none )
```

Burrowing into the code still further, we see that the
heart of the matter is the function call

```findSectionByLabel dest doc.sections```

Its role is to extract from a list of sections a section
with given name:

```findSectionByLabel : String -> List Section -> Maybe Section```

This is accomplished by a simple application of `List.filter:`

```
findSectionByLabel label_ sections =
      List.filter (\\sec -> getLabel sec == label_) sections
        |> List.head
```
where

```
getLabel { label } =
    case label of
        Camp.Named ( _, s ) ->
            s

        Camp.Anonymous n ->
            String.fromInt n
```


"""