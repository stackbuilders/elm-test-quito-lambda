module Library exposing (..)

import Browser as B
import Html as H
import Html.Events as HE
import Html.Attributes as HA
import Http
import Json.Decode as D
import Json.Encode as E
import Visible exposing (..)


-- Eff

type Eff
  = NoOp
  | GetBookList
    { url: String
    , onResult: Result Http.Error (List Book) -> Msg
    }


run: Eff -> Cmd Msg
run eff =
  case eff of
    NoOp -> Cmd.none
    GetBookList { url, onResult } ->
      Http.get
      { url = url
      , expect = Http.expectJson onResult (D.list bookDecoder)
      }


-- Model
type alias Model =
  { visible: Visible Book
  , errors: List String
  }

type AuthorName
  = Name String
  | Anonymous

type alias Book =
  { title : String
  , synopsis : String
  , author : AuthorName
  , picture : Maybe String
  }

type Msg
  = Next
  | Prev
  | GetBooks (Result Http.Error (List Book))

init : (Model, Eff)
init =(
  { errors = []
  , visible = mkVisible 3 []
  }, getBooks)

-- HTTP

getBooks : Eff
getBooks =
  GetBookList
    { url = "http://localhost:3000/books"
    , onResult = GetBooks
    }

-- JSON
bookDecoder : D.Decoder Book
bookDecoder =
  D.map4 Book
  (D.field "title" D.string)
  (D.field "synopsis" D.string)
  (D.field "author" authorNameDecoder)
  (D.field "picture" (D.nullable <| D.string))

authorNameDecoder : D.Decoder AuthorName
authorNameDecoder =
  D.oneOf
  [ D.map Name D.string
  , D.null Anonymous
  ]

bookEncoder : Book -> E.Value
bookEncoder book =
  E.object
  [ ("title", E.string book.title)
  , ("synopsis", E.string book.synopsis)
  , ("author", authorNameEncoder book.author)
  , ("picture", withEncode book.picture E.string)
  ]

withEncode : Maybe a -> (a -> E.Value) -> E.Value
withEncode m f =
  case m of
    Just a -> f a
    Nothing -> E.null

authorNameEncoder : AuthorName -> E.Value
authorNameEncoder an =
  case an of
    Name name -> E.string name
    _ -> E.null

-- View
view : Model -> H.Html Msg
view model =
  H.div
  [HA.class "library"]
  <| viewErrorsOrBooks model :: [viewActions]

viewErrorsOrBooks model =
  if List.isEmpty model.errors
  then viewBooks model
  else viewErrors model.errors

viewErrors errs =
  H.div [ HA.class "errors" ] <| List.map H.text errs

viewBooks model =
  H.div
  [ HA.class "books" ]
  <| List.map viewBook (takeVisible model.visible)

viewActions : H.Html Msg
viewActions =
    H.div
    []
    [ H.button [ HE.onClick Prev  ][ H.text "Prev"  ]
    , H.button [ HE.onClick Next  ][ H.text "Next"  ]
    ]

viewBook : Book -> H.Html Msg
viewBook book =
    H.div
    [ HA.class "book" ]
    [ H.h1 [][ H.text book.title  ]
    , H.img
      [ HA.src <| Maybe.withDefault "../assets/book.png" book.picture
      , HA.height 100
      ][]
    , H.p [][ H.text book.synopsis  ]
    , H.i [][ H.text <| "By "++ authorToString book.author  ]
    ]

authorToString : AuthorName -> String
authorToString author =
  case author of
    Name name -> name
    Anonymous -> "Anonymous"

-- Update
update : Msg -> Model -> (Model, Eff)
update msg model =
  case msg of
    Next ->
      ({ model | visible = up model.visible
      }, NoOp)
    Prev ->
      ({ model | visible = down model.visible }, NoOp)
    GetBooks (Ok books) ->
      ({ model |  visible = mkVisible 3 books }, NoOp)

    GetBooks _ ->
      ({ model | errors = ["Could not get books"], visible = mkVisible 3 [] }, NoOp)

-- Main
main : Program () Model Msg
main =
  B.element
    { init = \flags -> init |> Tuple.mapSecond run
    , update = \msg model -> update msg model |> Tuple.mapSecond run
    , view = view
    , subscriptions = \_ -> Sub.none
    }
