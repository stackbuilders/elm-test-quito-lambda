module Gallery exposing (..)

import Html as H
import Html.Events as HE
import Browser as B

-- Model
type alias Model =
  { books: List Book
  , shelf: Shelf
  , start : Int
  }

type Shelf
  = Shelf (List Book) Book (List Book)
  | Empty

type AuthorName
  = Name String
  | Anonymous

type alias Book =
  { title : String
  , synopsis : String
  , author : AuthorName
  }

init : Model
init =
  { books = [
    { title = "Iliada"
    , synopsis = "La batalla entre aqueos y troyanos"
    , author = Name "Homero"
    },
    { title = "Odisea"
    , synopsis = "Las aventuras de Odiseo en su camino a Ítaca desde Troya"
    , author = Name "Homero"
    },
    { title = "El libro que nunca escribí"
    , synopsis = "Habla sobre las cosas que nunca dije y las letras que no escribí"
    , author = Anonymous
    },
    { title = "La rebelión de la granja"
    , synopsis = "¿Cómo terminará la rebelión de los animales?"
    , author = Name "George Orwell"
    },
    { title = "Un mundo feliz"
    , synopsis = "El mundo ideal donde todos son felices, ¿o no?"
    , author = Name "Aldous Huxley"
    }
  ]
  , shelf = Empty
  , start = 0
  }

type Msg
  = Next
  | Prev

slice : Int -> Int -> List a -> List a
slice start end list =
  List.drop start
  <| List.take end
  <| list

-- View
view : Model -> H.Html Msg
view model =
  H.div
    []
    <| List.map viewBook (slice model.start (model.start + 3) model.books) ++ [viewActions]

viewActions : H.Html Msg
viewActions =
  H.div
    []
    [ H.button [ HE.onClick Prev ][ H.text "Prev" ]
    , H.button [ HE.onClick Next ][ H.text "Next" ]
    ]

viewBook : Book -> H.Html Msg
viewBook book =
  H.div
    []
    [ H.h1 [][ H.text book.title ]
    , H.p [][ H.text book.synopsis ]
    , H.i [][ H.text <| "By "++ authorToString book.author ]
    ]

authorToString : AuthorName -> String
authorToString author =
  case author of
    Name name -> name
    Anonymous -> "Anonymous"


-- Update
update : Msg -> Model -> Model
update msg model =
  case msg of
    Next ->
      { model | start =
          if model.start < List.length model.books - 3
          then model.start + 1
          else model.start
      }
    Prev ->
      { model | start =
          if model.start > 0
          then model.start - 1
          else model.start
      }

main : Program () Model Msg
main =
  B.sandbox
    { init = init
    , update = update
    , view = view
    }
