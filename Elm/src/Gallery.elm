module Gallery exposing (..)

import Html as H
import Browser as B

-- Model
type alias Model =
  { books: List Book
  , shelf: Shelf
  }

type Shelf
  = Shelf (Maybe Book) Book (Maybe Book)
  | Empty

type AuthorName
  = Name String
  | Anonymous

type alias Book =
  { title : String
  , sinipsis : String
  , author : AuthorName
  }

init : Model
init =
  { books = [
    { title = "Odisea"
    , sinipsis = "..."
    , author = Name "Homero"
    },
    { title = "The book I came up with"
    , sinipsis = "..."
    , author = Anonymous
    }
  ]
  , shelf = Empty
  }

type Msg
  = Next
  | Prev

-- View
view : Model -> H.Html Msg
view model =
  H.div
    []
    <| List.map viewImage model.books

viewImage : Book -> H.Html Msg
viewImage { title } =
  H.div [][ H.text title ]


-- Update
update : Msg -> Model -> Model
update msg model = model

main : Program () Model Msg
main =
  B.sandbox
    { init = init
    , update = update
    , view = view
    }
