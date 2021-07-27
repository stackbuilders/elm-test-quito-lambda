module Library exposing (..)

import Html as H
import Html.Events as HE

-- Model
type alias Model =
  { books: List Book
  , start : Int
  }

type AuthorName
  = Name String
  | Anonymous

type alias Book =
  { title : String
  , synopsis : String
  , author : AuthorName
  }

type Msg
  = Next
  | Prev


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
    [ H.button [ HE.onClick Prev  ][ H.text "Prev"  ]
    , H.button [ HE.onClick Next  ][ H.text "Next"  ]
    ]

viewBook : Book -> H.Html Msg
viewBook book =
    H.div
    []
    [ H.h1 [][ H.text book.title  ]
    , H.p [][ H.text book.synopsis  ]
    , H.i [][ H.text <| "By "++ authorToString book.author  ]
    ]

authorToString : AuthorName -> String
authorToString author =
  case author of
    Name name -> name
    Anonymous -> "Anonymous"

slice : Int -> Int -> List a -> List a
slice start end list =
  List.drop start
  <| List.take end
  <| list
