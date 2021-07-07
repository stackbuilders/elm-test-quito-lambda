module Gallery exposing (..)

import Html as H
import Browser as B

-- Model
type alias Model =
  { images: List String
  }

init = Model ["hello.jpg", "world.png"]

type Msg
  = Next
  | Prev

-- View
view : Model -> H.Html Msg
view model =
  H.div
    []
    <| List.map viewImage model.images

viewImage : String -> H.Html Msg
viewImage image =
  H.div [][ H.text image ]


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
