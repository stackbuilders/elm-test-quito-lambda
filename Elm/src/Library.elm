module Library exposing (..)

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
  , start = 0
  }

type Msg
  = Next
  | Prev
