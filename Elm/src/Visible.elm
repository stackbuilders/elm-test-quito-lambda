module Visible exposing (Visible, mkVisible, up, down, takeVisible)

type alias Visible a =
  { index: Int
  , list: List a
  , maxCount: Int
  }

mkVisible : Int -> List a -> Visible a
mkVisible count list =
  Visible 0 list count


takeVisible : Visible a -> List a
takeVisible v =
  slice v.index (v.index + v.maxCount) v.list

slice : Int -> Int -> List a -> List a
slice start end list =
  List.drop start
  <| List.take end
  <| list

up : Visible a -> Visible a
up v =
  { v | index =
      if v.index < List.length v.list - v.maxCount
      then v.index + 1
      else v.index
  }

down : Visible a -> Visible a
down v =
  { v | index =
      if v.index > 0
      then v.index - 1
      else v.index
  }
