module LibraryTest exposing (..)

import Test exposing (..)
import Expect as E
import Library as L

upSpec : Test
upSpec =
  describe "up"
  [ test "returns 1 when start is 0 and list has N elements" <|
    \_ ->
      L.up 0 [1, 2, 3, 4, 5] |> E.equal 1
  , test "returns 2 when start is 1 and list has N elements" <|
    \_ ->
      L.up 1 [1, 2, 3, 4, 5] |> E.equal 2
  , test "returns start when list is small" <|
    \_ ->
      L.up 0 [1, 2] |> E.equal 0
  ]
