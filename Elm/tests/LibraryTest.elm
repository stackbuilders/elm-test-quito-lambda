module LibraryTest exposing (..)

import Test exposing (..)
import Expect as E
import Json.Decode as JD
import Json.Encode as JE
import Library as L
import Fuzz as F
import ProgramTest as PT
import Test.Html.Selector as S
import SimulatedEffect.Cmd as SCmd
import SimulatedEffect.Http as SHttp

-- Unit test

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
  , fuzz2 F.int (F.list F.int)"up start list >= start" <|
    \start ls ->
        L.up start ls |> E.atLeast start
  ]

downSpec : Test
downSpec =
  describe "down"
  [ test "returns 0 when start is 1 and list has N elements" <|
    \_ ->
      L.down 1 |> E.equal 0
  , fuzz F.int "down start <= start" <|
    \start ->
        L.down start |> E.atMost start
  ]

-- Integration test
fixture : L.Book
fixture = L.Book "Foo" "..." L.Anonymous

app : PT.ProgramTest L.Model L.Msg L.Eff
app =
  PT.createElement
  { init = \_ -> L.init
  , view = L.view
  , update = L.update
  }
  |> PT.withSimulatedEffects runEff
  |> PT.start ()

runEff : L.Eff -> PT.SimulatedEffect L.Msg
runEff eff =
  case eff of
    L.NoOp -> SCmd.none
    L.GetBookList { url, onResult } ->
      SHttp.get
      { url = url, expect = SHttp.expectJson onResult (JD.list L.bookDecoder) }

appTest : Test
appTest =
  describe "app"
  [ test "shows prev buttons" <|
    \_ ->
      app
      |> PT.expectViewHas [ S.text "Prev" ]
  , test "shows next buttons" <|
    \_ ->
      app
      |> PT.expectViewHas [ S.text "Next" ]
  , test "shows book after loading books" <|
    \_ ->
      app
      |> PT.simulateHttpOk
        "GET"
        "http://localhost:3000/books"
        (JE.encode 0 <| JE.list L.bookEncoder [fixture])
      |> PT.expectViewHas [ S.text fixture.title ]
  , test "show newest book on next" <|
    \_ ->
      let
        books = [fixture, fixture, fixture, { fixture | title = "New book" }]
      in
        app
        |> PT.simulateHttpOk
          "GET"
          "http://localhost:3000/books"
          (JE.encode 0 <| JE.list L.bookEncoder books)
        |> PT.clickButton "Next"
        |> PT.expectViewHas [ S.text "New book" ]
  , test "show oldest book on next" <|
    \_ ->
      let
        books = [{ fixture | title = "Old book" }, fixture, fixture, fixture]
      in
        app
        |> PT.simulateHttpOk
          "GET"
          "http://localhost:3000/books"
          (JE.encode 0 <| JE.list L.bookEncoder books)
        |> PT.clickButton "Next"
        |> PT.clickButton "Prev"
        |> PT.expectViewHas [ S.text "Old book" ]
  , test "hides oldest book on next" <|
    \_ ->
      let
        books = [{ fixture | title = "Old book" }, fixture, fixture, fixture]
      in
        app
        |> PT.simulateHttpOk
          "GET"
          "http://localhost:3000/books"
          (JE.encode 0 <| JE.list L.bookEncoder books)
        |> PT.clickButton "Next"
        |> PT.expectViewHasNot [ S.text "Old book" ]
  ]
