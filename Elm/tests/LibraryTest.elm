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

upTest : Test
upTest =
  describe "up"
  [ test "gives start = 0 and books = [1,2,3,4,5] returns 1" <|
    \_ ->
      L.up 0 [1, 2, 3, 4, 5]
      |> E.equal 1
  , test "given start = 2 and books = [1,2,3,4,5] returns 2" <|
    \_ ->
      L.up 2 [1, 2, 3, 4, 5]
      |> E.equal 2
  ]

downTest : Test
downTest =
  describe "down"
  [ test "given start = 1 returns 0" <|
    \_ ->
      L.down 1
      |> E.equal 0
  , test "given start = 0 returns 0" <|
    \_ ->
      L.down 0
      |> E.equal 0
  ]

fuzzJSON : Test
fuzzJSON =
  describe "(decode . encode) x == x"
  [ fuzz F.string "decode/encode are idempotent" <|
    \str ->
      let
        book = L.Book str str (L.Name str) Nothing
      in
        JE.encode 0 (L.bookEncoder book)
        |> JD.decodeString L.bookDecoder
        |> E.equal (Ok book)
  ]

-- Integration test

fixture : L.Book
fixture = L.Book "Foo" "..." L.Anonymous Nothing

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
