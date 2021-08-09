module Main exposing (..)

import Browser
import Html exposing (Html, button, div, table, td, text, tr, img)
import Html.Attributes exposing (colspan, src, style)
import Html.Events exposing (onClick)


type Dice
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Empty


type Checkmark
    = Checked
    | Unchecked


type TriBool
    = Unavailable
    | Used
    | Free


type alias Yellow =
    { one3 : Checkmark
    , one6 : Checkmark
    , one5 : Checkmark
    , two2 : Checkmark
    , two1 : Checkmark
    , two5 : Checkmark
    , three1 : Checkmark
    , three2 : Checkmark
    , three4 : Checkmark
    , four3 : Checkmark
    , four4 : Checkmark
    , four6 : Checkmark
    }


type alias Blue =
    { two : Checkmark
    , three : Checkmark
    , four : Checkmark
    , five : Checkmark
    , six : Checkmark
    , seven : Checkmark
    , eight : Checkmark
    , nine : Checkmark
    , ten : Checkmark
    , eleven : Checkmark
    , twelve : Checkmark
    }


type alias Green =
    List Checkmark


type alias Orange =
    List Dice


type alias Purple =
    List Dice


type alias Card =
    { rerolls : List TriBool
    , plusOnes : List TriBool
    , yellow : Yellow
    , blue : Blue
    , green : Green
    , orange : Orange
    , purple : Purple
    }


type UIState
    = Normal
    | ChooseOrange Int
    | ChoosePurple Int


type alias Model =
    { card : Card
    , uiState : UIState
    }


init : Model
init =
    { card =
        { rerolls = List.repeat 7 Unavailable
        , plusOnes = List.repeat 7 Unavailable
        , yellow =
            { one3 = Unchecked
            , one6 = Unchecked
            , one5 = Unchecked
            , two2 = Unchecked
            , two1 = Unchecked
            , two5 = Unchecked
            , three1 = Unchecked
            , three2 = Unchecked
            , three4 = Unchecked
            , four3 = Unchecked
            , four4 = Unchecked
            , four6 = Unchecked
            }
        , blue =
            { two = Unchecked
            , three = Unchecked
            , four = Unchecked
            , five = Unchecked
            , six = Unchecked
            , seven = Unchecked
            , eight = Unchecked
            , nine = Unchecked
            , ten = Unchecked
            , eleven = Unchecked
            , twelve = Unchecked
            }
        , green = List.repeat 11 Unchecked
        , orange = List.repeat 11 Empty
        , purple = List.repeat 11 Empty
        }
    , uiState = Normal
    }


type Msg
    = TapReroll Int
    | TapPlusOne Int
    | TapYellowOne3
    | TapYellowOne6
    | TapYellowOne5
    | TapYellowTwo2
    | TapYellowTwo1
    | TapYellowTwo5
    | TapYellowThree1
    | TapYellowThree2
    | TapYellowThree4
    | TapYellowFour3
    | TapYellowFour4
    | TapYellowFour6
    | TapBlue2
    | TapBlue3
    | TapBlue4
    | TapBlue5
    | TapBlue6
    | TapBlue7
    | TapBlue8
    | TapBlue9
    | TapBlue10
    | TapBlue11
    | TapBlue12
    | TapGreen Int
    | TapOrange Int
    | TapPurple Int
    | TapDice1
    | TapDice2
    | TapDice3
    | TapDice4
    | TapDice5
    | TapDice6
    | TapDiceClear
    | TapCancel


main =
    Browser.sandbox { init = init, update = update, view = view }


flipTriBool : TriBool -> TriBool
flipTriBool bool =
    case bool of
        Unavailable ->
            Free

        Free ->
            Used

        Used ->
            Unavailable


flipCheckmark : Checkmark -> Checkmark
flipCheckmark value =
    case value of
        Checked ->
            Unchecked

        Unchecked ->
            Checked


update : Msg -> Model -> Model
update msg model =
    case msg of
        TapReroll idx ->
            let
                oldRerolls =
                    model.card.rerolls

                newRerolls =
                    List.indexedMap
                        (\i val ->
                            if i == idx then
                                flipTriBool val

                            else
                                val
                        )
                        oldRerolls

                oldCard =
                    model.card

                newCard =
                    { oldCard | rerolls = newRerolls }
            in
            { model | card = newCard }

        TapPlusOne idx ->
            let
                oldPlusOnes =
                    model.card.plusOnes

                newPlusOnes =
                    List.indexedMap
                        (\i val ->
                            if i == idx then
                                flipTriBool val

                            else
                                val
                        )
                        oldPlusOnes

                oldCard =
                    model.card

                newCard =
                    { oldCard | plusOnes = newPlusOnes }
            in
            { model | card = newCard }

        TapYellowOne3 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | one3 = flipCheckmark model.card.yellow.one3 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowOne6 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | one6 = flipCheckmark model.card.yellow.one6 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowOne5 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | one5 = flipCheckmark model.card.yellow.one5 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowTwo2 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | two2 = flipCheckmark model.card.yellow.two2 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowTwo1 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | two1 = flipCheckmark model.card.yellow.two1 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowTwo5 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | two5 = flipCheckmark model.card.yellow.two5 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowThree1 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | three1 = flipCheckmark model.card.yellow.three1 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowThree2 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | three2 = flipCheckmark model.card.yellow.three2 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowThree4 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | three4 = flipCheckmark model.card.yellow.three4 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowFour3 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | four3 = flipCheckmark model.card.yellow.four3 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowFour4 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | four4 = flipCheckmark model.card.yellow.four4 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapYellowFour6 ->
            let
                oldYellow =
                    model.card.yellow

                newYellow =
                    { oldYellow | four6 = flipCheckmark model.card.yellow.four6 }

                oldCard =
                    model.card

                newCard =
                    { oldCard | yellow = newYellow }
            in
            { model | card = newCard }

        TapGreen idx ->
            let
                oldGreen =
                    model.card.green

                newGreen =
                    List.indexedMap
                        (\i val ->
                            if i == idx then
                                flipCheckmark val

                            else
                                val
                        )
                        oldGreen

                oldCard =
                    model.card

                newCard =
                    { oldCard | green = newGreen }
            in
            { model | card = newCard }

        TapBlue2 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | two = flipCheckmark oldBlue.two }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue3 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | three = flipCheckmark oldBlue.three }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue4 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | four = flipCheckmark oldBlue.four }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue5 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | five = flipCheckmark oldBlue.five }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue6 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | six = flipCheckmark oldBlue.six }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue7 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | seven = flipCheckmark oldBlue.seven }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue8 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | eight = flipCheckmark oldBlue.eight }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue9 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | nine = flipCheckmark oldBlue.nine }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue10 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | ten = flipCheckmark oldBlue.ten }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue11 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | eleven = flipCheckmark oldBlue.eleven }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapBlue12 ->
            let
                oldBlue =
                    model.card.blue

                newBlue =
                    { oldBlue | twelve = flipCheckmark oldBlue.twelve }

                oldCard =
                    model.card

                newCard =
                    { oldCard | blue = newBlue }
            in
            { model | card = newCard }

        TapOrange idx ->
            { model | uiState = ChooseOrange idx }

        TapDice1 ->
            case model.uiState of
                ChooseOrange idx ->
                    updateOrange idx One model
                ChoosePurple idx ->
                    updatePurple idx One model

                otherwise ->
                    model

        TapDice2 ->
            case model.uiState of
                ChooseOrange idx ->
                    updateOrange idx Two model
                ChoosePurple idx ->
                    updatePurple idx Two model

                otherwise ->
                    model

        TapDice3 ->
            case model.uiState of
                ChooseOrange idx ->
                    updateOrange idx Three model
                ChoosePurple idx ->
                    updatePurple idx Three model

                otherwise ->
                    model

        TapDice4 ->
            case model.uiState of
                ChooseOrange idx ->
                    updateOrange idx Four model
                ChoosePurple idx ->
                    updatePurple idx Four model

                otherwise ->
                    model

        TapDice5 ->
            case model.uiState of
                ChooseOrange idx ->
                    updateOrange idx Five model
                ChoosePurple idx ->
                    updatePurple idx Five model

                otherwise ->
                    model

        TapDice6 ->
            case model.uiState of
                ChooseOrange idx ->
                    updateOrange idx Six model
                ChoosePurple idx ->
                    updatePurple idx Six model

                otherwise ->
                    model

        TapDiceClear ->
            case model.uiState of
                ChooseOrange idx ->
                    updateOrange idx Empty model
                ChoosePurple idx ->
                    updatePurple idx Empty model

                otherwise ->
                    model

        TapPurple idx ->
            { model | uiState = ChoosePurple idx }

        otherwise ->
            model


updateOrange : Int -> Dice -> Model -> Model
updateOrange idx newDice model =
    let
        oldOrange =
            model.card.orange

        newOrange =
            List.indexedMap
                (\i val ->
                    if i == idx then
                        newDice

                    else
                        val
                )
                oldOrange

        oldCard =
            model.card

        newCard =
            { oldCard | orange = newOrange }
    in
    { model | card = newCard, uiState = Normal }

updatePurple : Int -> Dice -> Model -> Model
updatePurple idx newDice model =
    let
        oldPurple =
            model.card.purple

        newPurple =
            List.indexedMap
                (\i val ->
                    if i == idx then
                        newDice

                    else
                        val
                )
                oldPurple

        oldCard =
            model.card

        newCard =
            { oldCard | purple = newPurple }
    in
    { model | card = newCard, uiState = Normal }


rerollButton : TriBool -> Html Msg
rerollButton bool =
    case bool of
        Unavailable ->
            text "/Reroll/"

        Free ->
            text "🔄"

        Used ->
            text "X 🔄"


rerollCell : Int -> TriBool -> Html Msg
rerollCell idx bool =
    td [] [ button [ onClick (TapReroll idx) ] [ rerollButton bool ] ]


plusOneButton : TriBool -> Html Msg
plusOneButton bool =
    case bool of
        Unavailable ->
            text "/+1/"

        Free ->
            text "+1"

        Used ->
            text "X +1"


plusOneCell : Int -> TriBool -> Html Msg
plusOneCell idx bool =
    td [] [ button [ onClick (TapPlusOne idx) ] [ plusOneButton bool ] ]


yellowCell : String -> Checkmark -> Msg -> Html Msg
yellowCell label value msg =
    let
        txtLabel =
            case value of
                Checked ->
                    "X " ++ label

                Unchecked ->
                    label
    in
    button [ onClick msg ] [ text txtLabel ]


yellowSection : Yellow -> Html Msg
yellowSection yellow =
    table []
        [ tr []
            [ td [] [ yellowCell "3" yellow.one3 TapYellowOne3 ]
            , td [] [ yellowCell "6" yellow.one6 TapYellowOne6 ]
            , td [] [ yellowCell "5" yellow.one5 TapYellowOne5 ]
            , td [] [ text "X" ]
            ]
        , tr []
            [ td [] [ yellowCell "2" yellow.two2 TapYellowTwo2 ]
            , td [] [ yellowCell "1" yellow.two1 TapYellowTwo1 ]
            , td [] [ text "X" ]
            , td [] [ yellowCell "5" yellow.two5 TapYellowTwo5 ]
            ]
        , tr []
            [ td [] [ yellowCell "1" yellow.three1 TapYellowThree1 ]
            , td [] [ text "X" ]
            , td [] [ yellowCell "2" yellow.three2 TapYellowThree2 ]
            , td [] [ yellowCell "4" yellow.three4 TapYellowThree4 ]
            ]
        , tr []
            [ td [] [ text "X" ]
            , td [] [ yellowCell "3" yellow.four3 TapYellowFour3 ]
            , td [] [ yellowCell "4" yellow.four4 TapYellowFour4 ]
            , td [] [ yellowCell "6" yellow.four6 TapYellowFour6 ]
            ]
        ]


blueCell : String -> Checkmark -> Msg -> Html Msg
blueCell label value msg =
    let
        txtLabel =
            case value of
                Checked ->
                    "X" ++ label

                Unchecked ->
                    label
    in
    button [ onClick msg ] [ text txtLabel ]


blueSection : Blue -> Html Msg
blueSection blue =
    table []
        [ tr []
            [ td [] []
            , td [] [ blueCell "2" blue.two TapBlue2 ]
            , td [] [ blueCell "3" blue.three TapBlue3 ]
            , td [] [ blueCell "4" blue.four TapBlue4 ]
            ]
        , tr []
            [ td [] [ blueCell "5" blue.five TapBlue5 ]
            , td [] [ blueCell "6" blue.six TapBlue6 ]
            , td [] [ blueCell "7" blue.seven TapBlue7 ]
            , td [] [ blueCell "8" blue.eight TapBlue8 ]
            ]
        , tr []
            [ td [] [ blueCell "9" blue.nine TapBlue9 ]
            , td [] [ blueCell "10" blue.ten TapBlue10 ]
            , td [] [ blueCell "11" blue.eleven TapBlue11 ]
            , td [] [ blueCell "12" blue.twelve TapBlue12 ]
            ]
        ]


greenCell : String -> Checkmark -> Msg -> Html Msg
greenCell label value msg =
    let
        txtLabel =
            case value of
                Checked ->
                    "X" ++ label

                Unchecked ->
                    label
    in
    button [ onClick msg ] [ text txtLabel ]


greenSection : Green -> Html Msg
greenSection green =
    let
        labels =
            [ ">= 1"
            , ">= 2"
            , ">= 3"
            , ">= 4"
            , ">= 5"
            , ">= 1"
            , ">= 2"
            , ">= 3"
            , ">= 4"
            , ">= 5"
            , ">= 6"
            ]

        cell =
            \x y z -> greenCell x y (TapGreen z)
    in
    table []
        (List.map3 cell labels green (List.range 0 11))


orangeCell : Int -> Dice -> Html Msg
orangeCell idx dice =
    let
        mult =
            case idx of
                3 ->
                    2

                6 ->
                    2

                8 ->
                    2

                10 ->
                    3

                otherwise ->
                    1

        val =
            case dice of
                Empty ->
                    0

                One ->
                    1

                Two ->
                    2

                Three ->
                    3

                Four ->
                    4

                Five ->
                    5

                Six ->
                    6

        label =
            case dice of
                Empty ->
                    "\u{00A0}"

                -- non-breaking space
                otherwise ->
                    String.fromInt (val * mult)
    in
    td []
        [ button [ onClick (TapOrange idx) ] [ text label ]
        ]


orangeSection : Orange -> Html Msg
orangeSection orange =
    table []
        [ tr []
            (List.indexedMap orangeCell orange)
        ]


purpleCell : Int -> Dice -> Html Msg
purpleCell idx dice =
    let
        val =
            case dice of
                Empty ->
                    0

                One ->
                    1

                Two ->
                    2

                Three ->
                    3

                Four ->
                    4

                Five ->
                    5

                Six ->
                    6

        label =
            case dice of
                Empty ->
                    "\u{00A0}"

                otherwise ->
                    String.fromInt val
    in
    td []
        [ button [ onClick (TapPurple idx) ] [ text label ]
        ]



purpleSection : Purple -> Html Msg
purpleSection purple =
    table []
        [ tr []
            (List.intersperse (td [] [text "<"]) (List.indexedMap purpleCell purple))
        ]



viewNormal : Card -> Html Msg
viewNormal card =
    div []
        [ table []
            [ tr [ colspan 2 ] [ table [] [ tr [] (List.indexedMap rerollCell card.rerolls) ] ]
            , tr [ colspan 2 ] [ table [] [ tr [] (List.indexedMap plusOneCell card.plusOnes) ] ]
            , tr []
                [ td []
                    [ yellowSection card.yellow
                    ]
                , td [] [ blueSection card.blue ]
                ]
            , tr [ colspan 2 ]
                [ td []
                    [ greenSection card.green
                    ]
                ]
            , tr [ colspan 2 ]
                [ td []
                    [ orangeSection card.orange
                    ]
                ]
            , tr [ colspan 2 ]
                [ td []
                    [ purpleSection card.purple
                    ]
                ]
            ]
        ]


viewPick : Int -> Html Msg
viewPick idx =
    table []
        [ tr []
            [ td [] [ button [ onClick TapDice1 ] [ text "1" ] ]
            , td [] [ button [ onClick TapDice2 ] [ text "2" ] ]
            ]
        , tr []
            [ td [] [ button [ onClick TapDice3 ] [ text "3" ] ]
            , td [] [ button [ onClick TapDice4 ] [ text "4" ] ]
            ]
        , tr []
            [ td [] [ button [ onClick TapDice5 ] [ text "5" ] ]
            , td [] [ button [ onClick TapDice6 ] [ text "6" ] ]
            ]
        , tr [ colspan 2 ]
            [ td [] [ button [ onClick TapDiceClear ] [ text "X" ] ]
            ]
        ]


view : Model -> Html Msg
view model =
  case model.uiState of
    Normal ->
      viewNormal model.card

    ChooseOrange idx ->
      viewPick idx

    ChoosePurple idx ->
      viewPick idx
