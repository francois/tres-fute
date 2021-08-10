module Main exposing (..)

import Browser
import Html exposing (Html, button, col, div, img, span, table, td, text, tr)
import Html.Attributes exposing (colspan, rowspan, src, style, width)
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
    { turns : List Bool
    , rerolls : List TriBool
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


type alias Score =
    { yellow : Int
    , blue : Int
    , green : Int
    , orange : Int
    , purple : Int
    , foxes : Int
    , total : Int
    }


scoreYellow : Yellow -> Int
scoreYellow yellow =
    let
        col1 =
            if yellow.one3 == Checked && yellow.two2 == Checked && yellow.three1 == Checked then
                10

            else
                0

        col2 =
            if yellow.one6 == Checked && yellow.two1 == Checked && yellow.four3 == Checked then
                14

            else
                0

        col3 =
            if yellow.one5 == Checked && yellow.three2 == Checked && yellow.four4 == Checked then
                16

            else
                0

        col4 =
            if yellow.two5 == Checked && yellow.three4 == Checked && yellow.four6 == Checked then
                20

            else
                0
    in
    col1 + col2 + col3 + col4


scoreBlue : Blue -> Int
scoreBlue blue =
    let
        list =
            [ blue.two, blue.three, blue.four, blue.five, blue.six, blue.seven, blue.eight, blue.nine, blue.ten, blue.eleven, blue.twelve ]

        checked =
            List.filter (\x -> x == Checked) list

        numChecked =
            List.length checked
    in
    case numChecked of
        1 ->
            1

        2 ->
            2

        3 ->
            4

        4 ->
            7

        5 ->
            11

        6 ->
            16

        7 ->
            22

        8 ->
            29

        9 ->
            37

        10 ->
            46

        11 ->
            56

        otherwise ->
            0


scoreGreen : Green -> Int
scoreGreen green =
    let
        checked =
            List.filter (\x -> x == Checked) green

        numChecked =
            List.length checked
    in
    case numChecked of
        1 ->
            1

        2 ->
            3

        3 ->
            6

        4 ->
            10

        5 ->
            15

        6 ->
            21

        7 ->
            28

        8 ->
            36

        9 ->
            45

        10 ->
            55

        11 ->
            66

        otherwise ->
            0


diceToFace : Dice -> Int
diceToFace dice =
    case dice of
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

        Empty ->
            0


scoreOrange : Orange -> Int
scoreOrange orange =
    let
        faces =
            List.map diceToFace orange

        points =
            List.indexedMap (\i val -> val * orangeIndexMultipler i) faces
    in
    List.sum points


scorePurple : Purple -> Int
scorePurple purple =
    let
        faces =
            List.map diceToFace purple
    in
    List.sum faces


yellowFox : Yellow -> Int
yellowFox yellow =
    if yellow.four3 == Checked && yellow.four4 == Checked && yellow.four6 == Checked then
        1

    else
        0


blueFox : Blue -> Int
blueFox blue =
    if blue.nine == Checked && blue.ten == Checked && blue.eleven == Checked && blue.twelve == Checked then
        1

    else
        0


greenFox : Green -> Int
greenFox green =
    let
        numChecked =
            List.length (List.filter (\x -> x == Checked) green)
    in
    if numChecked >= 7 then
        1

    else
        0


orangeFox : Orange -> Int
orangeFox orange =
    let
        numFilled =
            List.length (List.filter (\x -> x /= Empty) orange)
    in
    if numFilled >= 8 then
        1

    else
        0


purpleFox : Purple -> Int
purpleFox purple =
    let
        numFilled =
            List.length (List.filter (\x -> x /= Empty) purple)
    in
    if numFilled >= 7 then
        1

    else
        0


scoreOf : Card -> Score
scoreOf card =
    let
        yellowScore =
            scoreYellow card.yellow

        blueScore =
            scoreBlue card.blue

        greenScore =
            scoreGreen card.green

        orangeScore =
            scoreOrange card.orange

        purpleScore =
            scorePurple card.purple

        numFoxes =
            List.sum
                [ yellowFox card.yellow
                , blueFox card.blue
                , greenFox card.green
                , orangeFox card.orange
                , purpleFox card.purple
                ]

        minScore =
            case List.minimum [ yellowScore, blueScore, greenScore, orangeScore, purpleScore ] of
                Just score ->
                    score

                Nothing ->
                    0

        foxScore =
            numFoxes * minScore

        totalScore =
            List.sum [ yellowScore, blueScore, greenScore, orangeScore, purpleScore, foxScore ]
    in
    { yellow = yellowScore
    , blue = blueScore
    , green = greenScore
    , orange = orangeScore
    , purple = purpleScore
    , foxes = foxScore
    , total = totalScore
    }


init : Model
init =
    { card =
        { rerolls = List.repeat 7 Unavailable
        , plusOnes = List.repeat 7 Unavailable
        , turns = List.repeat 6 False
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
    | TapTurn Int
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
        TapTurn idx ->
            let
                oldTurns =
                    model.card.turns

                newTurns =
                    List.indexedMap
                        (\i val ->
                            if i == idx then
                                not val

                            else
                                val
                        )
                        oldTurns

                oldCard =
                    model.card

                newCard =
                    { oldCard | turns = newTurns }
            in
            { model | card = newCard }

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


turnButton : Int -> Bool -> Html Msg
turnButton idx state =
    case state of
        True ->
            text ("X" ++ String.fromInt idx)

        False ->
            text (String.fromInt idx)


turnCell : Int -> Bool -> Html Msg
turnCell idx state =
    td []
        [ htmlButton (TapTurn idx) [ turnButton (idx + 1) state ]
        ]


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
    td []
        [ htmlButton
            (TapReroll idx)
            [ rerollButton bool ]
        ]


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
    td [] [ htmlButton (TapPlusOne idx) [ plusOneButton bool ] ]


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
    textButton msg txtLabel


yellowSection : Yellow -> Html Msg
yellowSection yellow =
    table [ style "text-align" "center" ]
        [ tr []
            [ td [] [ yellowCell "3" yellow.one3 TapYellowOne3 ]
            , td [] [ yellowCell "6" yellow.one6 TapYellowOne6 ]
            , td [] [ yellowCell "5" yellow.one5 TapYellowOne5 ]
            , td [] [ text "X" ]
            , bonusCell BonusBlue
            ]
        , tr []
            [ td [] [ yellowCell "2" yellow.two2 TapYellowTwo2 ]
            , td [] [ yellowCell "1" yellow.two1 TapYellowTwo1 ]
            , td [] [ text "X" ]
            , td [] [ yellowCell "5" yellow.two5 TapYellowTwo5 ]
            , bonusCell (BonusOrange 4)
            ]
        , tr []
            [ td [] [ yellowCell "1" yellow.three1 TapYellowThree1 ]
            , td [] [ text "X" ]
            , td [] [ yellowCell "2" yellow.three2 TapYellowThree2 ]
            , td [] [ yellowCell "4" yellow.three4 TapYellowThree4 ]
            , bonusCell BonusGreen
            ]
        , tr []
            [ td [] [ text "X" ]
            , td [] [ yellowCell "3" yellow.four3 TapYellowFour3 ]
            , td [] [ yellowCell "4" yellow.four4 TapYellowFour4 ]
            , td [] [ yellowCell "6" yellow.four6 TapYellowFour6 ]
            , bonusCell BonusFox
            ]
        , tr []
            [ td [ style "font-size" "20px", style "font-weight" "bold" ] [ text "10" ]
            , td [ style "font-size" "20px", style "font-weight" "bold" ] [ text "14" ]
            , td [ style "font-size" "20px", style "font-weight" "bold" ] [ text "16" ]
            , td [ style "font-size" "20px", style "font-weight" "bold" ] [ text "20" ]
            , bonusCell BonusPlusOne
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
    textButton msg txtLabel


blueSection : Blue -> Html Msg
blueSection blue =
    table []
        [ tr []
            [ td [ colspan 4 ]
                [ table [ style "width" "100%", style "text-align" "right", style "color" "white", style "font-weight" "bold", style "font-size" "20px" ]
                    [ tr []
                        [ td [] [ text "1" ]
                        , td [] [ text "2" ]
                        , td [] [ text "4" ]
                        , td [] [ text "7" ]
                        , td [] [ text "11" ]
                        , td [] [ text "16" ]
                        , td [] [ text "22" ]
                        , td [] [ text "29" ]
                        , td [] [ text "37" ]
                        , td [] [ text "46" ]
                        , td [] [ text "56" ]
                        ]
                    , tr []
                        [ td [] [ text "1" ]
                        , td [] [ text "2" ]
                        , td [] [ text "3" ]
                        , td [] [ text "4" ]
                        , td [] [ text "5" ]
                        , td [] [ text "6" ]
                        , td [] [ text "7" ]
                        , td [] [ text "8" ]
                        , td [] [ text "9" ]
                        , td [] [ text "10" ]
                        , td [] [ text "11" ]
                        ]
                    ]
                ]
            ]
        , tr []
            [ td [ style "width" "80px" ] []
            , td [ style "width" "80px" ] [ blueCell "2" blue.two TapBlue2 ]
            , td [ style "width" "80px" ] [ blueCell "3" blue.three TapBlue3 ]
            , td [ style "width" "80px" ] [ blueCell "4" blue.four TapBlue4 ]
            , bonusCell (BonusOrange 5)
            ]
        , tr []
            [ td [ style "width" "80px" ] [ blueCell "5" blue.five TapBlue5 ]
            , td [ style "width" "80px" ] [ blueCell "6" blue.six TapBlue6 ]
            , td [ style "width" "80px" ] [ blueCell "7" blue.seven TapBlue7 ]
            , td [ style "width" "80px" ] [ blueCell "8" blue.eight TapBlue8 ]
            , bonusCell BonusYellow
            ]
        , tr []
            [ td [ style "width" "80px" ] [ blueCell "9" blue.nine TapBlue9 ]
            , td [ style "width" "80px" ] [ blueCell "10" blue.ten TapBlue10 ]
            , td [ style "width" "80px" ] [ blueCell "11" blue.eleven TapBlue11 ]
            , td [ style "width" "80px" ] [ blueCell "12" blue.twelve TapBlue12 ]
            , bonusCell BonusFox
            ]
        , tr []
            [ bonusCell BonusReroll
            , bonusCell BonusGreen
            , bonusCell (BonusPurple 6)
            , bonusCell BonusPlusOne
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
    textButton msg txtLabel


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
            \x y z -> td [] [ greenCell x y (TapGreen z) ]
    in
    table []
        [ tr [ style "color" "white", style "font-weight" "bold", style "font-size" "20px", style "text-align" "center" ]
            [ td [] [ text "1" ]
            , td [] [ text "3" ]
            , td [] [ text "6" ]
            , td [] [ text "10" ]
            , td [] [ text "15" ]
            , td [] [ text "21" ]
            , td [] [ text "28" ]
            , td [] [ text "36" ]
            , td [] [ text "45" ]
            , td [] [ text "55" ]
            , td [] [ text "66" ]
            ]
        , tr [] (List.map3 cell labels green (List.range 0 11))
        , tr []
            [ td [] []
            , td [] []
            , td [] []
            , bonusCell BonusPlusOne
            , td [] []
            , bonusCell BonusBlue
            , bonusCell BonusFox
            , td [] []
            , bonusCell (BonusPurple 6)
            , bonusCell BonusReroll
            , td [] []
            , td [] []
            ]
        ]


orangeIndexMultipler : Int -> Int
orangeIndexMultipler idx =
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


orangeCell : Int -> Dice -> Html Msg
orangeCell idx dice =
    let
        mult =
            orangeIndexMultipler idx

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

        body =
            case dice of
                Empty ->
                    if mult > 1 then
                        button
                            [ onClick (TapOrange idx)
                            , style "width" "80px"
                            , style "height" "80px"
                            , style "font-size" "20px"
                            , style "font-weight" "thin"
                            ]
                            [ span [ style "color" "lightslategray" ] [ text ("×" ++ String.fromInt mult) ] ]

                    else
                        textButton (TapOrange idx) "\u{00A0}"

                otherwise ->
                    let
                        label =
                            String.fromInt (val * mult)
                    in
                    textButton (TapOrange idx) label
    in
    td [] [ body ]


orangeSection : Orange -> Html Msg
orangeSection orange =
    table []
        [ tr []
            (List.indexedMap orangeCell orange)
        , tr []
            [ td [] []
            , td [] []
            , bonusCell BonusReroll
            , td [] []
            , bonusCell BonusYellow
            , bonusCell BonusPlusOne
            , td [] []
            , bonusCell BonusFox
            , td [] []
            , bonusCell (BonusPurple 6)
            , td [] []
            , td [] []
            ]
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
        [ textButton (TapPurple idx) label ]


purpleSection : Purple -> Html Msg
purpleSection purple =
    table []
        [ tr []
            (List.intersperse (td [ style "font-weight" "bold", style "font-size" "20px", style "color" "white" ] [ text "<" ]) (List.indexedMap purpleCell purple))
        , tr []
            (List.intersperse (td [] [])
                [ td [] []
                , td [] []
                , bonusCell BonusReroll
                , bonusCell BonusBlue
                , bonusCell BonusPlusOne
                , bonusCell BonusYellow
                , bonusCell BonusFox
                , bonusCell BonusReroll
                , bonusCell BonusGreen
                , bonusCell (BonusOrange 6)
                , bonusCell BonusPlusOne
                ]
            )
        ]


type Bonus
    = BonusBlue
    | BonusOrange Int
    | BonusGreen
    | BonusFox
    | BonusPlusOne
    | BonusReroll
    | BonusYellow
    | BonusPurple Int
    | BonusXOr6


bonusCell : Bonus -> Html Msg
bonusCell bonus =
    let
        backgroundColor =
            case bonus of
                BonusBlue ->
                    "blue"

                BonusOrange _ ->
                    "orange"

                BonusGreen ->
                    "green"

                BonusFox ->
                    "transparent"

                BonusPlusOne ->
                    "black"

                BonusReroll ->
                    "transparent"

                BonusYellow ->
                    "yellow"

                BonusPurple _ ->
                    "purple"

                BonusXOr6 ->
                    "black"

        textColor =
            case bonus of
                BonusYellow ->
                    "black"

                otherwise ->
                    "white"

        label =
            case bonus of
                BonusOrange val ->
                    String.fromInt val

                BonusPurple val ->
                    String.fromInt val

                BonusFox ->
                    "🦊"

                BonusPlusOne ->
                    "+1"

                BonusReroll ->
                    "🔄"

                BonusBlue ->
                    "X"

                BonusGreen ->
                    "X"

                BonusYellow ->
                    "X"

                BonusXOr6 ->
                    "X|6"
    in
    td [ style "background-color" backgroundColor, style "text-align" "center", style "width" "80px", style "height" "80px", style "color" textColor ] [ span [ style "padding" "20px", style "font-size" "30px" ] [ text label ] ]


viewScore : Score -> Html Msg
viewScore score =
    table []
        [ tr []
            [ td [] [ text "Yellow" ]
            , td [] [ text (String.fromInt score.yellow) ]
            ]
        , tr []
            [ td [] [ text "Blue" ]
            , td [] [ text (String.fromInt score.blue) ]
            ]
        , tr []
            [ td [] [ text "Green" ]
            , td [] [ text (String.fromInt score.green) ]
            ]
        , tr []
            [ td [] [ text "Orange" ]
            , td [] [ text (String.fromInt score.orange) ]
            ]
        , tr []
            [ td [] [ text "Purple" ]
            , td [] [ text (String.fromInt score.purple) ]
            ]
        , tr []
            [ td [] [ text "Foxes" ]
            , td [] [ text (String.fromInt score.foxes) ]
            ]
        , tr []
            [ td [] [ text "Total" ]
            , td [] [ text (String.fromInt score.total) ]
            ]
        ]


turnSection : List Bool -> Html Msg
turnSection turns =
    table []
        [ tr [] (List.indexedMap turnCell turns)
        , tr []
            [ td [] [ bonusCell BonusReroll ]
            , td [] [ bonusCell BonusPlusOne ]
            , td [] [ bonusCell BonusReroll ]
            , td [] [ bonusCell BonusXOr6 ]
            , td [] []
            , td [] []
            ]
        ]


viewNormal : Card -> Html Msg
viewNormal card =
    div []
        [ table [ style "width" "100%" ]
            [ col [ width 1 ] []
            , col [ width 1 ] []
            , tr []
                [ td [ colspan 2 ]
                    [ table []
                        [ tr []
                            [ td [] [ turnSection card.turns ]
                            , td [ rowspan 4, style "vertical-align" "top", style "font-size" "20px" ] [ viewScore (scoreOf card) ]
                            ]
                        , tr [] [ table [] [ tr [] [ tr [] (List.indexedMap rerollCell card.rerolls) ] ] ]
                        , tr [] [ table [] [ tr [] [ tr [] (List.indexedMap plusOneCell card.plusOnes) ] ] ]
                        ]
                    ]
                ]
            , tr [] [ td [ colspan 2 ] [ text "\u{00A0}" ] ]
            , tr []
                [ td [ style "background-color" "yellow" ]
                    [ yellowSection card.yellow
                    ]
                , td [ style "background-color" "blue" ] [ blueSection card.blue ]
                ]
            , tr [] [ td [ colspan 2 ] [ text "\u{00A0}" ] ]
            , tr []
                [ td [ colspan 2, style "background-color" "green" ]
                    [ greenSection card.green
                    ]
                ]
            , tr [] [ td [ colspan 2 ] [ text "\u{00A0}" ] ]
            , tr []
                [ td [ colspan 2, style "background-color" "orange" ]
                    [ orangeSection card.orange
                    ]
                ]
            , tr [] [ td [ colspan 2 ] [ text "\u{00A0}" ] ]
            , tr []
                [ td [ colspan 2, style "background-color" "purple" ]
                    [ purpleSection card.purple
                    ]
                ]
            ]
        ]


viewPick : Int -> Html Msg
viewPick idx =
    div [ style "margin" "320px 320px auto" ]
        [ table []
            [ tr []
                [ td [] [ textButton TapDice1 "1" ]
                , td [] [ textButton TapDice2 "2" ]
                , td [] [ textButton TapDice3 "3" ]
                ]
            , tr []
                [ td [] [ textButton TapDice4 "4" ]
                , td [] [ textButton TapDice5 "5" ]
                , td [] [ textButton TapDice6 "6" ]
                ]
            , tr [ colspan 2 ]
                [ td [] [ textButton TapDiceClear "X" ]
                ]
            ]
        ]


textButton : Msg -> String -> Html Msg
textButton msg label =
    button
        [ onClick msg
        , style "width" "80px"
        , style "height" "80px"
        , style "font-size" "20px"
        , style "font-weight" "thin"
        ]
        [ text label ]


htmlButton : Msg -> List (Html Msg) -> Html Msg
htmlButton msg body =
    button
        [ onClick msg
        , style "width" "100px"
        , style "height" "100px"
        , style "font-size" "20px"
        , style "font-weight" "thin"
        ]
        body


view : Model -> Html Msg
view model =
    case model.uiState of
        Normal ->
            viewNormal model.card

        ChooseOrange idx ->
            viewPick idx

        ChoosePurple idx ->
            viewPick 0
