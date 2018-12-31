module Quiz exposing
    ( Answer(..)
    , Question
    , Quiz
    , QuizState(..)
    , advance
    , build
    , currentIndex
    , currentPrizeMoney
    , currentQuestion
    , isCorrect
    , prizeMoney
    , questionDecoder
    )

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, array, decodeValue, field, list, map6, string)
import List.Extra
import Random
import Random.List


type alias Quiz =
    { before : List Question
    , current : Question
    , after : List Question
    , state : QuizState
    }


type QuizState
    = Playing
    | Lost
    | Won


type alias Question =
    { text : String
    , a : String
    , b : String
    , c : String
    , d : String
    , answer : Answer
    }


type Answer
    = A
    | B
    | C
    | D


build : (Result String Quiz -> msg) -> List Question -> Cmd msg
build tagger questions =
    questions
        |> Random.List.shuffle
        |> Random.map (List.take 15)
        |> Random.map init
        |> Random.generate tagger


init : List Question -> Result String Quiz
init questions =
    case questions of
        [] ->
            Err "no questions available"

        first :: others ->
            Ok
                { before = []
                , current = first
                , after = others
                , state = Playing
                }


length : Quiz -> Int
length quiz =
    List.length quiz.before + 1 + List.length quiz.after


advance : Quiz -> Quiz
advance quiz =
    if currentIndex quiz == length quiz then
        { quiz | state = Won }

    else
        case quiz.after of
            [] ->
                quiz

            head :: rest ->
                { quiz | before = quiz.before ++ [ quiz.current ], current = head, after = rest }


isCorrect : Answer -> Quiz -> Bool
isCorrect ans quiz =
    quiz.current.answer == ans


currentQuestion : Quiz -> Question
currentQuestion quiz =
    quiz.current


questionDecoder : Decoder Question
questionDecoder =
    map6 Question
        (field "question" string)
        (field "A" string)
        (field "B" string)
        (field "C" string)
        (field "D" string)
        (field "answer" answerDecoder)


answerDecoder : Decoder Answer
answerDecoder =
    JD.string
        |> JD.andThen
            (\answerText ->
                case answerText of
                    "A" ->
                        JD.succeed A

                    "B" ->
                        JD.succeed B

                    "C" ->
                        JD.succeed C

                    "D" ->
                        JD.succeed D

                    _ ->
                        JD.fail ("invalid answer: " ++ answerText)
            )


currentPrizeMoney : Quiz -> Float
currentPrizeMoney quiz =
    case quiz.state of
        Lost ->
            lostPrizeMoney (currentIndex quiz)

        _ ->
            prizeMoney (currentIndex quiz)


lostPrizeMoney : Int -> Float
lostPrizeMoney questionIndex =
    if questionIndex < 5 then
        100

    else if questionIndex < 10 then
        1000

    else if questionIndex < 15 then
        32000

    else
        1000000


prizeMoney : Int -> Float
prizeMoney questionIndex =
    prizes
        |> Dict.get questionIndex
        |> Maybe.withDefault 0


currentIndex : Quiz -> Int
currentIndex quiz =
    List.length quiz.before + 1


prizes : Dict Int Float
prizes =
    Dict.fromList
        [ ( 15, 1000000 )
        , ( 14, 500000 )
        , ( 13, 250000 )
        , ( 12, 125000 )
        , ( 11, 64000 )
        , ( 10, 32000 )
        , ( 9, 16000 )
        , ( 8, 8000 )
        , ( 7, 4000 )
        , ( 6, 2000 )
        , ( 5, 1000 )
        , ( 4, 500 )
        , ( 3, 300 )
        , ( 2, 200 )
        , ( 1, 100 )
        ]
