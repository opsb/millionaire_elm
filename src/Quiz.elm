module Quiz exposing (Answer(..), Question, Quiz, QuizState(..), advance, build, currentPrizeMoney, currentQuestion, isCorrect, prizeMoney, questionDecoder)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, array, decodeValue, field, list, map6, string)
import List.Extra
import Random
import Random.List


type alias Quiz =
    { currentQuestion : Int
    , questions : List Question
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


build : (Quiz -> msg) -> List Question -> Cmd msg
build tagger questions =
    questions
        |> Random.List.shuffle
        |> Random.map (List.take 15)
        |> Random.map init
        |> Random.generate tagger


init : List Question -> Quiz
init questions =
    { currentQuestion = 0
    , questions = questions
    , state = Playing
    }


advance : Quiz -> Quiz
advance quiz =
    if quiz.currentQuestion == List.length quiz.questions then
        { quiz | state = Won }

    else
        { quiz | currentQuestion = quiz.currentQuestion + 1 }


isCorrect : Answer -> Quiz -> Bool
isCorrect ans quiz =
    case currentQuestion quiz of
        Just question ->
            question.answer == ans

        Nothing ->
            False


currentQuestion : Quiz -> Maybe Question
currentQuestion quiz =
    List.Extra.getAt quiz.currentQuestion quiz.questions


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


prizeMoney : Int -> Float
prizeMoney questionIndex =
    prizes
        |> Dict.get questionIndex
        |> Maybe.withDefault 0


currentPrizeMoney : Quiz -> Float
currentPrizeMoney quiz =
    prizeMoney quiz.currentQuestion


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
