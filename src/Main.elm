module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, array, decodeValue, field, list, map6, string)
import List.Extra
import Random
import Random.List


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type Model
    = InvalidQuestions
    | PreparingQuiz (List QuestionToAnswer)
    | PlayingQuiz (List QuestionToAnswer) Quiz


type alias Quiz =
    { currentQuestion : Int
    , questions : List QuestionToAnswer
    , state : QuizState
    }


type QuizState
    = Playing
    | Lost
    | Won


type alias QuestionToAnswer =
    { question : String
    , a : String
    , b : String
    , c : String
    , d : String
    , answer : String
    }


questionDecoder : Decoder QuestionToAnswer
questionDecoder =
    map6 QuestionToAnswer
        (field "question" string)
        (field "A" string)
        (field "B" string)
        (field "C" string)
        (field "D" string)
        (field "answer" string)



--init model


init : Json.Decode.Value -> ( Model, Cmd Msg )
init json =
    case decodeValue (list questionDecoder) json of
        Ok questions ->
            ( PreparingQuiz questions, buildQuiz questions )

        Err error ->
            ( InvalidQuestions, Cmd.none )


buildQuiz : List QuestionToAnswer -> Cmd Msg
buildQuiz questions =
    questions
        |> Random.List.shuffle
        |> Random.map (List.take 15)
        |> Random.generate QuizBuilt


initQuiz : List QuestionToAnswer -> Quiz
initQuiz questions =
    { currentQuestion = 1
    , questions = questions
    , state = Playing
    }



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = Answer String
    | QuizBuilt (List QuestionToAnswer)
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msgm, model ) of
        ( QuizBuilt questions, PreparingQuiz allQuestions ) ->
            ( PlayingQuiz allQuestions (Quiz 0 questions Playing)
            , Cmd.none
            )

        ( Answer ans, PlayingQuiz allQuestions quiz ) ->
            if isCorrect ans quiz then
                ( PlayingQuiz allQuestions (advanceQuestion quiz)
                , Cmd.none
                )

            else
                ( PlayingQuiz allQuestions { quiz | state = Lost }
                , Cmd.none
                )

        ( CloseModal, PlayingQuiz allQuestions quiz ) ->
            ( PreparingQuiz allQuestions
            , buildQuiz allQuestions
            )

        _ ->
            ( model, Cmd.none )


advanceQuestion : Quiz -> Quiz
advanceQuestion quiz =
    if quiz.currentQuestion >= 15 then
        { quiz | state = Won }

    else
        { quiz | currentQuestion = quiz.currentQuestion + 1 }


isCorrect : String -> Quiz -> Bool
isCorrect ans quiz =
    case currentQuestion quiz of
        Just question ->
            question.answer == ans

        Nothing ->
            False


currentQuestion : Quiz -> Maybe QuestionToAnswer
currentQuestion quiz =
    List.Extra.getAt quiz.currentQuestion quiz.questions



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        InvalidQuestions ->
            text "invalid questions"

        PreparingQuiz questions ->
            text "preparing quiz"

        PlayingQuiz questions quiz ->
            gameView quiz


gameView : Quiz -> Html Msg
gameView quiz =
    case quiz.state of
        Playing ->
            layout
                [ quizView quiz ]

        Won ->
            layout
                [ quizView quiz
                , modal "Game over!!! You won the ultimate prize €1,000,000"
                ]

        Lost ->
            layout
                [ quizView quiz
                , modal (gameOverMessage quiz)
                ]


gameOverMessage : Quiz -> String
gameOverMessage quiz =
    if quiz.currentQuestion < 5 then
        "Game over!!! You won €100"

    else if quiz.currentQuestion < 10 then
        "Game over!!! You won €1,000"

    else
        "Game over!!! You won €32,000"


layout : List (Html Msg) -> Html Msg
layout body =
    div [ class "container" ] body


quizView : Quiz -> Html Msg
quizView quiz =
    div [ class "row" ]
        [ div [ class "col-xs-12 main" ]
            [ div [ class "row" ]
                [ avatarSection
                , prizeSection quiz
                ]
            , div [ class "row" ]
                [ lifeLineSection
                ]
            , case List.Extra.getAt quiz.currentQuestion quiz.questions of
                Just question ->
                    div [ class "row" ]
                        [ answerSection question
                        ]

                Nothing ->
                    text ""
            ]
        ]


modal : String -> Html Msg
modal message =
    div [ class "game-over-modal" ]
        [ div [ class "game-over-modal-content" ]
            [ span [ class "close-tab", onClick CloseModal ]
                [ text "×" ]
            , div [ class "game-over-modal-content-holder" ]
                [ text message ]
            ]
        ]


avatarSection : Html msg
avatarSection =
    div [ class "avatar col-xs-offset-1 col-xs-6" ]
        []


prizeSection : Quiz -> Html msg
prizeSection quiz =
    div [ class "prizes col-xs-4" ]
        [ div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 15 )
                , ( "_15", True )
                ]
            ]
            [ text "€1,000,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 14 )
                , ( "_14", True )
                ]
            ]
            [ text "€500,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 13 )
                , ( "_13", True )
                ]
            ]
            [ text "€250,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 12 )
                , ( "_12", True )
                ]
            ]
            [ text "€125,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 11 )
                , ( "_11", True )
                ]
            ]
            [ text "€64,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 10 )
                , ( "_10", True )
                ]
            ]
            [ text "€32,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 9 )
                , ( "_9", True )
                ]
            ]
            [ text "€16,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 8 )
                , ( "_8", True )
                ]
            ]
            [ text "€8,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 7 )
                , ( "_7", True )
                ]
            ]
            [ text "€40,00" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 6 )
                , ( "_6", True )
                ]
            ]
            [ text "€2,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 5 )
                , ( "_5", True )
                ]
            ]
            [ text "€1,000" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 4 )
                , ( "_4", True )
                ]
            ]
            [ text "€500" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 3 )
                , ( "_3", True )
                ]
            ]
            [ text "€300" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 2 )
                , ( "_2", True )
                ]
            ]
            [ text "€200" ]
        , div
            [ classList
                [ ( "row", True )
                , ( "prize", True )
                , ( "current-prize", quiz.currentQuestion == 1 )
                , ( "_1", True )
                ]
            ]
            [ text "€100" ]
        ]


lifeLineSection : Html msg
lifeLineSection =
    div [ class "lifeLine" ]
        []


answerSection : QuestionToAnswer -> Html Msg
answerSection question =
    div [ class "answers col-xs-12" ]
        [ div [ class "row" ]
            [ div [ class "question" ]
                [ text question.question ]
            ]
        , div [ class "row" ]
            [ div [ class "answer a", onClick (Answer "A") ]
                [ text question.a ]
            , div [ class "answer b", onClick (Answer "B") ]
                [ text question.b ]
            , div [ class "answer c", onClick (Answer "C") ]
                [ text question.c ]
            , div [ class "answer d", onClick (Answer "D") ]
                [ text question.d ]
            ]
        ]



--extractQuestions : Quiz -> QuestionToAnswer
--extractQuestions quiz =
--    case
--        quiz.questions
--            |> Array.fromList
--            |> Array.get (quiz.currentQuestion - 1)
--    of
--        Just q ->
--            case
--                quiz.questionsToAnswer
--                    |> Array.fromList
--                    |> Array.get q
--            of
--                Nothing ->
--                    QuestionToAnswer "empty" "empty" "empty" "empty" "empty" "empty"
--                Just val ->
--                    val
--        Nothing ->
--            QuestionToAnswer "empty" "empty" "empty" "empty" "empty" "empty"
