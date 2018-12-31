module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Numeral
import Quiz exposing (Answer(..), Question, Quiz, QuizState(..))


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type Model
    = InvalidQuestions
    | BuildingQuiz (List Question)
    | PlayingQuiz (List Question) Quiz



--init model


init : JD.Value -> ( Model, Cmd Msg )
init json =
    case JD.decodeValue (JD.list Quiz.questionDecoder) json of
        Ok questions ->
            ( BuildingQuiz questions, Quiz.build QuizBuilt questions )

        Err error ->
            ( InvalidQuestions, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = AnswerClicked Answer
    | QuizBuilt Quiz
    | CloseModalClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( QuizBuilt quiz, BuildingQuiz allQuestions ) ->
            ( PlayingQuiz allQuestions quiz
            , Cmd.none
            )

        ( AnswerClicked ans, PlayingQuiz allQuestions quiz ) ->
            if Quiz.isCorrect ans quiz then
                ( PlayingQuiz allQuestions (Quiz.advance quiz)
                , Cmd.none
                )

            else
                ( PlayingQuiz allQuestions { quiz | state = Lost }
                , Cmd.none
                )

        ( CloseModalClicked, PlayingQuiz allQuestions quiz ) ->
            ( BuildingQuiz allQuestions
            , Quiz.build QuizBuilt allQuestions
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        InvalidQuestions ->
            text "invalid questions"

        BuildingQuiz questions ->
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
    "Game over!!! You won " ++ formatEuros (Quiz.currentPrizeMoney quiz)


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
            , case Quiz.currentQuestion quiz of
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
            [ span [ class "close-tab", onClick CloseModalClicked ]
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
    div [ class "prizes col-xs-4" ] <|
        List.map (prizeBadge quiz) (List.reverse <| List.range 1 15)


prizeBadge : Quiz -> Int -> Html msg
prizeBadge quiz questionIndex =
    div
        [ classList
            [ ( "row", True )
            , ( "prize", True )
            , ( "current-prize", quiz.currentQuestion == questionIndex )
            , ( "_" ++ String.fromInt questionIndex, True )
            ]
        ]
        [ text <| formatEuros (Quiz.prizeMoney questionIndex) ]


lifeLineSection : Html msg
lifeLineSection =
    div [ class "lifeLine" ]
        []


answerSection : Question -> Html Msg
answerSection question =
    div [ class "answers col-xs-12" ]
        [ div [ class "row" ]
            [ div [ class "question" ]
                [ text question.text ]
            ]
        , div [ class "row" ]
            [ answerButton question.a "a" A
            , answerButton question.b "b" B
            , answerButton question.c "c" C
            , answerButton question.d "d" D
            ]
        ]


answerButton : String -> String -> Answer -> Html Msg
answerButton text_ letter answer =
    div
        [ class ("answer " ++ letter)
        , onClick (AnswerClicked answer)
        ]
        [ text text_ ]


formatEuros : Float -> String
formatEuros amount =
    Numeral.format "€0,0.00" amount
