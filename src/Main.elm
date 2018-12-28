module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)
import Json.Decode exposing (Decoder, field, string, map6, list, decodeValue, array)
import Array
import Random

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

-- MODEL

type alias Model = 
    {
       questionsToAnswer: List QuestionToAnswer
     , currentQuestion: Int
     , questions: List Int
     , showModal: Bool
     , message: String
    }

type alias QuestionToAnswer =
    {
        question: String
     ,  a: String
     ,  b: String
     ,  c: String
     ,  d: String
     ,  answer: String
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
init : Json.Decode.Value -> (Model, Cmd Msg)
init questions =
  (Model (getQuestions questions) 1 [] False "", get15Randoms)


getQuestions : Json.Decode.Value -> List QuestionToAnswer
getQuestions questions =
 case(decodeValue (list questionDecoder) questions) of
   Ok question ->
     question
   Err error ->
     []

get15Randoms : Cmd Msg     
get15Randoms = 
  Random.generate RndGen (Random.int 0 106)


-- SUBSCRIPTION
subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none

-- UPDATE
type Msg = Answer String
  | RndGen Int
  | CloseModal

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Answer ans ->
      ( (checkAnswer model ans), Cmd.none )
    RndGen rndGen ->
      if List.length (model.questions) < 15
        then
          if List.member rndGen model.questions
            then
              (model, get15Randoms)
          else
            ({model | questions = rndGen :: model.questions  }, get15Randoms)
      else
        (model, Cmd.none)
    CloseModal ->
      ({model | showModal = False, currentQuestion = 1, questions = []}, get15Randoms)
      
checkAnswer model ans =
  if(.answer (extractQuestions model) == ans)
    then
      if model.currentQuestion >= 15
        then
          composeMsg model
      else
        ({model | currentQuestion = model.currentQuestion + 1})
  else
    composeMsg model
    
composeMsg: Model -> Model
composeMsg model =
  if model.currentQuestion == 15
    then
      ({model | showModal = True, message = "Game over!!! You won the ultimate prize €1,000,000"})
  else if model.currentQuestion < 15 && model.currentQuestion >= 10
    then
      ({model | showModal = True, message = "Game over!!! You won €32,000"})
  else if model.currentQuestion < 10 && model.currentQuestion >= 5
    then
      ({model | showModal = True, message = "Game over!!! You won €1,000"})
  else
      ({model | showModal = True, message = "Game over!!! You won €100"})

-- VIEW
view : Model -> Html Msg
view model =
  div [ class "container"]
    [ div [ class "row"]
          [ div [class "col-xs-12 main"]
                [  div [ class "row"]
                       [ avatarSection model
                       , prizeSection model
                       ]
                , div [ class "row"]
                      [ lifeLineSection model
                      ]
                , div [ class "row"]
                      [ answerSection model
                      ]
                ]
          ]
       ,div[]
          [ modal model]
    ]

modal : Model -> Html Msg
modal model =
  let 
    elem = 
        if model.showModal
          then
            div [class "game-over-modal"]
                [
                div [ class "game-over-modal-content"]
                    [
                      span [ class "close-tab", onClick CloseModal]
                           [text "×"]
                    , div [ class "game-over-modal-content-holder"]
                         [ text model.message]
                    ]
                 ]
         else
           div [][]
   in
     elem


avatarSection : Model -> Html msg
avatarSection model =
     div [ class "avatar col-xs-offset-1 col-xs-6"]
         [ 

         ]

prizeSection : Model -> Html msg
prizeSection model =
    div [ class "prizes col-xs-4"]
        [ div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 15),
                    ("_15", True)
                 ]
            ]
              [ text "€1,000,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 14),
                    ("_14", True)
                 ]
            ]
              [ text "€500,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 13),
                    ("_13", True)
                 ]
            ]
              [ text "€250,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 12),
                    ("_12", True)
                 ]
            ]
              [ text "€125,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 11),
                    ("_11", True)
                 ]
            ]
              [ text "€64,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 10),
                    ("_10", True)
                 ]
            ]
              [ text "€32,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 9),
                    ("_9", True)
                 ]
            ]
              [ text "€16,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 8),
                    ("_8", True)
                 ]
            ]
              [ text "€8,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 7),
                    ("_7", True)
                 ]
            ]
              [ text "€40,00"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 6),
                    ("_6", True)
                 ]
            ]
              [ text "€2,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 5),
                    ("_5", True)
                 ]
            ]
              [ text "€1,000"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 4),
                    ("_4", True)
                 ]
            ]
              [ text "€500"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 3),
                    ("_3", True)
                 ]
            ]
              [ text "€300"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 2),
                    ("_2", True)
                 ]
            ]
              [ text "€200"]
        , div [ classList [
                    ("row", True),
                    ("prize", True),
                    ("current-prize", model.currentQuestion == 1),
                    ("_1", True)
                 ]
            ]
              [ text "€100"]
        ]

lifeLineSection : Model -> Html msg
lifeLineSection model =
    div [ class "lifeLine"]
        [ 

        ]

answerSection : Model -> Html Msg
answerSection model =
    div [ class "answers col-xs-12"]
        [ div [class "row"]
              [ div [ class "question"]
                    [ text (.question (extractQuestions model))]
              ]
        , div [ class "row"]
              [ div [ class "answer a", onClick (Answer "A")]
                    [ text (.a (extractQuestions model))]
              , div [ class "answer b", onClick (Answer "B")]
                    [ text (.b (extractQuestions model))]
              , div [ class "answer c", onClick (Answer "C")]
                    [ text (.c (extractQuestions model))]
              , div [ class "answer d", onClick (Answer "D")]
                    [ text (.d (extractQuestions model))]      
              ]
        ]
        
extractQuestions : Model -> QuestionToAnswer
extractQuestions model =
  case(
    model.questions
      |> Array.fromList
      |> Array.get (model.currentQuestion - 1)
    ) of
    Just q ->
      case(
        model.questionsToAnswer
          |> Array.fromList
          |> Array.get q
      ) of
        Nothing ->
          QuestionToAnswer "empty" "empty" "empty" "empty" "empty" "empty"
        Just val ->
          val
    Nothing ->
      QuestionToAnswer "empty" "empty" "empty" "empty" "empty" "empty"