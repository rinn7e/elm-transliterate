module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main =
    Html.beginnerProgram
        { model =
            init
        , update = update
        , view = view
        }



--model


type alias Model =
    { todo : TodoModel
    , todos : List TodoModel
    }


type alias TodoModel =
    { title : String
    , content : String
    }


initTodo : TodoModel
initTodo =
    { title = ""
    , content = ""
    }


init : Model
init =
    { todo = initTodo
    , todos = []
    }



--update


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "Rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "https://bootswatch.com/flatly/bootstrap.min.css"
            ]

        children =
            []
    in
        node tag attrs children


type Msg
    = AddTodo
    | TitleInput String
    | ContentInput String
    | RemoveAll
    | RemoveItem TodoModel
    | ClearInput


update : Msg -> Model -> Model
update msg model =
    case msg of
        TitleInput text ->
            { model | todo = { todo | title = text } }

        ContentInput text ->
            { model | todo = text }

        RemoveItem todo ->
            { model | todo = List.filter (\x -> x /= text) model.todos }



-- AddTodo ->
--     { model | todos = model.todo :: model.todos, todo = " " }
--
-- RemoveAll ->
--     { model | todos = [] }
--
--
-- ClearInput ->
--     { model | todo = "" }
--view


todoItem : TodoModel -> Html Msg
todoItem todo =
    li [] [ text todo.title, button [ onClick (RemoveItem todo), class "btn btn-primary" ] [ text "x" ] ]


todoList : List TodoModel -> Html Msg
todoList todos =
    let
        child =
            List.map todoItem todos
    in
        ul [] child


view : Model -> Html Msg
view model =
    div [ class "jumbotron" ]
        [ stylesheet
        , input
            [ type_ "text"
            , placeholder "Title"
            , onInput TitleInput
            , value model.todo.title
            , class "form-control"
            ]
            []
        , input
            [ type_ "text"
            , placeholder "Content"
            , onInput ContentInput
            , value model.todo.content
            , class "form-control"
            ]
            []
        , button [ onClick (AddTodo), class "btn btn-primary" ] [ text "Submit" ]
        , button [ onClick RemoveAll, class "btn btn-danger" ] [ text "Remove All" ]
        , div [] [ todoList model.todos ]
        ]
