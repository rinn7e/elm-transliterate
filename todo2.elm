module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


type alias Model =
    { todo :
        { title : String
        , content : String
        , index : Int
        }
    , edit :
        { title : String
        , content : String
        , index : Int
        }
    , editBox : Bool
    , todos :
        List
            { title : String
            , content : String
            , index : Int
            }
    , title : String
    , content : String
    }


model : Model
model =
    { todo =
        { title = ""
        , content = ""
        , index = 1
        }
    , edit =
        { title = ""
        , content = ""
        , index = 0
        }
    , editBox = False
    , todos =
        []
    , title = ""
    , content = ""
    }


type Msg
    = TitleInput String
    | ContentInput String
    | TitleEdit String
    | ContentEdit String
    | AddTodo
    | RemoveItem Int
    | EditItem { title : String, content : String, index : Int }
    | SaveEdit



-- getCurrentIndex : Model -> Int
-- getCurrentIndex model =
--   { model | todo = { modelTodo | index } }


update : Msg -> Model -> Model
update msg model =
    case msg of
        TitleInput text ->
            let
                oldTodo =
                    model.todo

                newTodo =
                    { oldTodo | title = text }
            in
                { model | todo = newTodo }

        ContentInput text ->
            let
                modelTodo =
                    model.todo
            in
                { model | todo = { modelTodo | content = text } }

        TitleEdit text ->
            let
                modelEdit =
                    model.edit
            in
                { model | edit = { modelEdit | title = text } }

        ContentEdit text ->
            let
                modelEdit =
                    model.edit
            in
                { model | edit = { modelEdit | content = text } }

        AddTodo ->
            let
                modelTodo =
                    model.todo

                currentIndex =
                    model.todo.index
            in
                { model
                    | todos = model.todo :: model.todos
                    , todo =
                        { modelTodo
                            | title = ""
                            , content = ""
                            , index = currentIndex + 1
                        }
                }

        RemoveItem index ->
            { model | todos = List.filter (\x -> x.index /= index) model.todos }

        EditItem todo ->
            let
                modelTodo =
                    model.todo
            in
                { model | edit = todo, editBox = True }

        -- SaveEdit ->
        --     let
        --         newTodos =
        --             List.filter (\x -> x.index /= model.edit.index) model.todos
        --
        --     in
        --         { model
        --             | todos = model.edit :: newTodos
        --             , edit =
        --                 { title = ""
        --                 , content = ""
        --                 , index = 0
        --                 }
        --             , editBox = False
        --         }
        SaveEdit ->
            let
                -- changetodo todo =
                --     if (todo.index == model.edit.index) then
                --         model.edit
                --     else
                --         todo
                newTodos =
                    List.map
                        (\todo ->
                            if (todo.index == model.edit.index) then
                                model.edit
                            else
                                todo
                        )
                        model.todos
            in
                { model
                    | todos = newTodos
                    , edit =
                        { title = ""
                        , content = ""
                        , index = 0
                        }
                    , editBox = False
                }


todoItem : { title : String, content : String, index : Int } -> Html Msg
todoItem todo =
    div []
        [ h3 [] [ text (toString (todo.index) ++ " Title: " ++ todo.title) ]
        , p [] [ text ("Content: " ++ todo.content) ]
        , button [ onClick (EditItem todo) ] [ text "Edit" ]
        , button [ onClick (RemoveItem todo.index) ] [ text "Delete" ]
        ]


todoEdit : { title : String, content : String, index : Int } -> Html Msg
todoEdit todo =
    div []
        [ input [ placeholder "Title", value todo.title, onInput TitleEdit ] []
        , input [ placeholder "Content", value todo.content, onInput ContentEdit ]
            []
        , button
            [ onClick SaveEdit ]
            [ text "save" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "TodoList" ]
        , input [ placeholder "Title", value model.todo.title, onInput TitleInput ] []
        , input [ placeholder "Content", value model.todo.content, onInput ContentInput ] []
        , button [ onClick AddTodo ] [ text "submit" ]
        , div []
            (List.map
                todoItem
                model.todos
            )
        , h3 [] [ text "Edit" ]
        , if model.editBox == True then
            (todoEdit model.edit)
          else
            h3 [] [ text "noedit" ]
        ]
