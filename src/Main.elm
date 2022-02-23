module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode as De
import Model exposing (..)
import Model.Event as Event
import Model.Event.Category as EventCategory
import Model.PersonalDetails as PersonalDetails
import Model.Repo as Repo
import Model.Event.Category as Categ


type Msg
    = GetRepos
    | GotRepos (Result Http.Error (List Repo.Repo))
    | SelectEventCategory EventCategory.EventCategory
    | DeselectEventCategory EventCategory.EventCategory


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , getRepos
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

getRepos : Cmd Msg
getRepos = Http.get
    { url = "https://api.github.com/users/emiliascutea/repos?sort=pushed"
    , expect = Http.expectJson GotRepos (De.list Repo.decodeRepo)
    }



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRepos ->
            ( model, Cmd.none )

        GotRepos res -> -- success or error
            case res of
                Ok repos -> ({model | repos = repos}, Cmd.none)
                Err _ -> ( model, Cmd.none )

        SelectEventCategory category -> -- set category true
            ( { model | selectedEventCategories = Categ.set category True model.selectedEventCategories} , Cmd.none)

        DeselectEventCategory category -> -- set category false
            ( { model | selectedEventCategories = Categ.set category False model.selectedEventCategories} , Cmd.none)

eventCategoryToMsg : ( EventCategory.EventCategory, Bool ) -> Msg
eventCategoryToMsg ( event, selected ) =
    if selected then
        SelectEventCategory event

    else
        DeselectEventCategory event


view : Model -> Html Msg
view model =
    let
        eventCategoriesView =
            EventCategory.view model.selectedEventCategories |> Html.map eventCategoryToMsg

        eventsView = 
            model.events
                |> List.filter (.category >> (\cat -> EventCategory.isEventCategorySelected cat model.selectedEventCategories))
                |> List.map Event.view
                |> div []
                |> Html.map never

        reposView =
            model.repos
                |> Repo.sortByStars
                |> List.take 7
                |> List.map Repo.view
                |> div []

    in
        div []
            [ h1 [(style "text-align" "center"), (style "font-size" "150%")] [i [] [text "My portfolio"]]
            , PersonalDetails.view model.personalDetails
            , h2 [(style "font-size" "130%"), (style "text-align" "center")] [ i[][text "Experience" ]]
            , div[style "background-color" "AntiqueWhite"] [eventCategoriesView
                                                            , eventsView]
            , h2 [(style "font-size" "130%"), (style "text-align" "center")] [i [] [text "My top repos"]]
            , div [style "background-color" "Thistle"] [reposView]
            ]
