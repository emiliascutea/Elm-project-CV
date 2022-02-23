module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, style, href)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (..)
import List exposing (..)

type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"


sortByInterval : List Event -> List Event
sortByInterval events =
    events
        |> List.sortWith (\ev1 -> \ev2 -> Interval.compare ev1.interval ev2.interval)
    
view : Event -> Html Never
view event =
    let
        getClass imp = 
            case imp of
                True -> class "event event-important"
                False -> class "event"
        
        getFont = style "font-family" "Calibri"
        getSize = style "font-size" "115%"

        getLength = Interval.length event.interval  |> Maybe.withDefault (0, 0)

        getInterval = if (getYears ++ " " ++ getMonths) == " " then "None" else (getYears ++ " " ++ getMonths)

        getYears = if Tuple.first getLength == 0 then "" else (String.fromInt (Tuple.first getLength) ++ " years")
        
        getMonths = if Tuple.second getLength == 0 then "" else (String.fromInt (Tuple.second getLength) ++ " months")
        
        getUrl = case event.url of
                    Just u -> a [href u] [text u]
                    Nothing -> a [] [text "None"]
    in
        div [getClass event.important] 
            [ h1 [class "event-title", getSize] [text event.title]
            , p [class "event-description"] [ div [ getFont ] [ i [] [ text "Description" ]], event.description]
            , p [class "event-category"] [ div [ getFont ] [ i [] [ text "Category" ]], ( categoryView event.category )]
            , p [class "event-interval"] [ div [ getFont ] [ i [] [ text "Interval" ]], text ( getInterval )]
            , p [class "event-url"] [ div [ getFont ] [ i [] [ text "Link" ]], getUrl]
            ]
