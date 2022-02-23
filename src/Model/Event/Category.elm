module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view)

import Html exposing (Html, div, input, text, p)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories =
    [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type alias SelectedEventCategories = { academic :  Maybe EventCategory, work : Maybe EventCategory, project : Maybe EventCategory, award : Maybe EventCategory}

{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected =  { academic =  Just Academic, work = Just Work, project = Just Project, award = Just Award}


{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current =
    let
        checkSelected str =
            case str of
                Just s -> True
                Nothing -> False
    in
        case category of
            Academic -> 
                checkSelected current.academic
            Work -> 
                checkSelected current.work
            Project -> 
                checkSelected current.project
            Award -> 
                checkSelected current.award


{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}


set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value current =
    let
        {academic, work, project, award} = current
        
        newAcademic = 
            if category == Academic then
                if value == True then
                    Just Academic
                else
                    Nothing
            else
                academic
        
        
        newWork =
            if category == Work then
                if value == True then
                    Just Work
                else
                    Nothing
            else 
                work

        newProject = 
            if category == Project then
                if value == True then
                    Just Project
                else
                    Nothing
            else 
                project

        newAward = 
            if category == Award then
                if value == True then
                    Just Award
                else
                    Nothing
            else 
                award
    in
        {academic = newAcademic, work = newWork, project = newProject, award = newAward}
    


checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model =
    div [(style "text-align" "center")] 
        [ (checkbox "Academic events" (isEventCategorySelected Academic model) Academic)
        , (checkbox "Work events" (isEventCategorySelected Work model) Work)
        , (checkbox "Project events" (isEventCategorySelected Project model) Project)
        , (checkbox "Award events" (isEventCategorySelected Award model) Award)
        ]
