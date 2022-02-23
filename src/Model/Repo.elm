module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Json.Decode as De


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


view : Repo -> Html msg
view repo =
    let
        getFont = style "font-family" "Calibri"
        getSize = style "font-size" "115%"

    in
        div [class "repo"] 
        [ h1 [class "repo-name", getSize] [text repo.name]
        , p [class "repo-description"] [ div [getFont] [ i [] [text "Description"]], text (repo.description |> Maybe.withDefault "None")]
        , p [class "repo-url"] [div [getFont] [ i [] [text "Link"]], a [href repo.url] [text repo.url]]
        , p [class "repo-stars"] [ div [getFont] [ i [] [text "Stars"]], text (String.fromInt <| repo.stars)]
        ]


sortByStars : List Repo -> List Repo
sortByStars repos = repos |> List.sortBy .stars

{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}

decodeRepo : De.Decoder Repo
decodeRepo =
    De.map5 Repo
        (De.field "name" De.string)
        (De.maybe (De.field "description" De.string))
        (De.field "html_url" De.string)
        (De.field "pushed_at" De.string)
        (De.field "stargazers_count" De.int)