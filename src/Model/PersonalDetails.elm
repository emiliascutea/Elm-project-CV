module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id, href, style, src)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }


view : PersonalDetails -> Html msg
view details =
    let
        getContacts contacts = List.map (\cont -> p [class "contact-detail"] [text (cont.name ++ " : " ++ cont.detail)]) contacts
        getSocials socials = List.map (\social -> p [class "social-link"] [a [href social.detail ] [text social.name]] ) socials
    in
        div [style "background-color" "powderblue"] 
        [ h1[id "name"] [text details.name]
        , em[id "intro"] [text ("About me: " ++ details.intro)]
        , div [] (getContacts details.contacts)
        , div [] (getSocials details.socials)
        ]
        
  
