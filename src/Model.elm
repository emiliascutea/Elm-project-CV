module Model exposing (..)

import Html exposing (b, div, p, text)
import Model.Date as Date
import Model.Event as Event exposing (Event)
import Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected)
import Model.Interval as Interval
import Model.PersonalDetails exposing (DetailWithName, PersonalDetails)
import Model.Repo exposing (Repo)


type alias Model =
    { personalDetails : PersonalDetails
    , events : List Event
    , selectedEventCategories : SelectedEventCategories
    , repos : List Repo
    }


academicEvents : List Event
academicEvents =
    [ { title = "Graduated from Colegiul National Gheorghe Lazar Sibiu"
      , interval = Interval.withDurationYears (Date.onlyYear 2015) 3
      , description = p [] [ text "I obtained ", b [] [ text "very" ], text " good grades." ]
      , category = Academic
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Education : Technical University of Cluj - Napoca"
      , interval = Interval.withDurationYears (Date.onlyYear 2020) 2
      , description = p [] [ text "Studying Computer Science." ]
      , category = Academic
      , url = Just "https://ac.utcluj.ro/acasa.html"
      , tags = []
      , important = False
      }
    ]


workEvents : List Event
workEvents =
    [ { title = "Work event 1"
      , interval = Interval.withDurationMonths 2019 Date.Jun 3
      , description = text "Internship"
      , category = Work
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


projectEvens : List Event
projectEvens =
    [ { title = "Personal project 1"
      , interval = Interval.withDurationMonths 2019 Date.Dec 3
      , description = text "Small app in Java"
      , category = Project
      , url = Just "https://github.com/OOP-Projects-2020-2021/ParkingLot-S"
      , tags = []
      , important = False
      }
    , { title = "Personal project 2"
      , interval = Interval.withDurationMonths 2019 Date.Nov 1
      , description = text "Medical facilty database application in Java"
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


personalDetails : PersonalDetails
personalDetails =
    { name = "Emilia Scutea"
    , intro = "I love Functional Programming"
    , contacts = [ DetailWithName "contact " "escutea@yahoo.ro" ]
    , socials = [ DetailWithName "github" "https://github.com/emiliascutea" ]
    }


initModel : Model
initModel =
    { personalDetails = personalDetails
    , events = Event.sortByInterval <| academicEvents ++ workEvents ++ projectEvens
    , selectedEventCategories = allSelected
    , repos = []
    }
