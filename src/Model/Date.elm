module Model.Date exposing (Date, Month(..), compare, compareMonth, full, month, monthToString, monthsBetween, monthsBetweenMonths, offsetMonths, onlyYear, view, year)

import Html exposing (..)
import Html exposing (text)
import Model.Util exposing (chainCompare)


type Date
    = Date { year : Int, month : Maybe Month }

monthsInYear = 12

year : Date -> Int
year (Date d) =
    d.year


month : Date -> Maybe Month
month (Date d) =
    d.month


full : Int -> Month -> Date
full y m =
    Date { year = y, month = Just m }


onlyYear : Int -> Date
onlyYear y =
    Date { year = y, month = Nothing }


{-| Given two `Date`s it returns the number of months between the two dates as an **absolute** value.
The month fields are handled as follows:

  - If both are present (`Just`), they are included normally in the calculation
  - If both are missing (`Nothing`), the number of years between the two dates is calculated
  - Otherwise the result is undefined (`Nothing`)

```
    monthsBetween (full 2020 Jan) (full 2020 Feb) --> Just 1
    monthsBetween (full 2020 Mar) (full 2020 Jan) --> Just 2

    monthsBetween (full 2020 Jan) (full 2021 Feb) --> Just 13
    monthsBetween (full 2021 Jan) (full 2020 Feb) --> Just 11

    monthsBetween (onlyYear 2020) (full 2021 Jan) --> Nothing
    monthsBetween (full 2020 Jan) (onlyYear 2021) --> Nothing

    monthsBetween (full 2020 Dec) (full 2021 Jan) --> Just 1
```

-}

calculateYears : Date -> Date -> Int
calculateYears dA dB = 
    let
        year1 = year(dA)

        year2 = year(dB)
    in
        abs (year1 - year2 - 1) * monthsInYear

calculateMonths : Date -> Date -> Int
calculateMonths dA dB =
    let
        month1 = monthToIntx(month(dA))

        month2 = monthToIntx(month(dB))

        year1 = year(dA)

        year2 = year(dB)

        monthsInYears = (year2 - year1 - 1) * monthsInYear
    in
       case (Basics.compare year1 year2) of
            EQ -> 
                abs(month2 - month1)
            LT -> 
                case (Basics.compare month1 month2) of
                    EQ -> 
                        monthsInYears
                    _ -> 
                        monthsInYears + (monthToInt(Dec) + 1 - month1) + month2                
            GT -> 
                calculateMonths dB dA


monthsBetween : Date -> Date -> Maybe Int
monthsBetween dA dB = 
    let
        hasMonth date = 
            case month(date) of
                Just mon -> 
                    True
                Nothing -> 
                    False
    in
        case (hasMonth dA) && hasMonth(dB) of 
            True -> 
                Just (calculateMonths dA dB) 
            False ->         
                case (hasMonth dA) || (hasMonth dB) of
                    True -> 
                        Nothing 
                    False -> 
                        Just (calculateYears dA dB)




{-| Compares two dates.
First, dates are compared by the year field. If it's equal, the month fields are used as follows:

  - If both are present (`Just`), they are compared the result is returned
  - If both are missing (`Nothing`), the dates are equal
  - Otherwise the date without month is greater

```
    Model.Date.compare (full 2020 Jan) (full 2021 Jan) --> LT
    Model.Date.compare (full 2021 Dec) (full 2021 Jan) --> GT

    Model.Date.compare (full 2020 Jan) (full 2020 Dec) --> LT
    Model.Date.compare (onlyYear 2020) (onlyYear 2021) --> LT

    Model.Date.compare (onlyYear 2020) (full 2020 Dec) --> GT
    Model.Date.compare (onlyYear 2019) (full 2020 Dec) --> LT
```

-}
getMonth : Maybe Month -> Int
getMonth mon =
    case mon of
        Just m -> monthToInt m 
        Nothing -> 12

compare : Date -> Date -> Order
compare (Date d1) (Date d2) =
    let
        year1 = d1.year
        year2 = d2.year
        month1 = d1.month
        month2 = d2.month
    in
        case (Basics.compare year1 year2) of
            LT -> LT
            GT -> GT
            EQ -> Basics.compare (getMonth month1) (getMonth month2)


{-| Given a current date and the number of months, it returns a new date with the given number of months passed.

-}
offsetMonths : Int -> Date -> Date
offsetMonths months (Date d) =
    let
        addMonths =
            modBy 12 months

        addedMonths =
            d.month
                |> Maybe.map monthToInt
                |> Maybe.map ((+) addMonths)

        newMonth =
            addedMonths
                |> Maybe.map (modBy 12)
                |> Maybe.andThen intToMonth

        addYears =
            months // 12

        extraYear =
            if Maybe.withDefault 0 addedMonths >= 12 then
                1

            else
                0
    in
    Date { year = d.year + addYears + extraYear, month = newMonth }


view : Date -> Html msg
view (Date d) =
    let
        parseMonth mon = 
            case mon of
                Just m -> monthToString m
                Nothing -> ""
    in
        div [] 
        [ h1[] [text ("Date : " ++ String.fromInt(d.year))]
        , h2[] [text (parseMonth d.month)]
        ]



-- MONTH


type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec

monthToIntx : Maybe Month -> Int
monthToIntx mon =
    case mon of
        Just Jan -> 1
        Just Feb -> 2
        Just Mar -> 3
        Just Apr -> 4
        Just May -> 5
        Just Jun -> 6
        Just Jul -> 7
        Just Aug -> 8
        Just Sep -> 9
        Just Oct -> 10
        Just Nov -> 11
        Just Dec -> 12
        Nothing -> 0

intToMonth : Int -> Maybe Month
intToMonth idx =
    case idx of
        0 ->
            Just Jan

        1 ->
            Just Feb

        2 ->
            Just Mar

        3 ->
            Just Apr

        4 ->
            Just May

        5 ->
            Just Jun

        6 ->
            Just Jul

        7 ->
            Just Aug

        8 ->
            Just Sep

        9 ->
            Just Oct

        10 ->
            Just Nov

        11 ->
            Just Dec

        _ ->
            Nothing


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


monthToString : Month -> String
monthToString m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"

monthToStringx :  Maybe Month -> String
monthToStringx mon = 
    case mon of
        Just Jan -> 
            "January"
        Just Feb -> 
            "February"
        Just Mar -> 
            "March"
        Just Apr -> 
            "April"
        Just May -> 
            "May"
        Just Jun -> 
            "June"
        Just Jul -> 
            "July"
        Just Aug -> 
            "August"
        Just Sep -> 
            "September"
        Just Oct -> 
            "October"
        Just Nov -> 
            "November"
        Just Dec -> 
            "December"
        Nothing  -> 
            ""

compareMonth : Month -> Month -> Order
compareMonth m1 m2 =
    Basics.compare (monthToInt m1) (monthToInt m2)


{-| Returns the number of months between two months as an **absolute** value.

    monthsBetweenMonths Jan Jan --> 0

    monthsBetweenMonths Jan Apr --> 3

    monthsBetweenMonths Apr Jan --> 3

-}
monthsBetweenMonths : Month -> Month -> Int
monthsBetweenMonths m1 m2 =
    let
        mon1 = monthToInt m1
        mon2 = monthToInt m2
    in
        abs(mon1 - mon2)
