import Html exposing (Html, button)
-- import Html exposing (Html, input)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

-- import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Round
import Color.Convert exposing (colorToHex)
import Color


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = { time : Time, seconds : Int, attendees : String, rate : String, cost : Float, running : Bool}

init : (Model, Cmd Msg)
init =
  ({ time = 0, seconds = 0, attendees = "10", rate = "100", cost = 0, running = False}, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | Attendees String
  | Rate String
  | Bleed Bool
  | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime, seconds = model.seconds + 1, cost = cost model}, Cmd.none)
    Attendees str ->
      ({ model | attendees = str, seconds = 0}, Cmd.none)
    Rate str ->
      ({ model | rate = str, seconds = 0}, Cmd.none)
    Bleed bool ->
      ({ model | running = bool}, Cmd.none)
    Reset ->
      ({ model | seconds = 0, cost = 0}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.running then
    Time.every second Tick
  else
    Sub.none



-- VIEW

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.p [] [ text "Let It Bleed" ]
    , Html.p [] [ text "Attendees: ", Html.input [ placeholder "0", onInput Attendees ] [] ]
    , Html.p [] [ text "Rate: $", Html.input [ placeholder "0", onInput Rate ] [] ]
    , Html.p []
        [ radio "Run" (Bleed True)
        , radio "Pause" (Bleed False)
        , button [ onClick Reset ] [ text "Reset" ]
        ]
    , clock model
    , Html.p [] [ text "Elapsed: ", text (strElapsed model.seconds) ]
    , Html.p [] [ text "Cost: $", text (Round.round 2 model.cost) ]
    ]

radio : String -> msg -> Html msg
radio value msg =
  Html.label
    [ Html.Attributes.style [("padding", "20px")]
    ]
    [ Html.input [ Html.Attributes.type_ "radio", Html.Attributes.name "font-size", onClick msg ] []
    , text value
    ]

cost : Model -> Float
cost model = (toFloat model.seconds * toFloat (strToInt model.attendees) * toFloat (strToInt model.rate)) / 3600

strToInt : String -> Int
strToInt str = Result.withDefault 0 (String.toInt str)

strElapsed : Int -> String
strElapsed n =
  let
      hr = n // 3600
      min = n // 60 - hr * 60
      sec = n - hr * 3600 - min * 60
  in
      toString hr ++ "h " ++ toString min ++ "m " ++ toString sec ++ "s"

-- produce a hex version of an RGB based on a cost
colorCostHex : Float -> String
colorCostHex cost =
  let
      (r, g, b) = colorCostRGB cost
  in
      colorToHex (Color.rgb r g b)

-- produce an RGB hue that morphs from blue to red as the cost increases
colorCostRGB : Float -> ( Int, Int, Int )
colorCostRGB cost =
  let
      r = Basics.min 255 (round ((cost / 100) * 255))
      b = Basics.min 255 (Basics.max 0 (255 - (5*r)))
      g = Basics.max 0 ((255 - b) - 2*r)
  in
    (r, g, b)

clock : Model -> Html Msg
clock model =
  let
    angle =
      turns (Time.inMinutes model.time)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    svg [ viewBox "0 0 100 100", Svg.Attributes.width "100px" ]
      -- [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      [ circle [ cx "50", cy "50", r "45", fill (colorCostHex model.cost) ] []
      , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
      ]
