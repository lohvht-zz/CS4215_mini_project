port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal

type alias Flags = {}

type alias Model =
  { navKey : Navigation.Key
  , pageSection : PageSection
  , navState : Navbar.State
  , modalVisibility : Modal.Visibility
  , isNavFixed : NavFix
  }

type NavFix = FixNav | FreeNav

port scrollNavData : (Bool -> msg) -> Sub msg
port scrollToTopId : String -> Cmd msg

type PageSection
  = IntroJumbo
  | Profile
  | Experience
  | Projects
  | Abilities
  | Contact

getSectionId : Maybe PageSection -> String
getSectionId pagesection =
  case pagesection of
    Just section ->
      case section of
        IntroJumbo -> "top-jumbo"
        Profile -> "profile"
        Experience -> "experience"
        Projects -> "projects"
        Abilities -> "abilities"
        Contact -> "contact"
    Nothing -> ""

type Msg
  = UrlChange Url
  | ClickedLink UrlRequest
  | NavMsg Navbar.State
  | CloseContactModal
  | ShowContactModal
  | OnScroll Bool

main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = ClickedLink
    , onUrlChange = UrlChange
    }

init : Flags -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags url key =
  let
    (navState, navCmd) = Navbar.initialState NavMsg
    
    (model, urlCmd) = urlUpdate url
      { navKey = key
      , navState = navState
      , pageSection = IntroJumbo
      , modalVisibility = Modal.hidden
      , isNavFixed = FixNav
      }
  in
    (model, Cmd.batch[urlCmd, navCmd])

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ scrollNavData OnScroll, Navbar.subscriptions model.navState NavMsg ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickedLink request ->
      case request of
        -- Internal urls like #profile etc.
        Browser.Internal url -> (model, Navigation.pushUrl model.navKey <| Url.toString url)
        -- Do not handle external hrefs in elm, do via html target="_blank"
        Browser.External href -> (model, Cmd.none)

    OnScroll navFixed ->
      ({ model | isNavFixed = if navFixed then FixNav else FreeNav }, Cmd.none) 
    UrlChange url -> urlUpdate url model
    NavMsg state -> ({ model | navState = state }, Cmd.none)
    CloseContactModal -> ({ model | modalVisibility = Modal.hidden }, Cmd.none)
    ShowContactModal -> ({ model | modalVisibility = Modal.shown }, Cmd.none)

urlUpdate : Url -> Model -> (Model, Cmd Msg)
urlUpdate url model =
  case decode url of
    Nothing ->
      ( { model | pageSection = IntroJumbo }, Cmd.none )

    Just route ->
      let sectionId = getSectionId(Just route) in
      ( { model | pageSection = route }, Cmd.batch [scrollToTopId sectionId] )


decode : Url -> Maybe PageSection
decode url =
  { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
  |> UrlParser.parse routeParser


routeParser : Parser (PageSection -> a) a
routeParser =
  UrlParser.oneOf
    [ UrlParser.map IntroJumbo top
    , UrlParser.map Profile (s "profile")
    , UrlParser.map Experience (s "experience")
    , UrlParser.map Projects (s "projects")
    , UrlParser.map Abilities (s "abilities")
    , UrlParser.map Contact (s "contact")
    ]

view : Model -> Browser.Document Msg
view model =
  { title = "Bill - Software Engineer"
  , body =
    [ div
      [ attribute "data-spy" "scroll"
      , attribute "data-target" "#navigation-bar"
      , attribute "data-offset" "0"    
      ]
      [ navMenu model
      , sectionJumbo model
      , sectionProfile model
      , sectionExperience model
      , sectionProjects model
      , sectionAbilities model
      , sectionContact model
      , contactModal model
      ]
    ]
  }

navMenu : Model -> Html Msg
navMenu model =
  Navbar.config NavMsg
    |> Navbar.withAnimation
    |> Navbar.collapseSmall
    |> Navbar.items
      [ Navbar.itemLink [ href "#profile" ] [ text "Profile" ]
      , Navbar.itemLink [ href "#experience" ] [ text "Experience" ]
      , Navbar.itemLink [ href "#projects" ] [ text "Projects" ]
      , Navbar.itemLink [ href "#abilities" ] [ text "Abilities" ]
      , Navbar.itemLink [ href "#contact" ] [ text "Contact" ]
      ]
    |> Navbar.attrs
      [ id "navigation-bar"
      , classList [("fixed", model.isNavFixed == FixNav)]
      ]
    |> Navbar.view model.navState

sectionJumbo : Model -> Html Msg
sectionJumbo model =
  div [ class "jumbotron", id (getSectionId (Just IntroJumbo)) ]
    [ Grid.container []
      [ h1 [] [ text "Hey There, My name is Bill"]
      , p [ class "lead" ] [ text "Click On The Arrow To Learn More About Me!" ]
      ]
    , div [ class "overlay" ] []
    , a [ href "#profile", class "scroll-down", id "scroll-down-button" ]
      [ span [ class "fa fa-chevron-down" ] [] ]
    ]

sectionProfile : Model -> Html Msg
sectionProfile model =
  div [ class "background-white" ]
    [ Grid.container [ id (getSectionId (Just Profile)) ]
      [ similarSectionHeader "Profile" [ text "I am a passionate fullstack developer" ]
      , Grid.row []
        [ Grid.col [Col.md4]
          [ h3 [] [ text "About Me" ]
          , p [] [ text "I am a web developer a burning passion for architecture and clean code."]
          ]
        , Grid.col [Col.md4, Col.attrs[ class "text-center" ] ]
          [ img
            [ class "profile-picture"
            , src "images/profile_picture.jpg"
            , alt "Bill Chang"
            , width 180
            , height 180
            ] []
          ]
        , Grid.col [Col.md4]
          [ h3 [] [ text "Details" ]
          , p []
            (List.concat[ profileDetails "Name:" "Bill Chang"
            , [ br [] [] ]
            , profileDetails "Age:" "24"
            , [ br [] [] ]
            , profileDetails "Interests:" "Taking long walks on the beach"
            , [ br [] [] ]
            , profileDetails "Location:" "Singapore"
            ])
          ]
        ]
      ]
    ]

profileDetails : String -> String -> List (Html Msg)
profileDetails header details =
  [ strong [] [ text header ], br [] [], text details ]

sectionExperience : Model -> Html Msg
sectionExperience model =
  Grid.container [ id (getSectionId (Just Experience)) ]
    [ similarSectionHeader "Experience"
      [ text "“The purpose of life is to live it, to taste experience to the utmost, to reach out eagerly and without fear for newer and richer experience.”"
      , br [] []
      , text "- Eleanor Roosevelt"
      ]
    , h3 [] [ text "Education" ]
    , experienceSubsectionRow
      "National University of Singapore"
      "Aug 2015 - Present"
      "Bachelor of Computing (Honours) in Computer Science"
      """Here I studied and learnt a wide range and depth of computer science
      knowledge from basic programming, OS and networking principles
      among others. It is in this course that got me really interested in
      Software Engineering principles and how to architect and structure code
      to improve maintainability."""
    , experienceSubsectionRow
      "NUS Overseas Colleges In Silicon Valley"
      "Jan 2017 - Dec 2017"
      "Overseas internship and entrepreneurship course programme"
      """Year long programme overseas hosted by NUS enterprise to promote
      entrepreneurship. Here I got a taste of working life inside a startup
      company as well as knowledge on how businesses and startups work
      within the Silicon Valley business ecosystem."""
    , experienceSubsectionRow
      "St. Andrew's Junior College"
      "Jan 2011 - Dec 2012"
      "Junior College (GCE A Levels)"
      """Went to junior college where I studied Physics, Mathematics, Chemistry
      and Economics. Realised during my last year in junior college that
      I was interested in both technology and mathematics, thus I
      started learning about algorithms by myself."""
    , hr [] []
    , h3 [] [ text "Work" ]
    , experienceSubsectionRow
      "Epic! Creations Inc."
      "Jan 2017 - Dec 2017"
      "Full Stack Developer Intern"
      """Worked as an intern in a children's ebook and digital media startup,
      where I collaborated with the team to work on many different projects, such
      as integrating APIs of different services like SphinxQL and Stripe, or to
      push out new features on Epic!'s web app. Did full stack development using
      AngularJS, PHP, MySQL and nginx. Also led the adoption and
      development of AngularJS for the company's backend admin dashboard."""
    , experienceSubsectionRow
      "National University of Singapore"
      "Aug 2016 - Dec 2016"
      "Teaching Assistant (Software Engineering)"
      """Create teaching materials used to conduct tutorial lessons based on
      software engineering concepts and principles. Facillitate discussion with
      students by asking guided questions to help students furthur their
      understanding of course material. Also mentored
      teams of 4 undergraduates in their software engineering project."""
    ]

experienceSubsectionRow : String -> String -> String -> String -> Html Msg
experienceSubsectionRow institutionName yearsPresent titleName description =
  Grid.row [ Row.attrs [class "experience-subsection"] ]
    [ Grid.col [ Col.md4 ]
      [ h4 [] [ text institutionName ]
      , p [ class "experience-period" ] [ text yearsPresent ]
      ]
    , Grid.col [ Col.md8 ]
      [ p []
        [ strong [] [ text titleName ]
        , span [ class "hidden-phone" ] [ text description ]
        ]
      ]
    ]

sectionProjects : Model -> Html Msg
sectionProjects model =
  div [ class "background-white" ]
    [ Grid.container [ id (getSectionId (Just Projects)) ]
      [ similarSectionHeader "Projects"
        [ text "“Others have seen what is and asked why. I have seen what could be and asked why not.”"
        , br [] []
        , text "- Pablo Picasso"
        ]
      , projectPreviewLink
        [ text "Maru" ]
        "images/maru_gameplay.gif"
        "Maru Gameplay"
        "https://github.com/MRSASPO/Project-Bong-Bong/releases/tag/v0.3"
        [ text "Maru, a 2D Platformer inspired by Mario and Super Meatball" ]
        [ "Unity", "C#" ]
      , div [ class "spacer" ] []
      , projectPreviewLink
        [ text "The ", i [] [strong [] [ text "Jimple" ]], text " Planner" ]
        "images/jimple_planner_demo.gif"
        "Demo of Jimple Planner"
        "https://github.com/cs2103jan2016-f14-2j/main/releases/tag/v0.5"
        [ text "Minimal text based task tracker, built with the working man,"
        , i [] [ text "Jim" ], text " in mind"
        ]
        [ "Java" ]
      ]
    ]

projectPreviewLink : List(Html Msg) -> String -> String -> String -> List(Html Msg) -> List(String) -> Html Msg
projectPreviewLink title previewImgLink previewAltText linkToProject caption tags =
  div []
    [ figure [ class "effect" ]
      [ img [ class "img-responsive", src previewImgLink, alt previewAltText ] []
      , figcaption []
        [ h3 [] title
        , p [] caption
        , p [] [ strong [] [text "Tags:"], br [] [], text (String.join ", " tags) ]
        ]
      , a [ href linkToProject, target "_blank" ] [ text "View More" ]
      , span [ class "icon" ]
        [ span [ class "fa fa-external-link-alt" ] [] ]
      ]
    ]

sectionAbilities : Model -> Html Msg
sectionAbilities model =
  Grid.container [ id (getSectionId (Just Abilities))  ]
    [ similarSectionHeader
      "Abilities"
      [ text """“Wisdom is not a product of schooling but of the lifelong attempt
       to acquire it.”"""
      , br [] []
      , text "- Albert Einstein"
      ]
    , h3 [] [ text "Technical Skills" ]
    , Grid.row []
      [ Grid.col [ Col.md4 ]
        [ h4 [] [ text "Programming Languages" ]
        , ul [ class "no-bullets" ]
          [ li [] [ text "Javascript"]
          , li [] [ text "Python"]
          , li [] [ text "Java"]
          , li [] [ text "PHP"]
          , li [] [ text "C/C++"]
          ]
        ]
      , Grid.col [ Col.md4 ]
        [ h4 [] [ text "Frontend" ]
        , ul [ class "no-bullets" ]
          [ li [] [ text "HTML"]
          , li [] [ text "CSS"]
          , li [] [ text "Bootstrap"]
          , li [] [ text "React"]
          , li [] [ text "AngularJS"]
          ]
        ]
      , Grid.col [ Col.md4 ]
        [ h4 [] [ text "Backend" ]
        , ul [ class "no-bullets" ]
          [ li [] [ text "Node.js"]
          , li [] [ text "Express"]
          ]
        ]
      ]
    , Grid.row []
      [ Grid.col [ Col.md4 ]
        [ h4 [] [ text "Databases" ]
        , ul [ class "no-bullets" ]
          [ li [] [ text "MySQL"]
          , li [] [ text "Postgres"]
          ]
        ]
      , Grid.col [ Col.md4 ]
        [ h4 [] [ text "Tools" ]
        , ul [ class "no-bullets" ]
          [ li [] [ text "Webpack"]
          , li [] [ text "Docker"]
          , li [] [ text "Gitlab CI"]
          , li [] [ text "Rancher"]
          , li [] [ text "Git"]
          ]
        ]
      , Grid.col [ Col.md4 ]
        [ h4 [] [ text "Cloud Services / API integerations" ]
        , ul [ class "no-bullets" ]
          [ li [] [ text "Stripe PHP SDK"]
          , li [] [ text "Basic AWS Management"]
          ]
        ]
      ]
    , hr [] []
    , h3 [] [ text "Language Proficiencies" ]
    , Grid.row []
      [ Grid.col [ Col.md6 ]
        [ ul [ class "no-bullets" ]
          [ li []
            [ span [ class "ability-title" ] [ strong [] [ text "English (Native)" ] ]
            , span [ class "ability-score" ] [ text "Fluent" ]
            ]
          , li []
            [ span [ class "ability-title" ] [ strong [] [ text "Mandarin (Mother Tongue)" ] ]
            , span [ class "ability-score" ] [ text "Intermediate" ]
            ]
          ]
        ]
      , Grid.col [ Col.md6 ]
        [ ul [ class "no-bullets" ]
          [ li []
            [ span [ class "ability-title" ] [ strong [] [ text "Japanese" ] ]
            , span [ class "ability-score" ] [ text "Beginner" ]
            ]
          ]
        ]
      ]
    ]

sectionContact : Model -> Html Msg
sectionContact model =
  div [ class "background-gray" ]
    [ Grid.container [ id (getSectionId (Just Contact))  ]
      [ similarSectionHeader
        "Contact"
        [ text "Want to know more about me?" ]
      , Grid.row []
        [ Grid.col [ Col.md6 ]
          [ ul [ class "no-bullets" ]
            [ li []
              [ a [ href "https://github.com/lohvht", target "_blank" ]
                [ i [ class "fab fa-github"] []
                , span [] [ text "See more of my projects!"]
                ]
              ]
            , li []
              [ a
                [ style "cursor" "pointer"
                , onClick ShowContactModal
                ]
                [ i [ class "far fa-envelope-open"] []
                , span [] [ text "Get in touch!"]
                ]
              ]
            ]
          ]
        , Grid.col [ Col.md6 ]
          [ ul [ class "no-bullets" ]
            [ li []
              [ a [ href "mailto:vic94loh@hotmail.com", target "_blank" ]
                [ i [ class "far fa-envelope"] []
                , span [] [ text "Or send me an email!"]
                ]
              ]
            ]
          ]
        ]
      , hr [] []
      ]
    ]

contactModal : Model -> Html Msg
contactModal model =
  Modal.config CloseContactModal
  |> Modal.h3 [] [ text "Keep In Touch!" ]
  |> Modal.body []
    [ span [] [ text "Interested in working together? Fill out the form below with some info about yourself and I will get back to you as soon as I can!" ]
    , Html.form [ id "contactForm", method "post", action "https://httpbin.org/post" ]
      [ div [ class "form-group "]
        [ label
          [ for "first-name", class "col-form-label" ]
          [ text "Name", span [ style "color" "red" ] [ text "*" ] ]
        , input
          [ class "form-control"
          , id "first-name"
          , name "firstName"
          , minlength 1
          , maxlength 255
          , type_ "text"
          , required True
          ] []
        ]
      , div [ class "form-group "]
        [ label
          [ for "email-address", class "col-form-label" ]
          [ text "Email Address", span [ style "color" "red" ] [ text "*" ] ]
        , input
          [ class "form-control"
          , id "email-address"
          , name "emailAddress"
          , minlength 1
          , maxlength 255
          , type_ "email"
          , required True
          ] []
        ]
      , div [ class "form-group "]
        [ label
          [ for "subject", class "col-form-label" ]
          [ text "Subject", span [ style "color" "red" ] [ text "*" ] ]
        , input
          [ class "form-control"
          , id "subject"
          , name "subject"
          , minlength 1
          , maxlength 255
          , type_ "text"
          , required True
          ] []
        ]
      , div [ class "form-group "]
        [ label
          [ for "message", class "col-form-label" ]
          [ text "Message", span [ style "color" "red" ] [ text "*" ] ]
        , textarea
          [ class "form-control"
          , id "message"
          , name "message"
          , minlength 1
          , maxlength 255
          , required True
          ] []
        ]
      , span [ style "color" "red" ] [ text "*" ]
      , i [] [ text "Indicates required fields" ] 
      , div [ class "modal-footer" ]
        [ Button.button [ Button.secondary, Button.attrs [ onClick CloseContactModal ] ]
          [ text "Close" ]
        , input [ class "btn btn-primary", type_ "submit", value "Submit"] []
        ]
      ]
    ]
  |> Modal.view model.modalVisibility




similarSectionHeader : String -> List(Html Msg) -> Html Msg
similarSectionHeader headerTitle headerBody =
  div [] 
    [h2 [] [ text headerTitle ]
    , p [ class "lead" ] headerBody
    , hr [] []
    ]
