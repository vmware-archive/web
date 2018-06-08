module SpacePipeline exposing (Flags, Model, Msg, init, update, view, subscriptions, changeToPipeline)

import Concourse
import Concourse.Job
import Concourse.Resource
import Html exposing (Html)
import Html.Attributes exposing (class, classList, href, rowspan, attribute)
import Html.Events exposing (onMouseOver, onMouseLeave)
import Http
import SpaceRoutes
import Task
import Time exposing (Time)
import SpacePreview


type alias Model =
    { ports : Ports
    , pipelineLocator : Concourse.PipelineIdentifier
    , jobs : List Concourse.SpaceJob
    , resources : List Concourse.SpaceResource
    , error : Maybe String
    , turbulenceImgSrc : String
    , selectedResourceSpace : ( String, String )
    }


type alias Ports =
    { title : String -> Cmd Msg
    }


type alias Flags =
    { teamName : String
    , pipelineName : String
    , turbulenceImgSrc : String
    , route : SpaceRoutes.ConcourseRoute
    }


type Msg
    = Noop
    | AutoupdateTimerTicked Time
    | JobsFetched (Result Http.Error (List Concourse.SpaceJob))
    | ResourcesFetched (Result Http.Error (List Concourse.SpaceResource))
    | ResourceHoverMsg ( String, String )


init : Ports -> Flags -> ( Model, Cmd Msg )
init ports flags =
    let
        model =
            { ports = ports
            , pipelineLocator =
                { teamName = flags.teamName
                , pipelineName = flags.pipelineName
                }
            , jobs = []
            , resources = []
            , error = Nothing
            , turbulenceImgSrc = flags.turbulenceImgSrc
            , selectedResourceSpace = ( "", "" )
            }
    in
        ( model
        , Cmd.batch
            [ fetchJobs model.pipelineLocator
            , fetchResources model.pipelineLocator
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        AutoupdateTimerTicked timestamp ->
            ( model
            , Cmd.batch
                [ fetchJobs model.pipelineLocator
                , fetchResources model.pipelineLocator
                ]
            )

        JobsFetched (Ok jobs) ->
            ( { model | jobs = jobs }, Cmd.none )

        JobsFetched (Err msg) ->
            ( { model | error = Just (toString msg) }, Cmd.none )

        ResourcesFetched (Ok resources) ->
            ( { model | resources = resources }, Cmd.none )

        ResourcesFetched (Err msg) ->
            ( { model | error = Just (toString msg) }, Cmd.none )

        ResourceHoverMsg resourceSpace ->
            ( { model | selectedResourceSpace = resourceSpace }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (5 * Time.second) AutoupdateTimerTicked ]


fetchJobs : Concourse.PipelineIdentifier -> Cmd Msg
fetchJobs pid =
    Task.attempt JobsFetched <|
        Concourse.Job.fetchSpaceJobs pid


fetchResources : Concourse.PipelineIdentifier -> Cmd Msg
fetchResources pid =
    Task.attempt ResourcesFetched <|
        Concourse.Resource.fetchSpaceResources pid


changeToPipeline : Flags -> Model -> ( Model, Cmd Msg )
changeToPipeline flags model =
    let
        pid =
            { teamName = flags.teamName
            , pipelineName = flags.pipelineName
            }
    in
        if model.pipelineLocator == pid then
            ( model, Cmd.none )
        else
            init model.ports flags


view : Model -> Html Msg
view model =
    Html.div [ class "pipeline-content" ]
        [ viewResources model.resources
        , SpacePreview.view model.jobs model.resources model.selectedResourceSpace
        ]


viewResources : List Concourse.SpaceResource -> Html Msg
viewResources resources =
    Html.div [ class "resources" ] <| List.map (\resource -> viewResource resource) resources


viewResource : Concourse.SpaceResource -> Html Msg
viewResource resource =
    if List.length resource.spaces > 1 then
        Html.div [ class "resource" ]
            [ Html.div [ class "resource-name" ] [ Html.text resource.name ]
            , Html.div [ class "resource-spaces" ] <| List.map (\space -> viewResourceSpace ( resource.name, space )) resource.spaces
            ]
    else
        Html.div [ class "resource" ]
            [ Html.div
                [ class "resource-name space-mouseover", onMouseOver (ResourceHoverMsg ( resource.name, Maybe.withDefault "default" <| List.head resource.spaces )), onMouseLeave (ResourceHoverMsg ( "", "" )) ]
                [ Html.text resource.name ]
            ]


viewResourceSpace : ( String, String ) -> Html Msg
viewResourceSpace ( resource, space ) =
    Html.div
        [ class "resource-space space-mouseover", onMouseOver (ResourceHoverMsg ( resource, space )), onMouseLeave (ResourceHoverMsg ( "", "" )) ]
        [ Html.text space ]
