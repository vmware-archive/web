module SpacePipeline exposing (Flags, Model, Msg, init, update, view, subscriptions, changeToPipeline)

import Concourse
import Concourse.Job
import Concourse.Resource
import Html exposing (Html)
import Html.Attributes exposing (class, classList, href, rowspan, attribute)
import Http
import Navigation
import SpacePreview
import SpaceRoutes
import Task
import Time exposing (Time)
import StrictEvents exposing (onLeftClickOrShiftLeftClick)


type alias Model =
    { ports : Ports
    , pipelineLocator : Concourse.PipelineIdentifier
    , jobs : List Concourse.SpaceJob
    , resources : List Concourse.SpaceResource
    , selectedResourceSpaces : List ( String, String )
    , error : Maybe String
    , turbulenceImgSrc : String
    , previewModel : SpacePreview.Model
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
    | ResourceClickMsg ( String, String )
    | ResourceShiftClickMsg ( String, String )
    | PreviewMsg SpacePreview.Msg


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
            , selectedResourceSpaces = []
            , error = Nothing
            , turbulenceImgSrc = flags.turbulenceImgSrc
            , previewModel = SpacePreview.init
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

        ResourceHoverMsg ( resource, space ) ->
            let
                ( newPreviewModel, _ ) =
                    SpacePreview.update (SpacePreview.ResourceHoverMsg ( resource, space )) model.previewModel
            in
                ( { model
                    | previewModel = newPreviewModel
                  }
                , Cmd.none
                )

        ResourceClickMsg resourceSpace ->
            let
                newSelectedResourceSpaces =
                    if [ resourceSpace ] == model.selectedResourceSpaces then
                        []
                    else
                        [ resourceSpace ]

                ( newPreviewModel, _ ) =
                    SpacePreview.update (SpacePreview.ResourceHighlightMsg newSelectedResourceSpaces) model.previewModel
            in
                ( { model
                    | previewModel = newPreviewModel
                    , selectedResourceSpaces = newSelectedResourceSpaces
                  }
                , Cmd.none
                )

        ResourceShiftClickMsg resourceSpace ->
            let
                newSelectedResourceSpaces =
                    if List.member resourceSpace model.selectedResourceSpaces then
                        List.filter ((/=) resourceSpace) model.selectedResourceSpaces
                    else
                        resourceSpace :: model.selectedResourceSpaces

                ( newPreviewModel, _ ) =
                    SpacePreview.update (SpacePreview.ResourceHighlightMsg newSelectedResourceSpaces) model.previewModel
            in
                ( { model
                    | previewModel = newPreviewModel
                    , selectedResourceSpaces = newSelectedResourceSpaces
                  }
                , Cmd.none
                )

        PreviewMsg msg ->
            let
                ( newPreviewModel, newPreviewMsg ) =
                    SpacePreview.update msg model.previewModel

                newModel =
                    { model | previewModel = newPreviewModel }
            in
                case msg of
                    SpacePreview.NavTo url ->
                        ( newModel, Navigation.newUrl url )

                    _ ->
                        ( newModel, Cmd.map PreviewMsg newPreviewMsg )


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
        [ viewResources model
        , Html.map PreviewMsg <| SpacePreview.view model.previewModel model.jobs model.resources
        ]


viewResources : Model -> Html Msg
viewResources model =
    Html.div [ class "resources" ] <|
        List.map (\resource -> viewResource resource model.selectedResourceSpaces) <|
            List.sortBy .name model.resources


viewResource : Concourse.SpaceResource -> List ( String, String ) -> Html Msg
viewResource resource resourceSpaces =
    if List.length resource.spaces > 1 then
        Html.div [ class "resource" ]
            [ Html.div [ class "resource-name" ] [ Html.text resource.name ]
            , Html.div [ class "resource-spaces" ] <| List.map (\space -> viewResourceSpace ( resource.name, space ) resourceSpaces) resource.spaces
            ]
    else
        Html.div [ class "resource" ] <|
            let
                space =
                    Maybe.withDefault "default" <| List.head resource.spaces
            in
                [ Html.div
                    [ classList
                        [ ( "resource-name", True )
                        , ( "active", List.member ( resource.name, space ) resourceSpaces )
                        ]
                    , onLeftClickOrShiftLeftClick
                        (ResourceClickMsg ( resource.name, space ))
                        (ResourceShiftClickMsg ( resource.name, space ))
                    ]
                    [ Html.text resource.name ]
                ]


viewResourceSpace : ( String, String ) -> List ( String, String ) -> Html Msg
viewResourceSpace ( resource, space ) resourceSpaces =
    Html.div
        [ classList
            [ ( "resource-space", True )
            , ( "active", List.member ( resource, space ) resourceSpaces )
            ]
        , onLeftClickOrShiftLeftClick
            (ResourceClickMsg ( resource, space ))
            (ResourceShiftClickMsg ( resource, space ))
        ]
        [ Html.text space ]
