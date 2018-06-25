port module SpaceSubPage exposing (Model(..), Msg(..), init, update, view, subscriptions, urlUpdate)

import Autoscroll
import Concourse
import Concourse.Pipeline
import Html exposing (Html)
import Http
import NoPipeline
import NotFound
import QueryString
import SpaceBuild
import SpacePipeline
import SpaceRoutes
import String
import Task
import UpdateMsg exposing (UpdateMsg)


-- TODO: move ports somewhere else


port setTitle : String -> Cmd msg


type Model
    = WaitingModel SpaceRoutes.ConcourseRoute
    | NoPipelineModel
    | NotFoundModel NotFound.Model
    | SpacePipelineModel SpacePipeline.Model
    | SpaceBuildModel (Autoscroll.Model SpaceBuild.Model)


type Msg
    = PipelinesFetched (Result Http.Error (List Concourse.Pipeline))
    | DefaultPipelineFetched (Maybe Concourse.Pipeline)
    | NoPipelineMsg NoPipeline.Msg
    | SpacePipelineMsg SpacePipeline.Msg
    | SpaceBuildMsg (Autoscroll.Msg SpaceBuild.Msg)
    | NewCSRFToken String


superDupleWrap : ( a -> b, c -> d ) -> ( a, Cmd c ) -> ( b, Cmd d )
superDupleWrap ( modelFunc, msgFunc ) ( model, msg ) =
    ( modelFunc model, Cmd.map msgFunc msg )


queryGroupsForRoute : SpaceRoutes.ConcourseRoute -> List String
queryGroupsForRoute route =
    QueryString.all "groups" route.queries


init : String -> SpaceRoutes.ConcourseRoute -> ( Model, Cmd Msg )
init turbulencePath route =
    case route.logical of
        SpaceRoutes.SpacePipeline teamName pipelineName ->
            superDupleWrap ( SpacePipelineModel, SpacePipelineMsg ) <|
                SpacePipeline.init
                    { title = setTitle
                    }
                    { teamName = teamName
                    , pipelineName = pipelineName
                    , turbulenceImgSrc = turbulencePath
                    , route = route
                    }

        SpaceRoutes.SpaceBuild teamName pipelineName jobName jobCombinationID buildName ->
            superDupleWrap ( SpaceBuildModel, SpaceBuildMsg ) <|
                Autoscroll.init
                    SpaceBuild.getScrollBehavior
                    << SpaceBuild.init
                        { title = setTitle }
                        { csrfToken = "", hash = route.hash }
                <|
                    SpaceBuild.SpaceJobBuildPage
                        { teamName = teamName
                        , pipelineName = pipelineName
                        , jobName = jobName
                        , jobCombinationID = (Result.withDefault 0 (String.toInt jobCombinationID))
                        , buildName = buildName
                        }

        SpaceRoutes.SpaceHome ->
            ( WaitingModel route
            , Cmd.batch
                [ fetchPipelines
                , setTitle ""
                ]
            )


handleNotFound : String -> ( a -> Model, c -> Msg ) -> ( a, Cmd c, Maybe UpdateMsg ) -> ( Model, Cmd Msg )
handleNotFound notFound ( mdlFunc, msgFunc ) ( mdl, msg, outMessage ) =
    case outMessage of
        Just UpdateMsg.NotFound ->
            ( NotFoundModel { notFoundImgSrc = notFound }, setTitle "Not Found " )

        Nothing ->
            superDupleWrap ( mdlFunc, msgFunc ) <| ( mdl, msg )


update : String -> String -> Concourse.CSRFToken -> Msg -> Model -> ( Model, Cmd Msg )
update turbulence notFound csrfToken msg mdl =
    case ( msg, mdl ) of
        ( NoPipelineMsg msg, model ) ->
            ( model, fetchPipelines )

        ( DefaultPipelineFetched pipeline, WaitingModel route ) ->
            case pipeline of
                Nothing ->
                    ( NoPipelineModel, setTitle "" )

                Just p ->
                    let
                        flags =
                            { teamName = p.teamName
                            , pipelineName = p.name
                            , turbulenceImgSrc = turbulence
                            , route = route
                            }
                    in
                        if String.startsWith "/space" (SpaceRoutes.toString route.logical) then
                            superDupleWrap ( SpacePipelineModel, SpacePipelineMsg ) <|
                                SpacePipeline.init { title = setTitle } flags
                        else
                            superDupleWrap
                                ( SpacePipelineModel, SpacePipelineMsg )
                            <|
                                SpacePipeline.init { title = setTitle } flags

        ( NewCSRFToken _, _ ) ->
            ( mdl, Cmd.none )

        ( SpacePipelineMsg message, SpacePipelineModel model ) ->
            superDupleWrap ( SpacePipelineModel, SpacePipelineMsg ) <| SpacePipeline.update message model

        ( SpaceBuildMsg message, SpaceBuildModel model ) ->
            handleNotFound notFound ( SpaceBuildModel, SpaceBuildMsg ) (Autoscroll.update SpaceBuild.updateWithMessage message model)

        unknown ->
            flip always (Debug.log ("impossible combination") unknown) <|
                ( mdl, Cmd.none )


urlUpdate : SpaceRoutes.ConcourseRoute -> Model -> ( Model, Cmd Msg )
urlUpdate route model =
    case ( route.logical, model ) of
        ( SpaceRoutes.SpacePipeline team pipeline, SpacePipelineModel mdl ) ->
            superDupleWrap ( SpacePipelineModel, SpacePipelineMsg ) <|
                SpacePipeline.changeToPipeline
                    { teamName = team
                    , pipelineName = pipeline
                    , turbulenceImgSrc = mdl.turbulenceImgSrc
                    , route = route
                    }
                    mdl

        ( SpaceRoutes.SpaceBuild teamName pipelineName jobName jobCombinationID buildName, SpaceBuildModel scrollModel ) ->
            let
                ( submodel, subcmd ) =
                    SpaceBuild.changeToBuild
                        (SpaceBuild.SpaceJobBuildPage
                            { teamName = teamName
                            , pipelineName = pipelineName
                            , jobName = jobName
                            , jobCombinationID = Result.withDefault 0 <| String.toInt jobCombinationID
                            , buildName = buildName
                            }
                        )
                        scrollModel.subModel
            in
                ( SpaceBuildModel { scrollModel | subModel = submodel }
                , Cmd.map SpaceBuildMsg (Cmd.map Autoscroll.SubMsg subcmd)
                )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view mdl =
    case mdl of
        WaitingModel _ ->
            Html.div [] []

        NoPipelineModel ->
            Html.map NoPipelineMsg <| NoPipeline.view

        NotFoundModel model ->
            NotFound.view model

        SpacePipelineModel model ->
            Html.map SpacePipelineMsg <| SpacePipeline.view model

        SpaceBuildModel model ->
            Html.map SpaceBuildMsg <| Autoscroll.view SpaceBuild.view model


subscriptions : Model -> Sub Msg
subscriptions mdl =
    case mdl of
        NoPipelineModel ->
            Sub.map NoPipelineMsg <| NoPipeline.subscriptions

        WaitingModel _ ->
            Sub.none

        NotFoundModel _ ->
            Sub.none

        SpacePipelineModel model ->
            Sub.map SpacePipelineMsg <| SpacePipeline.subscriptions model

        SpaceBuildModel model ->
            Sub.map SpaceBuildMsg <| Autoscroll.subscriptions SpaceBuild.subscriptions model


fetchPipelines : Cmd Msg
fetchPipelines =
    Task.attempt PipelinesFetched Concourse.Pipeline.fetchPipelines
