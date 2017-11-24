module Concourse.Login exposing (..)

import Base64
import Http
import Json.Decode
import Task exposing (Task)
import Concourse


noAuth : String -> Task Http.Error Concourse.AuthSession
noAuth teamName =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/teams/" ++ teamName ++ "/auth/token"
            , headers = []
            , body = Http.emptyBody
            , expect = Http.expectStringResponse parseResponse
            , timeout = Nothing
            , withCredentials = True
            }


parseResponse : Http.Response String -> Result String Concourse.AuthSession
parseResponse response =
    let
        authToken =
            Json.Decode.decodeString Concourse.decodeAuthToken response.body

        csrfToken =
            Concourse.retrieveCSRFToken response.headers
    in
        flip always (Debug.log "authToken, csrfToken" ( authToken, csrfToken )) <| Result.map2 (\a b -> { authToken = a, csrfToken = b }) authToken csrfToken


basicAuth : String -> String -> String -> Task Http.Error Concourse.AuthSession
basicAuth teamName username password =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/teams/" ++ teamName ++ "/auth/token"
            , headers = [ encodedAuthHeader username password ]
            , body = Http.emptyBody
            , expect = Http.expectStringResponse parseResponse
            , timeout = Nothing
            , withCredentials = True
            }


encodedAuthHeader : String -> String -> Http.Header
encodedAuthHeader username password =
    Http.header "Authorization" <|
        case Base64.encode (username ++ ":" ++ password) of
            Ok code ->
                "Basic " ++ code

            Err err ->
                -- hacky but prevents a lot of type system pain
                "!!! error: " ++ err
