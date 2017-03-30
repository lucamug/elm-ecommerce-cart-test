module Cart.Http exposing (..)

import Cart.Types exposing (..)
import Json.Encode
import HttpBuilder exposing (..)
import Task
import Dict
import Process


postItems : Purchase -> Cmd Msg
postItems purchase =
    let
        items =
            purchase.items
                |> List.map
                    (\item ->
                        Json.Encode.object
                            [ ( "id", Json.Encode.string item.product.id )
                            , ( "quantity", Json.Encode.int item.quantity )
                            , ( "email", Json.Encode.string (Maybe.withDefault "" item.customization.email) )
                            ]
                    )

        body =
            [ ( "items", Json.Encode.list items )
            ]

        request =
            HttpBuilder.post "/api/purchases"
                |> withJsonBody (Json.Encode.object body)
                |> withHeader "Content-Type" "application/json"
                |> send stringReader stringReader
    in
        Task.perform PurchaseFail PurchaseSucceed request


postPurchase : Purchase -> Cmd Msg
postPurchase purchase =
    let
        response =
            { data = "", status = 200, statusText = "", headers = (Dict.fromList [ ( "", "" ) ]), url = "" }

        task =
            (Process.sleep 3000) `Task.andThen` (\n -> (Task.succeed response))
    in
        Task.perform PurchaseFail PurchaseSucceed task
