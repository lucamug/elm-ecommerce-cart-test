module Cart.State exposing (init, update, subscriptions)

import Cart.Types exposing (..)
import Cart.Http exposing (..)
import HttpBuilder exposing (..)


availableProducts : List Product
availableProducts =
    [ Product
        "1"
        "Free Delivery"
        (Just "http://files.rakuten-static.de/789653a5d4e1d8bf4e00a151560691bf/thumbs/280/11/103cbe07d8e08f72bee5c25b1f243315/r760r770-gear-s3-smartwatch.jpg")
        "Samsung S3 Smart Watch"
        329.9
        True
    , Product
        "2"
        "Free Delivery"
        (Just "http://files.rakuten-static.de/a80a984051650024bc9aba3c601fb83a/thumbs/280/8a/0bd461750bf5d2a3b76049c13e2ad088/4-st-und-uumlck-klappsessel-sessel-klappstuhl-santos-natur.jpg")
        "Folding Chair"
        189.95
        True
    , Product
        "3"
        "Free Delivery"
        (Just "http://files.rakuten-static.de/9b29a711b743e15206bc41bcc7604b50/thumbs/280/57/733d7d6c4458e093ca91b707bc17a835/5ksm7591xeer-heavy-duty-k-und-uumlchenmaschine-500w-6-9l-empire-rot.jpg")
        "KitchenAid 5KSM7591"
        579
        True
    , Product
        "4"
        "Free Delivery"
        (Just "http://files.rakuten-static.de/e9d9152203630452fef5a10bdbe6f01b/thumbs/280/2d/c4494d84e489e8022313b5190a8cf0c5/likeabike-jumper-von-kokua-like-a-bike-orange.jpg")
        "KOKUA LIKEaBIKE"
        194.96
        True
    , Product
        "5"
        "Free Delivery"
        (Just "http://files.rakuten-static.de/3ab313888b4d55bc32195930a0a00cfe/thumbs/280/3d/1228f1a814c0532816be9038e1994b2f/chefsessel-buerostuhl-drehstuhl-basel-kunstleder-schwarz.jpg")
        "Office Chair, Basel"
        94.99
        True
    , Product
        "6"
        "Free Delivery"
        (Just "http://files.rakuten-static.de/5ed78c0b121753b6d5b6de4dbd792a34/thumbs/280/2f/1b13682d0df545fb3c949a9216722fc7/fritzbox-7490-breitbandrouter-mit-bis-zu-450-mbits-und-ipv6-unterst-und-uumltzung.jpg")
        "AVM FRITZ! Box 7490"
        189.9
        True
    ]


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model (Cart []) availableProducts ProductList False Nothing Nothing

        command =
            (Cmd.none)
    in
        ( model, command )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


isSameItem : Item -> Item -> Bool
isSameItem item1 item2 =
    (item1.product.id == item2.product.id) && (item1.customization == item2.customization)


increaseItemQuantityForProduct : List Item -> Product -> List Item
increaseItemQuantityForProduct items product =
    let
        increaseQuantity =
            \item ->
                if (item.product.id == product.id) then
                    { item | quantity = item.quantity + 1 }
                else
                    item
    in
        List.map increaseQuantity items


addToCart : Cart -> Product -> Customization -> Cart
addToCart cart product customization =
    let
        quantity =
            Maybe.withDefault 1 customization.quantity

        updatedItems =
            List.append cart.items [ Item product quantity customization ]
    in
        { cart | items = (Debug.log "updated cart items" updatedItems) }


updateItemQuantity : Cart -> Item -> Int -> Cart
updateItemQuantity cart itemToUpdate quantity =
    let
        updatedItems =
            List.filterMap
                (\item ->
                    if (isSameItem item itemToUpdate) then
                        if quantity == 0 then
                            Nothing
                        else
                            Just { item | quantity = quantity }
                    else
                        Just item
                )
                cart.items
    in
        { cart | items = updatedItems }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddToCart product customization ->
            let
                model =
                    { model | cart = (addToCart model.cart product customization), page = Checkout }
            in
                ( model, Cmd.none )

        UpdateItemQuantity item quantity ->
            let
                model =
                    { model | cart = (updateItemQuantity model.cart item quantity) }
            in
                ( model, Cmd.none )

        RemoveItem item ->
            let
                model =
                    { model | cart = (updateItemQuantity model.cart item 0) }
            in
                ( model, Cmd.none )

        StartCheckout ->
            let
                model =
                    { model | page = Checkout }
            in
                ( model, Cmd.none )

        SelectProduct product ->
            let
                model =
                    { model | currentProduct = Just product, page = ProductDetail }
            in
                ( model, Cmd.none )

        GoHome ->
            let
                model =
                    { model | currentProduct = Nothing, currentCustomization = Nothing, page = ProductList }
            in
                ( model, Cmd.none )

        Customize customization ->
            let
                model =
                    { model | currentCustomization = Just customization }
            in
                ( model, Cmd.none )

        StartPurchase cart ->
            let
                model =
                    { model | purchaseInProgress = True }

                purchase =
                    Purchase cart.items

                command =
                    postPurchase purchase
            in
                ( model, command )

        PurchaseSucceed response ->
            let
                model =
                    { model | purchaseInProgress = False, page = CheckoutSuccess, cart = Cart [] }
            in
                ( model, Cmd.none )

        PurchaseFail error ->
            let
                model =
                    { model | purchaseInProgress = False }

                errorMessage =
                    case error of
                        HttpBuilder.BadResponse response ->
                            response.data

                        _ ->
                            "Something went wrong with the payment... Please try again."
            in
                ( model, Cmd.none )
