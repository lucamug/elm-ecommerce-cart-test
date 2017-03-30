module Cart.View exposing (view)

import Cart.Types exposing (..)
import Html exposing (div, h1, text, ul, li, input, button, header, a, img, form, label, span)
import Html.Attributes exposing (id, class, attribute, value, type', href, src, style, disabled, pattern)
import Html.Events exposing (onClick, onInput, onSubmit)
import Numeral as Numeral
import String


defaultProductPictureURL : String
defaultProductPictureURL =
    "https://placeholdit.imgix.net/~text?txtsize=33&txt=Product&w=300&h=300"


productViewInStock : Product -> Html.Html Msg
productViewInStock product =
    li [ class "elm-cart-product" ]
        [ div [ onClick (SelectProduct product) ]
            [ div [ class "elm-cart-product-image" ]
                [ img [ src (getProductPictureURL product) ] []
                ]
            , div [ class "elm-cart-product-name" ] [ text product.name ]
            , div [ class "elm-cart-product-price elm-cart-button" ] [ text (formatCurrency product.price) ]
            ]
        ]


productViewOutOfStock : Product -> Html.Html Msg
productViewOutOfStock product =
    li [ class "elm-cart-product" ]
        [ div []
            [ div [ class "elm-cart-product-image" ]
                [ img [ src (getProductPictureURL product) ] []
                ]
            , div [ class "elm-cart-product-name" ] [ text product.name ]
            , div [ class "elm-cart-product-price elm-cart-button" ] [ text ("OUT OF STOCK") ]
            ]
        ]


productView : Product -> Html.Html Msg
productView product =
    if product.isInStock then
        (productViewInStock product)
    else
        (productViewOutOfStock product)


productsView : List Product -> Html.Html Msg
productsView products =
    div []
        [ ul [ class "elm-cart-products row" ] (List.map productView products)
        ]


shoppingCartItemCustomizationView : Item -> Html.Html Msg
shoppingCartItemCustomizationView item =
    let
        emailValue =
            Maybe.withDefault "N/A" item.customization.email
    in
        ul [ class "elm-cart-sc-item-customization" ]
            [ li []
                [ text ("Email: " ++ emailValue)
                ]
            ]


shoppingCartItemQuantityView : Item -> Html.Html Msg
shoppingCartItemQuantityView item =
    div [ class "elm-cart-sc-item-quantity" ]
        [ text (toString item.quantity)
        ]


shoppingCartItemNameView : Item -> Html.Html Msg
shoppingCartItemNameView item =
    div [ class "elm-cart-sc-item-name" ]
        [ text item.product.name
        ]


shoppingCartRemoveItemView : Item -> Html.Html Msg
shoppingCartRemoveItemView item =
    div
        [ class "elm-cart-sc-item-remove"
        , onClick (RemoveItem item)
        ]
        [ text "X" ]


shoppingCartItemView : Item -> Html.Html Msg
shoppingCartItemView item =
    li [ class "elm-cart-sc-item" ]
        [ shoppingCartItemQuantityView item
        , shoppingCartItemNameView item
        , shoppingCartItemCustomizationView item
        , shoppingCartRemoveItemView item
        ]


shoppingCartView : Cart -> Html.Html Msg
shoppingCartView cart =
    div [ class "elm-cart-sc" ]
        [ h1 [] [ text "Shopping Bag" ]
        , ul [ class "elm-cart-sc-items" ] (List.map shoppingCartItemView cart.items)
        ]


productListView : Model -> Html.Html Msg
productListView model =
    div []
        [ productsView model.availableProducts
        ]


formatCurrency : Float -> String
formatCurrency amount =
    (Numeral.format "0,00.00" amount) ++ " €"


totalView : Model -> Html.Html Msg
totalView model =
    let
        total =
            model.cart.items
                |> List.map (\item -> item.product.price * (toFloat item.quantity))
                |> List.sum
    in
        div [ class "elm-cart-sc-total" ]
            [ div [ class "elm-cart-sc-total-text" ]
                [ text "Total" ]
            , div
                [ class "elm-cart-sc-total-number" ]
                [ text (formatCurrency total) ]
            ]


loaderView : Model -> Html.Html Msg
loaderView model =
    case model.purchaseInProgress of
        True ->
            div []
                [ img
                    [ class "elm-cart-loader"
                    , src "img/loader.gif"
                    ]
                    []
                ]

        False ->
            div [] []


purchaseView : Model -> Html.Html Msg
purchaseView model =
    div [ class "elm-purchase" ]
        [ button
            [ class "elm-cart-button"
            , disabled model.purchaseInProgress
            , onClick <| StartPurchase <| model.cart
            ]
            [ text "Checkout" ]
        ]


checkoutView : Model -> Html.Html Msg
checkoutView model =
    div [ class "elm-cart-checkout" ]
        [ shoppingCartView model.cart
        , totalView model
        , purchaseView model
        , loaderView model
        ]


postCheckoutView : Model -> Html.Html Msg
postCheckoutView model =
    div [ class "elm-cart-postcheckout" ]
        [ h1 [] [ text "Enjoy your Purchase!" ]
        , img
            [ class "elm-cart-postcheckout-image"
            , src "img/delivery.gif"
            ]
            []
        , div
            [ href "#"
            , onClick GoHome
            , class "elm-cart-button"
            ]
            [ text "I want to buy more!" ]
        ]


getProductPictureURL : Product -> String
getProductPictureURL product =
    case product.pictureURL of
        Just url ->
            url

        Nothing ->
            defaultProductPictureURL


getCurrentCustomization : Model -> Customization
getCurrentCustomization model =
    case model.currentCustomization of
        Just customization ->
            customization

        Nothing ->
            Customization Nothing (Just 1)


productDescriptionView : Product -> Html.Html Msg
productDescriptionView product =
    div [ class "elm-cart-product-detail-description" ]
        [ text product.description ]


productDetailView : Model -> Html.Html Msg
productDetailView model =
    case model.currentProduct of
        Just product ->
            form [ onSubmit (AddToCart product (getCurrentCustomization model)) ]
                [ div [ class "elm-cart-product-detail" ]
                    [ div [ class "elm-cart-product-detail-name" ] [ text product.name ]
                    , img
                        [ class "elm-cart-product-detail-image"
                        , src (getProductPictureURL product)
                        ]
                        []
                    , div [ class "elm-cart-product-detail-price" ] [ text (formatCurrency product.price) ]
                    , productDescriptionView product
                    , customizationFormView product (getCurrentCustomization model)
                    , button [ class "elm-cart-product-detail-add elm-cart-button", type' "submit" ]
                        [ (text "Add To Cart") ]
                    ]
                ]

        Nothing ->
            div [] [ text "There's no product" ]


customizationFormView : Product -> Customization -> Html.Html Msg
customizationFormView product customization =
    let
        email =
            Maybe.withDefault "" customization.email

        quantityValue =
            case customization.quantity of
                Nothing ->
                    ""

                Just n ->
                    (toString n)

        parseQuantity quantity =
            case (String.toInt quantity) of
                Ok q ->
                    Just q

                Err err ->
                    Nothing
    in
        div [ class "elm-cart-product-customization" ]
            [ div [ class "form-group" ]
                [ label [] [ text "Email " ]
                , input
                    [ value email
                    , onInput
                        (\email ->
                            (Customize (Customization (Just email) customization.quantity))
                        )
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Quantity " ]
                , input
                    [ value quantityValue
                    , type' "number"
                    , pattern "\\d*"
                    , onInput
                        (\quantity ->
                            (Customize (Customization customization.email (parseQuantity quantity)))
                        )
                    ]
                    []
                ]
            ]


page : Model -> Html.Html Msg
page model =
    div [ class "elm-cart-page container-fluid" ]
        [ case model.page of
            ProductList ->
                productListView model

            ProductDetail ->
                productDetailView model

            Checkout ->
                checkoutView model

            CheckoutSuccess ->
                postCheckoutView model
        ]


headerView : Model -> Html.Html Msg
headerView model =
    header [ class "elm-cart-header row" ]
        [ a
            [ class "home"
            , (onClick GoHome)
            , (href "#")
            ]
            [ text "Home" ]
        , a
            [ class "elm-cart-header-checkout cart"
            , (onClick StartCheckout)
            , (href "#")
            ]
            [ text "" ]
        , span [ class "elm-cart-header-number" ]
            [ text (toString (List.length model.cart.items)) ]
        ]


footerView : Model -> Html.Html Msg
footerView model =
    div [ class "elm-footer" ]
        [ text "made with elm"
        , Html.br [] []
        , img [ src "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4KPCEtLSBHZW5lcmF0b3I6IEFkb2JlIElsbHVzdHJhdG9yIDE3LjEuMCwgU1ZHIEV4cG9ydCBQbHVnLUluIC4gU1ZHIFZlcnNpb246IDYuMDAgQnVpbGQgMCkgIC0tPgo8IURPQ1RZUEUgc3ZnIFBVQkxJQyAiLS8vVzNDLy9EVEQgU1ZHIDEuMS8vRU4iICJodHRwOi8vd3d3LnczLm9yZy9HcmFwaGljcy9TVkcvMS4xL0RURC9zdmcxMS5kdGQiPgo8c3ZnIHZlcnNpb249IjEuMSIgaWQ9IkxheWVyXzEiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHg9IjBweCIgeT0iMHB4IgogICB2aWV3Qm94PSIwIDAgMzIzLjE0MSAzMjIuOTUiIGVuYWJsZS1iYWNrZ3JvdW5kPSJuZXcgMCAwIDMyMy4xNDEgMzIyLjk1IiB4bWw6c3BhY2U9InByZXNlcnZlIj4KPGc+CiAgPHBvbHlnb24KICAgIGZpbGw9IiNGMEFEMDAiCiAgICBwb2ludHM9IjE2MS42NDksMTUyLjc4MiAyMzEuNTE0LDgyLjkxNiA5MS43ODMsODIuOTE2Ii8+CgogIDxwb2x5Z29uCiAgICBmaWxsPSIjN0ZEMTNCIgogICAgcG9pbnRzPSI4Ljg2NywwIDc5LjI0MSw3MC4zNzUgMjMyLjIxMyw3MC4zNzUgMTYxLjgzOCwwIi8+CgogIDxyZWN0CiAgICBmaWxsPSIjN0ZEMTNCIgogICAgeD0iMTkyLjk5IgogICAgeT0iMTA3LjM5MiIKICAgIHRyYW5zZm9ybT0ibWF0cml4KDAuNzA3MSAwLjcwNzEgLTAuNzA3MSAwLjcwNzEgMTg2LjQ3MjcgLTEyNy4yMzg2KSIKICAgIHdpZHRoPSIxMDcuNjc2IgogICAgaGVpZ2h0PSIxMDguMTY3Ii8+CgogIDxwb2x5Z29uCiAgICBmaWxsPSIjNjBCNUNDIgogICAgcG9pbnRzPSIzMjMuMjk4LDE0My43MjQgMzIzLjI5OCwwIDE3OS41NzMsMCIvPgoKICA8cG9seWdvbgogICAgZmlsbD0iIzVBNjM3OCIKICAgIHBvaW50cz0iMTUyLjc4MSwxNjEuNjQ5IDAsOC44NjggMCwzMTQuNDMyIi8+CgogIDxwb2x5Z29uCiAgICBmaWxsPSIjRjBBRDAwIgogICAgcG9pbnRzPSIyNTUuNTIyLDI0Ni42NTUgMzIzLjI5OCwzMTQuNDMyIDMyMy4yOTgsMTc4Ljg3OSIvPgoKICA8cG9seWdvbgogICAgZmlsbD0iIzYwQjVDQyIKICAgIHBvaW50cz0iMTYxLjY0OSwxNzAuNTE3IDguODY5LDMyMy4yOTggMzE0LjQzLDMyMy4yOTgiLz4KPC9nPgo8L3N2Zz4K" ] []
        ]


view : Model -> Html.Html Msg
view model =
    div
        [ id "elm-cart"
        ]
        [ headerView model
        , page model
        , footerView model
        ]
