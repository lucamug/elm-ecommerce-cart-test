module Cart.Types exposing (..)

import HttpBuilder exposing (..)


type alias Product =
    { id : String
    , description : String
    , pictureURL : Maybe String
    , name : String
    , price : Float
    , isInStock : Bool
    }


type alias Customization =
    { email : Maybe String
    , quantity : Maybe Int
    }


type alias Item =
    { product : Product
    , quantity : Int
    , customization : Customization
    }


type alias Cart =
    { items : List Item }


type alias Purchase =
    { items : List Item
    }


type alias Model =
    { cart : Cart
    , availableProducts : List Product
    , page : Page
    , purchaseInProgress : Bool
    , currentProduct : Maybe Product
    , currentCustomization : Maybe Customization
    }


type Page
    = ProductList
    | ProductDetail
    | Checkout
    | CheckoutSuccess


type Msg
    = AddToCart Product Customization
    | UpdateItemQuantity Item Int
    | RemoveItem Item
    | StartCheckout
    | StartPurchase Cart
    | SelectProduct Product
    | GoHome
    | Customize Customization
    | PurchaseSucceed (HttpBuilder.Response String)
    | PurchaseFail (HttpBuilder.Error String)
