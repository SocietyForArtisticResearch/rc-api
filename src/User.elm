module User exposing (..)

import Element.Region exposing (description)
import Json.Decode as JD exposing (Decoder, value)
import Serialize as S



-- JSON Parser for user object.


type UserID
    = UserID Int


getUserID : UserID -> Int
getUserID (UserID id) =
    id


type Username
    = Username String


getUsername : Username -> String
getUsername (Username name) =
    name


type UserDesc
    = UserDesc String


getUserDesc : UserDesc -> String
getUserDesc (UserDesc desc) =
    desc



-- what comes out of RC search


type alias ApiUser =
    { id : UserID
    , name : Username
    , description : UserDesc
    }


type alias ExtractedData =
    { country : Maybe String
    , year : Maybe Int
    , affiliation : Maybe String
    , interests : List String
    , tagline : Maybe String
    }


type Country
    = Country String


getCountry : Country -> String
getCountry (Country c) =
    c


type InterestKeyword
    = InterestKeyword String

getInterest : InterestKeyword -> String
getInterest (InterestKeyword kw) = kw


-- A codec for custom types with only one constructor that has only one field
newtypeCodec : (a -> b) -> (b -> a) -> S.Codec e b -> S.Codec e a
newtypeCodec from to fieldCodec =
    S.customType
        (\someCodec value ->
            someCodec (from value)
        )
        |> S.variant1 to fieldCodec
        |> S.finishCustomType

type alias User =
    { id : UserID
    , name : Username
    , description : UserDesc
    , interests : List InterestKeyword
    , country : Maybe Country
    , born : Maybe Int
    , tagline : Maybe String
    , affiliation : Maybe String
    }


userIDCodec : S.Codec e UserID
userIDCodec =
    newtypeCodec getUserID UserID S.int


usernameCodec : S.Codec e Username
usernameCodec =
    newtypeCodec getUsername Username S.string


userDescCodec : S.Codec e UserDesc
userDescCodec =
    newtypeCodec getUserDesc (UserDesc) S.string



userInterestCodec : S.Codec e InterestKeyword
userInterestCodec =
    newtypeCodec getInterest InterestKeyword S.string


countryCodec : S.Codec e Country
countryCodec =
    newtypeCodec getCountry Country S.string



userCodec : S.Codec e User
userCodec =
    S.record User
        |> S.field .id userIDCodec
        |> S.field .name usernameCodec
        |> S.field .description userDescCodec
        |> S.field .interests (S.list userInterestCodec)
        |> S.field .country (S.maybe countryCodec)
        |> S.field .born (S.maybe S.int)
        |> S.field .tagline (S.maybe S.string)
        |> S.field .affiliation (S.maybe S.string)
        |> S.finishRecord



decoder : Decoder (List ApiUser)
decoder =
    JD.list
        (JD.map3 ApiUser
            (JD.field "id" (JD.int |> JD.map UserID))
            (JD.field "name" (JD.string |> JD.map Username))
            (JD.field "description" (JD.string |> JD.map UserDesc))
        )


decodeString : String -> Result String (List ApiUser)
decodeString str =
    JD.decodeString decoder str
        |> Result.mapError JD.errorToString
