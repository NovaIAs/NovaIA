```f#

type Format =
    | JSON
    | CSV

let inline compose f g x = g (f x)
let inline identity x = x

type Result<'Ok, 'Err> =
    | Ok of 'Ok
    | Err of 'Err

type Parser<'Ok, 'Err, 'Format, Input> =
    (Input -> Result<'Ok, 'Err>)

type Writer<'Ok, 'Err, 'Format, Output> =
    ('Ok -> Output)

let inline partial f g x =
    match f x with
    | Ok x -> g x with
    | Err e -> Err e

let inline inline map f g x =
    f (g x)

let inline inline map2 f g h x y =
    f (g x) (h y)

let inline inline map3 f g h i j k x y z =
    f (g x) (h y) (i j) (k z)

let inline inline type1 f g x =
    f (g x)

type ('Ok -+ 'Err, 'Format) ValueConverter =
    ('Ok -> 'Err, 'Format, 'Ok) Writer

type ('Ok -+ 'Err, 'Format) ValueParser =
    ('Err -> 'Ok, 'Format, Input) Parser

let inline inline composeConverter f g x =
    f (g x)

type ('Ok -+ 'Err, 'Format) ValueCodec =
    ('Ok -+ 'Err, 'Format) ValueParser * ('Ok -+ 'Err, 'Format) ValueConverter

let inline inline composeCodec (p, c) (p2, c2) x =
    match (p x, p2 (c x)) with
    | (Ok ok, Ok ok2) -> Ok (c2 ok2)
    | (Ok ok, Err err) -> Err err
    | (Err err, Ok ok) -> Err err
    | (Err err, Err err2) -> Err (err ^ "\n" ^ err2)

type Endpoint<'Ok, 'Err, 'Format, Request, Response> =
    Request -> Result<'Ok, 'Err> -> Response

type ContractType =
    | InOnly
    | InOut

type Contract<'ContractType, 'Ok, 'Err, 'Format, Request, Response> =
    Request -> 'Err -> ('Ok, Endpoint<'Ok, 'Err, 'Format, Request, Response>)

let inline inline bindValueConverter (p, c) f =
    (p, composeConverter c f)

```