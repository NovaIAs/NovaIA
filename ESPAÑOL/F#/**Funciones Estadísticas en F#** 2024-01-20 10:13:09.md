```f#

// Definir una función que reciba un número y devuelva su factorial
let fact n =
    if n <= 1 then 1
    else n * fact (n-1)

// Definir una función recursiva que reciba una lista de números y devuelva su suma
let rec sum lst =
    match lst with
    | [] -> 0
    | h::t -> h + sum t

// Definir una función que reciba una lista de números y devuelva su máximo
let rec max lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> if t = [] then h else max (h::(max t))

// Definir una función que reciba una lista de números y devuelva su mínimo
let rec min lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> if t = [] then h else min (h::(min t))

// Definir una función que reciba una lista de números y devuelva su media
let rec mean lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> sum lst / (List.length lst)

// Definir una función que reciba una lista de números y devuelva su varianza
let rec variance lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> sum (List.map (fun x -> (x - mean lst) ** 2) lst) / (List.length lst)

// Definir una función que reciba una lista de números y devuelva su desviación estándar
let rec standard_deviation lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> sqrt (variance lst)

// Definir una función que reciba una lista de números y devuelva su mediana
let rec median lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> List.nth (List.sort lst) (List.length lst / 2)

// Definir una función que reciba una lista de números y devuelva su moda
let rec mode lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> let counts = List.groupBy id lst |> List.map (fun (x,y) -> (x, List.length y)) in
              List.find (fun (x,y) -> y = List.max (List.map snd counts)) counts |> fst

// Definir una función que reciba una lista de números y devuelva su rango
let rec range lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> max lst - min lst

// Definir una función que reciba una lista de números y devuelva su cuartiles
let rec quartiles lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> let sorted = List.sort lst in
              let q1 = List.nth sorted (List.length sorted / 4) in
              let q2 = List.nth sorted (List.length sorted / 2) in
              let q3 = List.nth sorted ((List.length sorted * 3) / 4) in
              (q1, q2, q3)

// Definir una función que reciba una lista de números y devuelva su deciles
let rec deciles lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> let sorted = List.sort lst in
              let d1 = List.nth sorted (List.length sorted / 10) in
              let d2 = List.nth sorted (List.length sorted * 2 / 10) in
              let d3 = List.nth sorted (List.length sorted * 3 / 10) in
              let d4 = List.nth sorted (List.length sorted * 4 / 10) in
              let d5 = List.nth sorted (List.length sorted * 5 / 10) in
              let d6 = List.nth sorted (List.length sorted * 6 / 10) in
              let d7 = List.nth sorted (List.length sorted * 7 / 10) in
              let d8 = List.nth sorted (List.length sorted * 8 / 10) in
              let d9 = List.nth sorted (List.length sorted * 9 / 10) in
              (d1, d2, d3, d4, d5, d6, d7, d8, d9)

// Definir una función que reciba una lista de números y devuelva su percentiles
let rec percentiles lst =
    match lst with
    | [] -> failwith "Lista vacía"
    | h::t -> let sorted = List.sort lst in
              let p10 = List.nth sorted (List.length sorted / 10) in
              let p25 = List.nth sorted (List.length sorted * 2 / 10) in
              let p50 = List.nth sorted (List.length sorted * 5 / 10) in
              let p75 = List.nth sorted (List.length sorted * 7 / 10) in
              let p90 = List.nth sorted (List.length sorted * 9 / 10) in
              (p10, p25, p50, p75, p90)

```

Este código define un conjunto de funciones estadísticas en F#. Las funciones incluyen:

* `fact`: Calcula el factorial de un número.
* `sum`: Calcula la suma de una lista de números.
* `max`: Calcula el máximo de una lista de números.
* `min`: Calcula el mínimo de una lista de números.
* `mean`: Calcula la media de una lista de números.
* `variance`: Calcula la varianza de una lista de números.
* `standard_deviation`: Calcula la desviación estándar de una lista de números.
* `median`: Calcula la mediana de una lista de números.
* `mode`: Calcula la moda de una lista de números.
* `range`: Calcula el rango de una lista de números.
* `quartiles`: Calcula los cuartiles de una lista de números.
* `deciles`: Calcula los deciles de una lista de números.
* `percentiles`: Calcula los percentiles de una lista de números.

Estas funciones se pueden utilizar para realizar análisis estadísticos de datos.