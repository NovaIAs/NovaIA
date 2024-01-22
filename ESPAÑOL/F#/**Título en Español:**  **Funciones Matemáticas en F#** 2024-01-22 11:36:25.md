```f#

// Definimos una función que recibe un entero y devuelve su cuadrado
let square x = x * x

// Definimos una función que recibe una lista de enteros y devuelve la suma de sus cuadrados
let sumOfSquares xs = Seq.sumBy (fun x -> square x) xs

// Definimos una función que recibe una lista de enteros y devuelve la lista de sus cuadrados
let squares xs = Seq.map (fun x -> square x) xs

// Definimos una función que recibe dos números y devuelve su máximo común divisor
let gcd a b =
  if (b = 0) then (a)
  else (gcd b (a % b))

// Definimos una función que recibe una lista de enteros y devuelve su máximo común divisor
let gcdOfList xs = xs |> Seq.reduce gcd

// Definimos una función que recibe una lista de enteros y devuelve su mínimo común múltiplo
let lcm a b =
  if (a = 0) || (b = 0) then (0)
  else ((a * b) / gcd a b)

// Definimos una función que recibe una lista de enteros y devuelve su mínimo común múltiplo
let lcmOfList xs = xs |> Seq.reduce lcm

// Definimos una función que recibe un número y devuelve su factorial
let factorial n =
  let rec factorial' n acc =
    if n > 0 then factorial' (n - 1) (n * acc)
    else acc
  factorial' n 1

// Definimos una función que recibe una lista de números y devuelve su factorial
let factorialOfList xs = xs |> Seq.map factorial

```

**Explicación del código:**

* La primera función, `square`, simplemente devuelve el cuadrado de un número entero.
* La segunda función, `sumOfSquares`, recibe una lista de enteros y devuelve la suma de sus cuadrados.
* La tercera función, `squares`, recibe una lista de enteros y devuelve la lista de sus cuadrados.
* La cuarta función, `gcd`, recibe dos números y devuelve su máximo común divisor.
* La quinta función, `gcdOfList`, recibe una lista de enteros y devuelve su máximo común divisor.
* La sexta función, `lcm`, recibe dos números y devuelve su mínimo común múltiplo.
* La séptima función, `lcmOfList`, recibe una lista de enteros y devuelve su mínimo común múltiplo.
* La octava función, `factorial`, recibe un número y devuelve su factorial.
* La novena función, `factorialOfList`, recibe una lista de números y devuelve su factorial.