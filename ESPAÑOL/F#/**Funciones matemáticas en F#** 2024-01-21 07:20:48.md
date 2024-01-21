```f#
// Importamos las librerías necesarias
open System
open System.IO
open System.Linq

// Creamos una función que reciba un número y devuelva su factorial
// Fórmula: n! = n * (n-1) * (n-2) * ... * 1
let factorial n =
  if n = 1 then
    1
  else
    n * factorial (n-1)

// Creamos una función que reciba una lista de números y devuelva su suma
let sum list =
  list |> Seq.sum

// Creamos una función que reciba una lista de números y devuelva su producto
let product list =
  list |> Seq.reduce *

// Creamos una función que reciba una lista de números y devuelva el mayor de ellos
let max list =
  list |> Seq.max

// Creamos una función que reciba una lista de números y devuelva el menor de ellos
let min list =
  list |> Seq.min

// Creamos una función que reciba una lista de números y devuelva la media de ellos
let average list =
  let sum = list |> Seq.sum
  let count = list |> Seq.length
  sum / count

// Creamos una función que reciba una lista de números y devuelva la desviación estándar de ellos
let standardDeviation list =
  let average = average list
  let variances = list |> Seq.map (fun x -> (x - average) ** 2) |> Seq.sum
  let variance = variances / (list |> Seq.length - 1)
  Math.Sqrt(variance)

// Creamos una función que reciba una lista de números y devuelva una lista con los números ordenados de menor a mayor
let sort list =
  list |> Seq.sort

// Creamos una función que reciba una lista de números y devuelva una lista con los números ordenados de mayor a menor
let sortDescending list =
  list |> Seq.sortDescending

// Creamos una función que reciba una lista de números y devuelva una lista con los números pares
let filterEven list =
  list |> Seq.filter (fun x -> x % 2 = 0)

// Creamos una función que reciba una lista de números y devuelva una lista con los números impares
let filterOdd list =
  list |> Seq.filter (fun x -> x % 2 <> 0)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que cumplan una condición dada
let filterWhere list predicate =
  list |> Seq.filter predicate

// Creamos una función que reciba una lista de números y devuelva una lista con los números que no cumplan una condición dada
let filterWhereNot list predicate =
  list |> Seq.filter (fun x -> not (predicate x))

// Creamos una función que reciba una lista de números y devuelva una lista con los números que sean mayores que un valor dado
let filterGreaterThan value list =
  list |> Seq.filter (fun x -> x > value)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que sean menores que un valor dado
let filterLessThan value list =
  list |> Seq.filter (fun x -> x < value)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que sean mayores o iguales que un valor dado
let filterGreaterThanOrEqualTo value list =
  list |> Seq.filter (fun x -> x >= value)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que sean menores o iguales que un valor dado
let filterLessThanOrEqualTo value list =
  list |> Seq.filter (fun x -> x <= value)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que sean diferentes de un valor dado
let filterNotEqualTo value list =
  list |> Seq.filter (fun x -> x <> value)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que sean iguales a un valor dado
let filterEqualTo value list =
  list |> Seq.filter (fun x -> x = value)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que cumplan una condición dada y otra lista con los números que no cumplan esa condición
let partition list predicate =
  list |> Seq.partition predicate

// Creamos una función que reciba una lista de números y devuelva una lista con los números que son múltiplos de un valor dado
let filterMultiplesOf value list =
  list |> Seq.filter (fun x -> x % value = 0)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que no son múltiplos de un valor dado
let filterNotMultiplesOf value list =
  list |> Seq.filter (fun x -> x % value <> 0)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que son divisores de un valor dado
let filterDivisorsOf value list =
  list |> Seq.filter (fun x -> value % x = 0)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que no son divisores de un valor dado
let filterNotDivisorsOf value list =
  list |> Seq.filter (fun x -> value % x <> 0)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que son primos
let filterPrime list =
  list |> Seq.filter (fun x ->
    if x <= 1 then
      false
    else
      let limit = Math.Sqrt(float x)
      for i = 2 to int limit do
        if x % i = 0 then
          false
      true)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que no son primos
let filterNotPrime list =
  list |> Seq.filter (fun x -> not (filterPrime [x]))

// Creamos una función que reciba una lista de números y devuelva una lista con los números que son perfectos
let filterPerfect list =
  list |> Seq.filter (fun x ->
    if x <= 1 then
      false
    else
      let divisors = list |> Seq.filter (fun y -> y > 0 && y < x && x % y = 0)
      let sum = divisors |> Seq.sum
      sum = x)

// Creamos una función que reciba una lista de números y devuelva una lista con los números que no son perfectos
let filterNotPerfect list =
  list |> Seq.filter (fun