```f#

let primes =
    Seq.unfold (fun n ->
        let next = Seq.find (fun p -> n % p <> 0) (Seq.init n (fun n -> n+1))
        Some (n, next))
    2

let isPrime x =
    if x <= 1 then false
    else primes |> Seq.takeWhile (fun n -> n*n <= x) |> Seq.all (fun p -> x % p <> 0)

let factors x =
    Seq.init (x+1) (fun n -> (n, (if isPrime n then 1 else 0)))
    |> Seq.map (fun (n,e) -> Seq.init (e+1) (fun m -> n**m))
    |> Seq.concat

let gcd a b =
    let rec _gcd a b =
        if b = 0 then a
        else _gcd b (a % b)
    if a <= 0 || b <= 0 then 0
    else _gcd (abs a) (abs b)

let lcm a b =
    let g = gcd a b
    (abs a * abs b) / g

let phi n =
    Seq.init n (fun m -> (m,m-1))
    |> Seq.filter (fun (m,_) -> gcd n m = 1)
    |> Seq.map (fun (m,_) -> m)
    |> Seq.sum

let primeFactors n =
    Seq.unfold (fun n ->
        let p = Seq.find (fun p -> n % p = 0) primes
        Some (p, (n / p)))
    n

let primeFactorsWithPowers n =
    primeFactors n
    |> Seq.map (fun p -> (p, Seq.count (fun q -> q = p) (primeFactors n)))

let divisors n =
    primeFactorsWithPowers n
    |> Seq.map (fun (p,e) -> Seq.init (e+1) (fun m -> p**m))
    |> Seq.concat
    |> Seq.distinct

let isPerfect n =
    divisors n
    |> Seq.map (fun d -> if d = n then 0 else d)
    |> Seq.sum = n

let isAbundant n =
    divisors n
    |> Seq.map (fun d -> if d = n then 0 else d)
    |> Seq.sum > n

let isDeficient n =
    divisors n
    |> Seq.map (fun d -> if d = n then 0 else d)
    |> Seq.sum < n

let goldbachConjecture n =
    Seq.init (n+1) (fun m -> (m, Seq.exists (fun (p,_) -> isPrime p && isPrime (n-p)) (primeFactorsWithPowers m)))
    |> Seq.filter (fun (m,_) -> m >= 2)
    |> Seq.filter (fun (m,_) -> m < n)

let isPrimeFermat n =
    if isPrime n then true
    else let rec gcd a b = if b = 0 then a else gcd b (a % b) in
        Seq.init (gcd (n-1) (n+1)) (fun m -> 2**m % n)
        |> Seq.exists (fun x -> x = 1)

let isPrimeMillerRabin n i =
    if n <= 1 then false
    else if n <= 3 then true
    else let d = n-1 in
        let k = Seq.count (fun x -> x = 1) (Seq.unfold (fun y ->
            if y = 1 then None else Some (y * y % n, y*y % n)) 2 d) in
        if k = 0 then false
        else Seq.exists (fun j -> 2**j % n <> 1 && 2**j % n <> n-1) (Seq.init i (fun j -> j))

let isPrimeAKS n =
    if n <= 1 then false
    else let b = 2
        let q = (n-1) / 2
        let s = Seq.count (fun x -> x = 1) (Seq.filter (fun x -> x % 2 = 1) (Seq.init (n+1) (fun a -> (a**q + 1) % n)))
        let r = s = 0 in
        let p = Seq.filter (fun x -> x <> 1 && x <> n-1) (Seq.init (s+1) (fun t -> (b**t) % n))
        let v = Seq.exists (fun x -> x = 1) (Seq.map (fun c -> (gcd (c+1) n) % n) p) in
        let w = Seq.exists (fun x -> x = n-1) (Seq.map (fun c -> (gcd (c-1) n) % n) p) in
        r && (not v || w)

// Print the first 100 prime numbers
for n in primes |> Seq.take 100 do
    printfn "%d" n

// Print the factors of 12
for f in factors 12 do
    printfn "%d" f

// Print the greatest common divisor of 12 and 18
printfn "%d" (gcd 12 18)

// Print the least common multiple of 12 and 18
printfn "%d" (lcm 12 18)

// Print the Euler's totient function of 10
printfn "%d" (phi 10)

// Print the prime factors of 12
for f in primeFactors 12 do
    printfn "%d" f

// Print the prime factors with powers of 12
for (p,e) in primeFactorsWithPowers 12 do
    printfn "%d^%d" p e

// Print the divisors of 12
for d in divisors 12 do
    printfn "%d" d

// Print whether 6 is a perfect number
printfn "%b" (isPerfect 6)

// Print whether 12 is an abundant number
printfn "%b" (isAbundant 12)

// Print whether 8 is a deficient number
printfn "%b" (isDeficient 8)

// Print the Goldbach conjecture for numbers up to 100
for (n,r) in goldbachConjecture 100 do
    printfn "%d: %b" n r

// Print whether 11 is a prime number using Fermat's little theorem
printfn "%b" (isPrimeFermat 11)

// Print whether 11 is a prime number using Miller-Rabin primality test with 10 iterations
printfn "%b" (isPrimeMillerRabin 11 10)

// Print whether 11 is a prime number using AKS primality test
printfn "%b" (isPrimeAKS 11)

```

This code implements a variety of number theory functions in F#. It includes functions for finding prime numbers, calculating the greatest common divisor and least common multiple of two numbers, finding the Euler's totient function, finding the prime factors of a number, and testing whether a number is perfect, abundant, or deficient. It also includes the Goldbach conjecture, which states that every even number greater than 2 can be expressed as the sum of two primes, and three primality tests: Fermat's little theorem, Miller-Rabin primality test, and AKS primality test.