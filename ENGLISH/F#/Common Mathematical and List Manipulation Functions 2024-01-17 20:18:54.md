```f#

let rec print_list = function
    | [] -> ()
    | [x] -> printfn "%d\n" x
    | x::xs -> printfn "%d " x; print_list xs

let rec split_list [] n _ = [], []
| split_list l n lst =
    let n1, l1, l2 = split_list l (n-1) lst
    l1::n1, l2

let int_to_digits n =
    let ret = []
    for i = 1 to 10 do
        ret <- ret @ [n mod 10]
        n <- n / 10
    done
    ret

let rec sum_list lst =
    match lst with
    | [] -> 0
    | x::xs -> x + sum_list xs

let rec sum_of_multiples_of n = function
    | [] -> 0
    | x::lst -> if x mod n = 0 then x + sum_of_multiples_of n lst else sum_of_multiples_of n lst

let rec is_palidrome = function
    | [] -> true
    | [x] -> true
    | x::xs -> if x = List.last xs then is_palidrome (List.tl (Lst.tl xs)) else false

let rec count_palidromes_of_multiples_of n = function
    | [] -> 0
    | x::lst -> if is_palidrome (int_to_digits x) then 1 + count_palidromes_of_multiples_of n lst else count_palidromes_of_multiples_of n lst

// A function to print the sum of the digits of a number
let rec sum_of_digits = function
    | 0 -> 0
    | n -> n mod 10 + sum_of_digits (n/10)

// A function to print the reverse of a number
let rec reverse_list = function
    | [] -> []
    | [x] -> [x]
    | x::xs -> reverse_list xs @ [x]

// A function to print the factorial of a number
let rec factorial = function
    | 0 -> 1
    | n -> n * factorial (n-1)

// A function to print the GCD of two numbers
let rec gcd = function
    | 0, _ -> 0
    | a, b -> gcd (b mod a, a)

// A function to print the LCM of two numbers
let lcm m n = m * (n / gcd m n)
```

This code contains the following functions:

* `print_list`: This function prints a list of numbers.
* `split_list`: This function splits a list into two lists, each of size `n`.
* `int_to_digits`: This function converts an integer to a list of its digits.
* `sum_list`: This function sums the elements of a list.
* `sum_of_multiples_of`: This function sums the elements of a list that are multiples of a given number.
* `is_palindrome`: This function checks if a list is a palindrome.
* `count_palidromes_of_multiples_of`: This function counts the number of palindromes in a list of multiples of a given number.
* `sum_of_digits`: This function returns the sum of the digits of a number.
* `reverse_list`: This function returns the reverse of a list.
* `factorial`: This function returns the factorial of a number.
* `gcd`: This function returns the greatest common divisor of two numbers.
* `lcm`: This function returns the least common multiple of two numbers.