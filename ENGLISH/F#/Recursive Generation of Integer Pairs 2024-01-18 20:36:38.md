```f#

let rec generatePairs (i:int) = 
    if i <= 0 then
        Seq.empty
    else
        Seq.unfold (fun (i,j) -> 
            if (i == 1) then
                None
            else
                Some ((i, j), (i-1, j+1))) (i, 0)

let pairs4 = generatePairs 4

// Display the pairs 
Seq.iter (fun (a:int, b:int) ->
    Printf.printf "%A %A\n" a b) pairs4

```

Explanation:
- The `generatePairs` function is a recursive function that generates a sequence of pairs of integers.
- The function takes a single argument, `i`, which specifies the number of pairs to generate.
- If `i` is less than or equal to 0, the function returns an empty sequence.
- Otherwise, the function uses a `Seq.unfold` expression to generate a sequence of pairs.
- The `Seq.unfold` expression takes a function as an argument.
- The function takes two arguments, `(i, j)`, where `i` is the current value of the first element of the pair and `j` is the current value of the second element of the pair.
- The function returns a tuple of two values:
  - The first value is the current pair, `(i, j)`.
  - The second value is the next pair to be generated, `(i-1, j+1)`.
- The function continues to generate pairs until `i` is equal to 1.
- The `pairs4` variable is assigned the value of the `generatePairs 4` expression, which generates a sequence of four pairs of integers.
- The `Seq.iter` function is used to display the pairs in the `pairs4` sequence.
- The `Printf.printf` function is used to format and display the pairs.