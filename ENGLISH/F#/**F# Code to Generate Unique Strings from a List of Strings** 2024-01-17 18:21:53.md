```f#
// This F# code defines a function called "GenerateUniqueStrings" that takes a list of strings as input and generates a new list of strings that contains all possible variations of the input strings with their characters in different orders.

let GenerateUniqueStrings (input : string list) : string list =
    // This function uses a recursive approach to generate all possible variations of the input strings.
    // It takes a list of strings as input and returns a list of lists of strings.
    // Each sublist in the returned list contains all possible variations of the corresponding string in the input list.
    let rec GenerateVariations (input : string list) : string list list =
        match input with
        | [] -> []  // If the input list is empty, return an empty list.
        | x :: xs ->  // If the input list is not empty, split the first string in the list into its characters.
                        // Recursively generate all possible variations of the remaining strings in the list.
                        // For each variation of the remaining strings, combine it with each character of the first string to create new variations.
                        // Return the list of all these new variations.
            let chars = x.ToCharArray()
            let variations = GenerateVariations xs
            let newVariations = List.map (fun var ->
                // For each variation of the remaining strings, combine it with each character of the first string.
                let newVariations = List.map (fun chr -> chr + var) chars
                // Return the list of all these new variations.
                newVariations) variations
            newVariations

    // Use the "GenerateVariations" function to generate all possible variations of the input strings.
    // Flatten the list of lists of strings into a single list of strings.
    List.concat (GenerateVariations input)

// Define a string list to test the "GenerateUniqueStrings" function.
let input = ["abc", "def", "ghi"]

// Call the "GenerateUniqueStrings" function with the input list and print the result.
printfn "%A" (GenerateUniqueStrings input)
```

Explanation:

1. `GenerateUniqueStrings` Function:
    - This function takes a list of strings as input and generates all possible variations of these strings with their characters in different orders.
    - It calls the recursive `GenerateVariations` function to generate all possible variations of the input strings.

2. `GenerateVariations` Function:
    - This recursive function takes a list of strings as input and returns a list of lists of strings.
    - Each sublist in the returned list contains all possible variations of the corresponding string in the input list.
    - It splits the first string in the input list into its characters.
    - Recursively calls itself to generate all possible variations of the remaining strings in the list.
    - For each variation of the remaining strings, it combines it with each character of the first string to create new variations.
    - Returns the list of all these new variations.

3. Using `GenerateVariations` to Generate Unique Strings:
    - The `GenerateUniqueStrings` function uses the `GenerateVariations` function to generate all possible variations of the input strings.
    - It then flattens the list of lists of strings into a single list of strings.
    - This flattened list contains all unique strings that can be formed by rearranging the characters of the input strings.

4. Testing the Code:
    - A sample input list is defined and passed to the `GenerateUniqueStrings` function.
    - The result is printed to the console using `printfn`.

This code demonstrates a more complex and differentiated F# code that recursively generates all possible variations of a list of strings, effectively creating a large and unique list of strings.