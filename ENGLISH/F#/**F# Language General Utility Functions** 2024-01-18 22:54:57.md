```f#
// Define a function that takes a list of integers and returns the sum of the list.
let sum (xs: int list) =
    xs |> Seq.fold (fun acc x -> acc + x) 0

// Define a function that takes a list of integers and returns the average of the list.
let average (xs: int list) =
    match xs with
    | [] -> failwith "Cannot compute the average of an empty list."
    | _ -> (sum xs) / (List.length xs)

// Define a function that takes a list of strings and returns the longest string in the list.
let longest (xs: string list) =
    match xs with
    | [] -> failwith "Cannot find the longest string in an empty list."
    | _ -> xs |> List.maxBy String.Length

// Define a function that takes a list of strings and returns a map from each string to its length.
let stringLengths (xs: string list) =
    xs |> List.map (fun s -> (s, String.Length s)) |> Map.ofList

// Define a function that takes a string and returns a list of all the words in the string.
let tokenize (s: string) =
    s |> String.Split [' '; '\n'; '\t'] |> List.filter (fun s -> s <> "")

// Define a function that takes a list of strings and returns a map from each word to the number of times it occurs in the list.
let wordFrequencies (xs: string list) =
    xs |> List.concat |> tokenize |> Seq.groupBy id |> Map.ofSeq (fun (k, v) -> (k, v |> Seq.length))

// Define a function that takes a list of integers and returns a list of the integers in sorted order.
let sort (xs: int list) =
    xs |> List.sort

// Define a function that takes a list of strings and returns a list of the strings in reverse order.
let reverse (xs: string list) =
    xs |> List.rev

// Define a function that takes a list of strings and returns a list of the strings with the first character capitalized.
let capitalize (xs: string list) =
    xs |> List.map (fun s -> s.[0].ToUpper() + s.Substring(1))

// Define a function that takes a list of strings and returns a string that is the concatenation of all the strings in the list, separated by commas.
let concatenate (xs: string list) =
    xs |> String.concat ", "

// Define a function that takes a list of integers and returns a string that is the concatenation of all the integers in the list, separated by commas.
let concatenateInts (xs: int list) =
    xs |> List.map string |> String.concat ", "

// Define a function that takes a list of strings and returns a string that is the concatenation of all the strings in the list, separated by newlines.
let concatenateLines (xs: string list) =
    xs |> String.concat "\n"

// Define a function that takes a list of integers and returns a string that is the concatenation of all the integers in the list, separated by newlines.
let concatenateLinesInts (xs: int list) =
    xs |> List.map string |> String.concat "\n"

// Define a function that takes a list of strings and returns a string that is the concatenation of all the strings in the list, separated by spaces.
let concatenateSpaces (xs: string list) =
    xs |> String.concat " "

// Define a function that takes a list of integers and returns a string that is the concatenation of all the integers in the list, separated by spaces.
let concatenateSpacesInts (xs: int list) =
    xs |> List.map string |> String.concat " "

// Define a function that takes a list of strings and returns a string that is the concatenation of all the strings in the list, separated by tabs.
let concatenateTabs (xs: string list) =
    xs |> String.concat "\t"

// Define a function that takes a list of integers and returns a string that is the concatenation of all the integers in the list, separated by tabs.
let concatenateTabsInts (xs: int list) =
    xs |> List.map string |> String.concat "\t"

// Define a function that takes a list of strings and returns a string that is the concatenation of all the strings in the list, separated by commas and enclosed in double quotes.
let concatenateDoubleQuotes (xs: string list) =
    xs |> String.concat ", " |> (fun s -> "\"" + s + "\"")

// Define a function that takes a list of integers and returns a string that is the concatenation of all the integers in the list, separated by commas and enclosed in double quotes.
let concatenateDoubleQuotesInts (xs: int list) =
    xs |> List.map string |> String.concat ", " |> (fun s -> "\"" + s + "\"")

// Define a function that takes a list of strings and returns a string that is the concatenation of all the strings in the list, separated by newlines and enclosed in double quotes.
let concatenateLinesDoubleQuotes (xs: string list) =
    xs |> String.concat "\n" |> (fun s -> "\"" + s + "\"")

// Define a function that takes a list of integers and returns a string that is the concatenation of all the integers in the list, separated by newlines and enclosed in double quotes.
let concatenateLinesDoubleQuotesInts (xs: int list) =
    xs |> List.map string |> String.concat "\n" |> (fun s -> "\"" + s + "\"")

// Define a function that takes a list of strings and returns a string that is the concatenation of all the strings in the list, separated by spaces and enclosed in double quotes.
let concatenateSpacesDoubleQuotes (xs: string list) =
    xs |> String.concat " " |> (fun s -> "\"" + s + "\"")

// Define a function that takes a list of integers and returns a string that is the concatenation of all the integers in the list, separated by spaces and enclosed in double quotes.
let concatenateSpacesDoubleQuotesInts (xs: int list) =
    xs |> List.map string |> String.concat " " |> (fun s -> "\"" + s + "\"")

// Define a function that takes a list of strings and returns a string that is the concatenation of