```f#
let rec binarySearch arr val l r =
    if l > r then -1
    else
        let mid = (l + r) / 2
        match arr.[mid] with
        | x when x = val -> mid
        | _ when arr.[mid] < val -> binarySearch arr val (mid + 1) r
        | _ -> binarySearch arr val l (mid - 1)

let arr = [1; 3; 5; 7; 9; 11; 13; 15]
binarySearch arr 5 0 (Array.length arr - 1)
```

Explanation:

1. binarySearch Function:
   - This is a recursive binary search function that takes an array arr, a value to search for val, and the left (l) and right (r) indices to search within as input.

2. Base Case:
   - If l becomes greater than r, it means the value is not in the array, so the function returns -1.

3. Recursive Case:
   - Calculate the middle index mid as the average of l and r.
   - Compare the value at the middle index arr.[mid] with the value to search for val:
     - If they're equal, return the middle index mid as the position of the value in the array.
     - If arr.[mid] is less than val, the value must be in the right half of the array, so call the function recursively with (mid + 1) as the new left index.
     - If arr.[mid] is greater than val, the value must be in the left half of the array, so call the function recursively with l as the new right index.

4. Example Usage:
   - In the example, the array arr is given, and the value 5 is searched for. The binary search algorithm is called with l = 0 and r = 7 (the index range of the array), which will return the index of 5 in the array.

This code demonstrates a recursive binary search algorithm in F#, which efficiently finds a given value in a sorted array. It's efficient because it reduces the search space by half with each recursive call, achieving logarithmic search time complexity.