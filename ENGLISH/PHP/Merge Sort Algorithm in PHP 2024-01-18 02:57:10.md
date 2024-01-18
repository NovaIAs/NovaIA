```php
<?php

namespace Acme\App\Algorithms\Sorting;

class MergeSort
{
    /**
     * Sort an array of integers using the merge sort algorithm.
     *
     * @param array $array The array to be sorted.
     * @return array The sorted array.
     *
     * Time Complexity: O(n log n)
     * Space Complexity: O(n)
     */
    public function sort(array $array): array
    {
        // If the array is empty or contains only one element, it is already sorted.
        if (count($array) <= 1) {
            return $array;
        }

        // Divide the array into two halves.
        $mid = count($array) / 2;
        $leftHalf = array_slice($array, 0, $mid);
        $rightHalf = array_slice($array, $mid);

        // Recursively sort the two halves.
        $leftHalf = $this->sort($leftHalf);
        $rightHalf = $this->sort($rightHalf);

        // Merge the sorted halves into a single sorted array.
        return $this->merge($leftHalf, $rightHalf);
    }

    /**
     * Merge two sorted arrays into a single sorted array.
     *
     * @param array $leftHalf The first sorted array.
     * @param array $rightHalf The second sorted array.
     * @return array The merged sorted array.
     *
     * Time Complexity: O(n)
     * Space Complexity: O(n)
     */
    private function merge(array $leftHalf, array $rightHalf): array
    {
        // Initialize the merged array.
        $merged = [];

        // Initialize the indices of the left and right halves.
        $leftIndex = 0;
        $rightIndex = 0;

        // While both indices are within the respective arrays.
        while ($leftIndex < count($leftHalf) && $rightIndex < count($rightHalf)) {
            // If the element in the left half is less than or equal to the element in the right half.
            if ($leftHalf[$leftIndex] <= $rightHalf[$rightIndex]) {
                // Add the element from the left half to the merged array.
                $merged[] = $leftHalf[$leftIndex];
                // Increment the index of the left half.
                $leftIndex++;
            } else {
                // Add the element from the right half to the merged array.
                $merged[] = $rightHalf[$rightIndex];
                // Increment the index of the right half.
                $rightIndex++;
            }
        }

        // Append the remaining elements from the left half.
        $merged = array_merge($merged, array_slice($leftHalf, $leftIndex));

        // Append the remaining elements from the right half.
        $merged = array_merge($merged, array_slice($rightHalf, $rightIndex));

        // Return the merged sorted array.
        return $merged;
    }
}
```

This code implements the merge sort algorithm in PHP. Merge sort is a divide-and-conquer sorting algorithm that works by recursively dividing the array into smaller and smaller subarrays until each subarray contains only one element. The subarrays are then merged together in sorted order, starting with the smallest subarrays and working up to the largest subarray.

The `sort()` method is the entry point to the algorithm. It takes an array of integers as input and returns a sorted array. The method first checks if the array is empty or contains only one element, in which case it is already sorted. If the array contains more than one element, the method divides the array into two halves, sorts each half recursively, and then merges the sorted halves together.

The `merge()` method is used to merge two sorted arrays into a single sorted array. The method takes two sorted arrays as input and returns a merged sorted array. The method initializes an empty merged array and then uses two indices to iterate through the two input arrays. The method compares the elements at the current indices and adds the smaller element to the merged array. The method then increments the index of the array that contains the smaller element. The process continues until one of the input arrays is empty. The method then appends the remaining elements from the other input array to the merged array.

The merge sort algorithm has a time complexity of O(n log n) and a space complexity of O(n). This means that the algorithm takes O(n log n) time to sort an array of size n, and it uses O(n) space to do so.