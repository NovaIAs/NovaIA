```cool
module MergeSort {
    class Sort {
        static method main() {
            var a: [8]: Int := [3, 1, 4, 1, 5, 9, 2, 6];
            var b: [8]: Int := MergeSort.mergeSort(a, 0, 7);
            for (var i: Int := 0 to 7) {
                System.out.write(b[i].toString() + " ");
            }
            System.out.write("\n");
        }

        static method mergeSort(a: [8]: Int, left: Int, right: Int): [8]: Int {
            if (left < right) {
                var mid: Int := (left + right) div 2;
                var leftArray: [4]: Int := MergeSort.mergeSort(a, left, mid);
                var rightArray: [4]: Int := MergeSort.mergeSort(a, mid + 1, right);
                return MergeSort.merge(leftArray, rightArray);
            } else {
                return [1]: Int := a[left];
            }
        }

        static method merge(leftArray: [4]: Int, rightArray: [4]: Int): [8]: Int {
            var mergedArray: [8]: Int := [8]: Int;
            var leftIndex: Int := 0;
            var rightIndex: Int := 0;
            var mergedIndex: Int := 0;

            while (leftIndex < leftArray.length and rightIndex < rightArray.length) {
                if (leftArray[leftIndex] < rightArray[rightIndex]) {
                    mergedArray[mergedIndex] := leftArray[leftIndex];
                    leftIndex := leftIndex + 1;
                } else {
                    mergedArray[mergedIndex] := rightArray[rightIndex];
                    rightIndex := rightIndex + 1;
                }
                mergedIndex := mergedIndex + 1;
            }

            while (leftIndex < leftArray.length) {
                mergedArray[mergedIndex] := leftArray[leftIndex];
                leftIndex := leftIndex + 1;
                mergedIndex := mergedIndex + 1;
            }

            while (rightIndex < rightArray.length) {
                mergedArray[mergedIndex] := rightArray[rightIndex];
                rightIndex := rightIndex + 1;
                mergedIndex := mergedIndex + 1;
            }

            return mergedArray;
        }
    }
}
```

Explanation:

* The `Sort` class contains the main program and the `mergeSort` and `merge` methods.
* The `main` method creates an array of integers, calls the `mergeSort` method to sort the array, and then prints the sorted array to the console.
* The `mergeSort` method recursively divides the array into smaller subarrays, sorts the subarrays, and then merges the sorted subarrays back together.
* The `merge` method takes two sorted arrays as input and merges them into a single sorted array.

This code demonstrates the use of recursion and divide-and-conquer algorithms to sort an array of integers. The `mergeSort` method divides the array into smaller subarrays until each subarray contains only one element. The subarrays are then sorted and merged back together to form the sorted array. The `merge` method takes two sorted arrays as input and merges them into a single sorted array.

The time complexity of the `mergeSort` algorithm is O(n log n), where n is the number of elements in the array. The space complexity is also O(n log n).