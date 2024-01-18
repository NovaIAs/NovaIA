```elixir
defmodule ComplexElixirCode do
  defmodule RecursiveFibonacci do
    def fib(n) when n < 2, do: n
    def fib(n), do: fib(n - 1) + fib(n - 2)
  end

  defmodule TailRecursiveFibonacci do
    def fib(n, acc \\ 0, next \\ 1) do
      if n == 0, do: acc, else: fib(n - 1, next, acc + next)
    end
  end

  defmodule StreamOfPrimes do
    def primes(from \\ 2) do
      Enum.filter(from..Float.infinity, &is_prime?/1) |> Stream.drop(1)
    end

    defp is_prime?(n) do
      Enum.all?(2..:math.floor(:math.sqrt(n)), &(rem(n, &1) != 0))
    end
  end

  defmodule SieveOfEratosthenes do
    def primes(limit \\ 100) do
      sieve(2..limit)
    end

    defp sieve(primes) do
      Enum.reduce(primes, {[], primes}, fn prime, {acc, remaining} ->
        {Enum.concat(acc, [prime]), Enum.drop_while(remaining, &(rem(&1, prime) != 0))}
      end) |> elem(0)
    end
  end

  defmodule QuickSort do
    def sort(list) do
      cond do
        length(list) < 2 -> list
        true ->
          pivot = Enum.at(list, div(length(list), 2))
          {left, right} = Enum.split_with(list, &(&1 < pivot))
          Enum.concat(sort(left), [pivot | sort(right)])
      end
    end
  end

  defmodule MergeSort do
    def sort(list) do
      cond do
        length(list) < 2 -> list
        true ->
          mid = div(length(list), 2)
          {left, right} = Enum.split(list, mid)
          Enum.concat(sort(left), sort(right)) |> merge()
      end
    end

    defp merge(left, right \\ []) do
      cond do
        left == [] -> right
        right == [] -> left
        true ->
          hd_left = Enum.at(left, 0)
          hd_right = Enum.at(right, 0)

          if hd_left < hd_right, do: [hd_left | merge(Enum.drop(left, 1), right)],
          else: [hd_right | merge(left, Enum.drop(right, 1))]
      end
    end
  end

  defmodule RadixSort do
    def sort(list) do
      max = Enum.max(list)
      exp = 1

      while max / exp > 0 do
        counting_sort(list, exp)
        exp = exp * 10
      end

      list
    end

    defp counting_sort(list, exp) do
      count = Enum.map(0..9, &0)
      output = Enum.map(list, &0)

      Enum.each(list, fn num ->
        index = Integer.mod_rem(num, exp)
        count = count |> List.update_at(index, &(&1 + 1))
      end)

      1..9 |> Enum.reduce(count, [], fn num, acc -> [acc |> List.update_at(num, &(&1 + count |> List.at(num - 1))) | acc] end)
      |> Enum.with_index |> Enum.each(fn {cnt, i} ->
        idx = count |> List.at(i - 1)..(cnt - 1)
        Enum.each(idx, fn idx ->
          output = output |> List.update_at(idx, &num_at(list, output, idx, exp))
        end)
      end)

      output
    end

    defp num_at(list, output, idx, exp) do
      Enum.at(list, idx / exp)
    end
  end

  defmodule HeapSort do
    def sort(list) do
      heapify(list)
      heap_size = length(list)

      while heap_size > 1 do
        heap_size = heap_size - 1
        swap(list, 1, heap_size)
        max_heapify(list, 1, heap_size)
      end

      list
    end

    defp heapify(list) do
      start = div(length(list), 2)

      while start > 0 do
        max_heapify(list, start, length(list))
        start = start - 1
      end
    end

    defp max_heapify(list, i, heap_size) do
      left = 2 * i
      right = 2 * i + 1

      largest =
        if left <= heap_size and list |> Enum.at(left - 1) > list |> Enum.at(i - 1),
          do: left,
          else: i

      if right <= heap_size and list |> Enum.at(right - 1) > list |> Enum.at(largest - 1),
        do: right,
        else: largest

      if largest != i do
        swap(list, i, largest)
        max_heapify(list, largest, heap_size)
      end
    end

    defp swap(list, i, j) do
      tmp = list |> Enum.at(i - 1)
      list = list |> List.update_at(i - 1, &list |> Enum.at(j - 1)) |> List.update_at(j - 1, &tmp)
    end
  end
end
```

Explanation:

1. Recursive Fibonacci: This module provides two methods for calculating Fibonacci numbers: `fib` (recursive) and `tail_recursive_fib` (tail recursive) using pattern matching and recursion.

2. Stream of Primes: The `primes` function generates an infinite stream of prime numbers starting from a given number using the `Enum.filter` and `Stream.drop` functions. It utilizes the `is_prime?` helper function to check for prime numbers efficiently.

3. Sieve of Eratosthenes: The `primes` function implements the Sieve of Eratosthenes algorithm to find prime numbers up to a given limit. It uses `Enum.split_with` and `Enum.reduce` to efficiently generate the list of prime numbers.

4. Quick Sort: The `sort` function implements the Quick Sort algorithm to sort a list of numbers. It uses the `Enum.at` function to select the pivot and `Enum.split_with` to divide the list into two parts based on the pivot.

5. Merge Sort: The `sort` function implements the Merge Sort algorithm to sort a list of numbers. It uses `Enum.split` to divide the list into two halves, recursively sorts each half, and then merges them back together using the `merge` helper function.

6. Radix Sort: The `sort` function implements the Radix Sort algorithm to sort a list of numbers. It repeatedly performs counting sort on the digits of the numbers to sort them in ascending order.

7. Heap Sort: The `sort` function implements the Heap Sort algorithm to sort a list of numbers. It utilizes a max-heap data structure to efficiently rearrange the elements of the list into sorted order.

8. Explanation of Helper Functions:

   - `counting_sort`: Performs counting sort for a specific digit position.
   - `num_at`: Retrieves the value at a specific index in a list.
   - `max_heapify`: Maintains the max-heap property of a binary heap tree.
   - `swap`: Swaps two elements in a list.