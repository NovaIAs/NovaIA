```elixir
defmodule Fibonacci do
  @doc """
  Calculates the nth fibonacci number using recursion.

  ## Examples

      iex> Fibonacci.calculate(0)
      0
      iex> Fibonacci.calculate(1)
      1
      iex> Fibonacci.calculate(10)
      55
  """
  def calculate(n) when n < 0 do
    raise ArgumentError, "n must be a non-negative integer"
  end

  def calculate(0), do: 0
  def calculate(1), do: 1
  def calculate(n) do
    calculate(n - 1) + calculate(n - 2)
  end
end

defmodule Prime do
  @doc """
  Checks if a given number is prime.

  ## Examples

      iex> Prime.is_prime?(2)
      true
      iex> Prime.is_prime?(10)
      false
  """
  def is_prime?(n) when n < 2 do
    false
  end

  def is_prime?(n) do
    is_prime?(n, 2, :math.sqrt(n))
  end

  defp is_prime?(n, divisor, limit) when divisor > limit do
    true
  end

  defp is_prime?(n, divisor, limit) when rem(n, divisor) == 0 do
    false
  end

  defp is_prime?(n, divisor, limit) do
    is_prime?(n, divisor + 1, limit)
  end
end

defmodule Sorting do
  @doc """
  Sorts a list of numbers using the bubble sort algorithm.

  ## Examples

      iex> Sorting.bubble_sort([5, 3, 1, 2, 4])
      [1, 2, 3, 4, 5]
  """
  def bubble_sort(list) do
    bubble_sort(list, [])
  end

  defp bubble_sort([], sorted) do
    sorted
  end

  defp bubble_sort([first | rest], sorted) do
    sorted_rest = bubble_sort(rest, sorted)
    insert_first(first, sorted_rest)
  end

  defp insert_first(element, [head | tail]) when element <= head do
    [element | head | tail]
  end

  defp insert_first(element, sorted) do
    [element | sorted]
  end
end

defmodule BinarySearch do
  @doc """
  Searches for a target value in a sorted list using the binary search algorithm.

  ## Examples

      iex> BinarySearch.search([1, 2, 3, 4, 5], 3)
      2
      iex> BinarySearch.search([1, 2, 3, 4, 5], 6)
      -1
  """
  def search(list, target) do
    search(list, target, 0, length(list) - 1)
  end

  defp search([], _target, _low, _high) do
    -1
  end

  defp search(_, _target, low, high) when low > high do
    -1
  end

  defp search(list, target, low, high) do
    mid = div(low + high, 2)
    case Enum.at(list, mid) do
      ^target -> mid
      value when value < target -> search(list, target, mid + 1, high)
      _ -> search(list, target, low, mid - 1)
    end
  end
end

defmodule QuickSort do
  @doc """
  Sorts a list of numbers using the quick sort algorithm.

  ## Examples

      iex> QuickSort.sort([5, 3, 1, 2, 4])
      [1, 2, 3, 4, 5]
  """
  def sort(list) do
    quick_sort(list, [])
  end

  defp quick_sort([], sorted) do
    sorted
  end

  defp quick_sort([pivot | rest], sorted) do
    {left, right} = partition(rest, pivot)
    quick_sort(left, sorted) ++ [pivot] ++ quick_sort(right, sorted)
  end

  defp partition([], pivot) do
    {{}, {}}
  end

  defp partition([head | tail], pivot) do
    {left, right} = partition(tail, pivot)
    case head do
      value when value < pivot -> {{head | left}, right}
      _ -> {left, {head | right}}
    end
  end
end

defmodule MergeSort do
  @doc """
  Sorts a list of numbers using the merge sort algorithm.

  ## Examples

      iex> MergeSort.sort([5, 3, 1, 2, 4])
      [1, 2, 3, 4, 5]
  """
  def sort(list) do
    merge_sort(list, [])
  end

  defp merge_sort([], sorted) do
    sorted
  end

  defp merge_sort([head | tail], sorted) do
    {left, right} = split(tail)
    merge(head, merge_sort(left, []), merge_sort(right, []), sorted)
  end

  defp split(list) do
    len = length(list)
    mid = div(len, 2)
    {Enum.take(list, mid), Enum.drop(list, mid)}
  end

  defp merge(element, left, right, sorted) do
    case {left, right} do
      {[], []} -> [element | sorted]
      {[], _} -> [element | merge([], right, sorted)]
      {_, []} -> [element | merge(left, [], sorted)]
      {head_left, head_right | tail_right} when head_left < head_right ->
        merge(head_right, tail_right, left, [head_left | sorted])
      {head_left | tail_left, head_right} ->
        merge(head_left, tail_left, right, [head_right | sorted])
    end
  end
end

defmodule HuffmanCoding do
  @doc """
  Encodes a string using Huffman coding.

  ## Examples

      iex> HuffmanCoding.encode("hello")
      "01101000011001010111001001100100011011110110111001100111"
  """
  def encode(string) do
    frequencies = frequencies(string)
    tree = build_tree(frequencies)
    encoded_string = encode_string(string, tree, [])
    encoded_string
  end

  defp frequencies(string) do
    string
    |> String.graphemes()
    |> Enum.reduce(%{}, fn char, acc ->
      Map.update(acc, char, 1, &(&1 + 1))
    end)
  end

  defp build_tree(frequencies) do
    frequencies
    |> Enum.map(fn {char, freq} -> {char, freq, nil, nil} end)
    |> Enum.sort_by(fn {_char, freq, _left, _right} -> freq end)
    |> build_tree_helper()
  end

  defp build_tree_helper([node]), do: node

  defp build_tree_helper([first, second | rest]) do
    new_node = {nil, first[1] + second[1], first, second}
    rest = [new_node | rest]
    rest |> Enum.sort_by(fn {_char, freq, _left, _right} -> freq end) |> build_tree_helper()
  end

  defp encode_string(string, tree, encoded) do
    string
    |> String.graphemes()
    |> Enum.reduce(encoded, fn char, acc ->
      encode_char(char, tree, acc)
    end)
  end

  defp encode_char(char, tree, encoded) do
    case tree do
      {char, _, _, _} -> encoded
      {_, _, left, right} ->
        if char in left do
          encode_char(char, left, encoded ++ "0")
        else
          encode_char(char, right, encoded ++ "1")
        end
    end
  end
end

defmodule RSA do
  @doc """
  Generates a public and private key pair using the RSA algorithm.

  ## Examples

      iex> {public_key, private_key} = RSA.generate_keys()
      {:public_key, :private_key}
  """
  def generate_keys() do
    p = generate_prime()
    q = generate_prime()
    n = p * q
    phi_n = (p - 1) * (q - 1)
    e = generate_coprime(phi_n)
    d = mod_inverse(e, phi_n)
    {e, n}, {d, n}
  end

  defp generate_prime() do
    candidate = :rand.uniform(1_000_000) |> Integer.to_string()
    if Prime.is_prime?(candidate) do
      candidate
    else
      generate_prime()
    end
  end

  defp generate_coprime(n) do
    candidate = :rand.uniform(n - 1) |> Integer.to_string()
    if gcd(candidate, n) == 1 do
      candidate
    else
      generate_coprime(n)
    end
  end

  defp gcd(a, b) do
    if b == 0 do
      a
    else
      gcd(b, rem(a, b))
    end
  end

  defp mod_inverse(a, m) do
    x0, x1, y0, y1 = 0, 1, 1, 0
    while m > 0 do
      q = div(a, m)
      a, m = m, rem(a, m)
      x0, x1 = x1, x0 - q * x1
      y0, y1 = y1, y0 - q * y1
    end
    x0
  end

  defp encrypt(message, e, n) do
    Integer.to_string(Integer.pow(message, e, n))
  end

  defp decrypt(message, d, n) do
    Integer.to_string(Integer.pow(message, d, n))
  end

  defp sign(message, d, n) do
    Integer.to_string(Integer.pow(message, d, n))
  end

  defp verify(signature, e, n) do
    Integer.to_string(Integer.pow(signature, e, n))
  end
end
```