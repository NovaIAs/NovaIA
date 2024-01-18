```elixir
defmodule UUID do
  def generate do
    generate_uuid()
  end

  defp generate_uuid do
    uuid = :crypto.strong_rand_bytes(16)
    version_uuid(uuid)
  end

  defp version_uuid(uuid) do
    uuid_version_4 uuid, 4
  end

  defp uuid_version_4(uuid, 4) do
    uuid = uuid |> List.insert_at(6, 4)
    uuid_variant(uuid, 2)
  end

  defp uuid_variant(uuid, variant) when variant in [2, 4, 6, 8] do
    uuid = uuid |> List.insert_at(8, variant)
    format_uuid(uuid)
  end

  defp format_uuid(uuid) do
    uuid_string = Enum.join(uuid, "", fn byte -> format_byte(byte) end)
    insert_dashes(uuid_string)
  end

  defp format_byte(byte) do
    byte = byte |> Integer.to_string(16)
    byte = if byte |> String.length() == 1, do: "0#{byte}", else: byte
    byte
  end

  defp insert_dashes(uuid_string) do
    uuid_string |> String.slice(0..7) <> "-" <>
    uuid_string |> String.slice(8..11) <> "-" <>
    uuid_string |> String.slice(12..15) <> "-" <>
    uuid_string |> String.slice(16..19) <> "-" <>
    uuid_string |> String.slice(20..31)
  end
end
```

Explanation:

1. `defmodule UUID do`: This line defines a module named `UUID` that will hold the functions for generating UUIDs.
2. `def generate do`: This line defines a function named `generate` in the `UUID` module that will generate a UUID.
3. `defp generate_uuid do`: This line defines a private function named `generate_uuid` in the `UUID` module that will actually generate the UUID.
4. `generate_uuid = :crypto.strong_rand_bytes(16)`: This line generates a random sequence of 16 bytes using the `:crypto.strong_rand_bytes/1` function.
5. `version_uuid(uuid)`: This line calls the `version_uuid` function with the generated random bytes as an argument.
6. `defp version_uuid(uuid, 4)`: This line defines a private function named `version_uuid` that takes two arguments: `uuid` (the random bytes) and `4` (the version of the UUID).
7. `uuid = uuid |> List.insert_at(6, 4)`: This line inserts the value `4` (the version of the UUID) into the list of bytes at position 6 (the version field in the UUID format).
8. `uuid_variant(uuid, 2)`: This line calls the `uuid_variant` function with the modified `uuid` and the variant value `2`.
9. `defp uuid_variant(uuid, variant) when variant in [2, 4, 6, 8]`: This line defines a private function named `uuid_variant` that takes two arguments: `uuid` (the modified bytes) and `variant` (the variant value).
10. `uuid = uuid |> List.insert_at(8, variant)`: This line inserts the `variant` value into the list of bytes at position 8 (the variant field in the UUID format).
11. `format_uuid(uuid)`: This line calls the `format_uuid` function with the modified `uuid`.
12. `defp format_uuid(uuid)`: This line defines a private function named `format_uuid` that takes one argument: `uuid` (the modified bytes).
13. `insert_dashes(uuid_string)`: This line calls the `insert_dashes` function with the `uuid_string`.
14. `defp insert_dashes(uuid_string)`: This line defines a private function named `insert_dashes` that takes one argument: `uuid_string` (the UUID string without dashes).
15. `uuid_string |> String.slice(0..7) <> "-" <>uuid_string |> String.slice(8..11) <> "-" <>uuid_string |> String.slice(12..15) <> "-" <>uuid_string |> String.slice(16..19) <> "-" <>uuid_string |> String.slice(20..31)`: This line inserts dashes into the UUID string at specific positions to format it as a standard UUID.