```elixir
defmodule ComplexCode do
  def generate_random_string(length) do
    :crypto.strong_rand_bytes(length) |> Base.url_encode64()
  end

  def parse_json(json_string) do
    Jason.decode(json_string)
  end

  def send_email(recipient, subject, body) do
    {:ok, _} = Swoosh.send(
      from: "example@gmail.com",
      to: recipient,
      subject: subject,
      html: body
    )
  end

  def fetch_data_from_api(url) do
    {:ok, response} = HTTPoison.get(url)
    response.body
  end

  def store_data_in_database(data) do
    Ecto.Repo.insert(%MySchema{data: data})
  end

  def perform_complex_calculation(n) do
    result = n * n * n
    result / 2
  end

  def main() do
    random_string = generate_random_string(10)

    json_data = fetch_data_from_api("https://example.com/api/v1/data")
    parsed_data = parse_json(json_data)

    send_email("john.doe@example.com", "Important Update", "Your data has been processed.")

    store_data_in_database(parsed_data)

    result = perform_complex_calculation(100)
    IO.puts "Result: #{result}"
  end
end

ComplexCode.main()
```

Explanation:

1. `generate_random_string/1`: Generates a random string of a specified length.

2. `parse_json/1`: Parses a JSON string into an Elixir map.

3. `send_email/3`: Sends an email using the Swoosh library.

4. `fetch_data_from_api/1`: Fetches data from a given URL using the HTTPoison library.

5. `store_data_in_database/1`: Stores data in a database using the Ecto framework.

6. `perform_complex_calculation/1`: Performs a complex calculation on a given number.

7. `main/0`: Defines the entry point of the program. Calls the above functions in sequence.

8. `ComplexCode.main()`: Invokes the `main` function to execute the program.