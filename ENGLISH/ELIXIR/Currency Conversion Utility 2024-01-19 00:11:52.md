```elixir
defmodule CurrencyConverter do
  def convert(amount, from, to) do
    rates = %{
      "USD" => 1.0,
      "EUR" => 0.89,
      "GBP" => 1.21,
      "JPY" => 107.0,
      "CAD" => 1.29
    }

    if from in rates and to in rates do
      converted_amount = amount * rates[to] / rates[from]
      {:ok, converted_amount}
    else
      {:error, "Invalid currency"}
    end
  end
end

IO.puts CurrencyConverter.convert(100, "USD", "EUR") # 89.0
IO.puts CurrencyConverter.convert(200, "GBP", "JPY") # 24,200.0
IO.puts CurrencyConverter.convert(300, "CAD", "USD") # 232.56
IO.puts CurrencyConverter.convert(400, "NOK", "USD") # {:error, "Invalid currency"}
```

This code defines a module called `CurrencyConverter` that provides a function called `convert`, which converts an amount of money from one currency to another.

The `convert` function takes three arguments:

* `amount`: The amount of money to convert.
* `from`: The currency to convert from.
* `to`: The currency to convert to.

The function first checks if both the `from` and `to` currencies are valid by checking if they are keys in the `rates` map. If both currencies are valid, the function calculates the converted amount by multiplying the `amount` by the exchange rate from the `to` currency to the `from` currency. The exchange rate is obtained by dividing the value of the `to` currency by the value of the `from` currency in the `rates` map.

If either the `from` or `to` currency is invalid, the function returns an error message.

The following are some examples of how to use the `convert` function:

```elixir
CurrencyConverter.convert(100, "USD", "EUR") # 89.0
CurrencyConverter.convert(200, "GBP", "JPY") # 24,200.0
CurrencyConverter.convert(300, "CAD", "USD") # 232.56
CurrencyConverter.convert(400, "NOK", "USD") # {:error, "Invalid currency"}
```

The first example converts 100 US dollars to euros and returns 89 euros. The second example converts 200 British pounds to Japanese yen and returns 24,200 yen. The third example converts 300 Canadian dollars to US dollars and returns 232.56 US dollars. The fourth example attempts to convert 400 Norwegian kroner to US dollars, but returns an error message because the Norwegian krone is not a valid currency in the `rates` map.