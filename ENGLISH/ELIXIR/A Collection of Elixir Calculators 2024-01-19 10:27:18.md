```elixir
defmodule Calculator do
  def add(a, b) do
    a + b
  end

  def subtract(a, b) do
    a - b
  end

  def multiply(a, b) do
    a * b
  end

  def divide(a, b) do
    a / b
  end
end

defmodule MortgageCalculator do
  def calculate_monthly_payment(principal, interest_rate, term_in_years) do
    monthly_interest_rate = interest_rate / 12
    number_of_payments = term_in_years * 12
    monthly_payment = principal * (monthly_interest_rate * (1 + monthly_interest_rate) ** number_of_payments) / ((1 + monthly_interest_rate) ** number_of_payments - 1)
    monthly_payment
  end
end

defmodule TipCalculator do
  def calculate_tip(bill_amount, tip_percentage) do
    tip_amount = bill_amount * (tip_percentage / 100)
    total_bill = bill_amount + tip_amount
    {tip_amount, total_bill}
  end
end

defmodule StringCalculator do
  def calculate(string) do
    numbers = string |> String.split(",") |> Enum.map(&String.to_integer/1)
    sum = Enum.sum(numbers)
    sum
  end
end

defmodule StatisticsCalculator do
  def calculate_mean(numbers) do
    sum = Enum.sum(numbers)
    mean = sum / Enum.count(numbers)
    mean
  end

  def calculate_median(numbers) do
    sorted_numbers = Enum.sort(numbers)
    length = Enum.count(sorted_numbers)
    if rem(length, 2) == 0 do
      median = (sorted_numbers[length / 2] + sorted_numbers[length / 2 + 1]) / 2
      median
    else
      median = sorted_numbers[div(length, 2) + 1]
      median
    end
  end

  def calculate_mode(numbers) do
    frequency_map = Enum.reduce(numbers, %{}, fn number, acc ->
      acc |> Map.update(number, 1, &(&1 + 1))
    end)
    maximum_frequency = Enum.max(frequency_map)
    modes = Enum.filter(frequency_map, fn {_number, frequency} -> frequency == maximum_frequency end)
    Enum.map(modes, fn {number, _frequency} -> number end)
  end
end

defmodule CurrencyConverter do
  def convert(amount, from_currency, to_currency) do
    rates = %{
      "USD" => 1.0,
      "GBP" => 0.75,
      "EUR" => 0.90,
      "JPY" => 110.0
    }
    converted_amount = amount * rates[to_currency] / rates[from_currency]
    converted_amount
  end
end

defmodule UnitConverter do
  def convert(value, from_unit, to_unit) do
    conversions = %{
      "cm" => 1.0,
      "m" => 100.0,
      "km" => 100000.0,
      "in" => 2.54,
      "ft" => 30.48,
      "yd" => 91.44
    }
    converted_value = value * conversions[to_unit] / conversions[from_unit]
    converted_value
  end
end

defmodule TemperatureConverter do
  def convert(value, from_unit, to_unit) do
    conversions = %{
      "C" => 1.0,
      "F" => (9 / 5) + 32,
      "K" => 273.15
    }
    converted_value = value * conversions[to_unit] / conversions[from_unit]
    converted_value
  end
end

defmodule DateCalculator do
  def add_days(date, number_of_days) do
    Date.add(date, number_of_days)
  end

  def subtract_days(date, number_of_days) do
    Date.sub(date, number_of_days)
  end

  def calculate_age(birth_date, today) do
    today |> Date.diff(birth_date, :days) |> div(365)
  end
end

defmodule TimeCalculator do
  def add_hours(time, number_of_hours) do
    Time.add(time, number_of_hours * 3600)
  end

  def subtract_hours(time, number_of_hours) do
    Time.sub(time, number_of_hours * 3600)
  end

  def calculate_duration(start_time, end_time) do
    Time.diff(end_time, start_time, :microseconds) |> div(1000000)
  end
end
```

This is a collection of different calculators written in Elixir. Each calculator has its own module and provides a specific functionality.

- **Calculator:** Provides basic arithmetic operations like addition, subtraction, multiplication, and division.

- **MortgageCalculator:** Calculates the monthly payment for a mortgage loan given the principal, interest rate, and term in years.

- **TipCalculator:** Calculates the tip amount and total bill amount given the bill amount and tip percentage.

- **StringCalculator:** Calculates the sum of a comma-separated string of numbers.

- **StatisticsCalculator:** Calculates the mean, median, and mode of a list of numbers.

- **CurrencyConverter:** Converts an amount of money from one currency to another using a fixed exchange rate.

- **UnitConverter:** Converts a value from one unit of measurement to another using a fixed conversion factor.

- **TemperatureConverter:** Converts a temperature from one unit of measurement to another using a fixed conversion factor.

- **DateCalculator:** Adds or subtracts a number of days to a given date, and calculates the age of a person given their birth date and the current date.

- **TimeCalculator:** Adds or subtracts a number of hours to a given time, and calculates the duration between two times.