```elixir
# Module to define a behavior for a simple calculator.
defmodule CalculatorBehavior do
  @moduledoc """
  This module defines a behavior for a simple calculator.

  A calculator must implement the following functions:

  * `add/2`: Adds two numbers together.
  * `subtract/2`: Subtracts one number from another.
  * `multiply/2`: Multiplies two numbers together.
  * `divide/2`: Divides one number by another.
  """

  @callback add(a :: number, b :: number) :: number
  @callback subtract(a :: number, b :: number) :: number
  @callback multiply(a :: number, b :: number) :: number
  @callback divide(a :: number, b :: number) :: number
end

# Module to define a simple calculator that implements the CalculatorBehavior behavior.
defmodule SimpleCalculator do
  @moduledoc """
  This module defines a simple calculator that implements the CalculatorBehavior behavior.
  """

  use CalculatorBehavior

  @impl CalculatorBehavior
  def add(a, b) do
    a + b
  end

  @impl CalculatorBehavior
  def subtract(a, b) do
    a - b
  end

  @impl CalculatorBehavior
  def multiply(a, b) do
    a * b
  end

  @impl CalculatorBehavior
  def divide(a, b) do
    a / b
  end
end

# Module to define a scientific calculator that implements the CalculatorBehavior behavior.
defmodule ScientificCalculator do
  @moduledoc """
  This module defines a scientific calculator that implements the CalculatorBehavior behavior.
  """

  use CalculatorBehavior

  @impl CalculatorBehavior
  def add(a, b) do
    a + b
  end

  @impl CalculatorBehavior
  def subtract(a, b) do
    a - b
  end

  @impl CalculatorBehavior
  def multiply(a, b) do
    a * b
  end

  @impl CalculatorBehavior
  def divide(a, b) do
    a / b
  end

  # Additional functions specific to a scientific calculator.
  def power(a, b) do
    :math.pow(a, b)
  end

  def logarithm(a) do
    :math.log(a)
  end

  def trigonometric_sin(a) do
    :math.sin(a)
  end

  def trigonometric_cos(a) do
    :math.cos(a)
  end

  def trigonometric_tan(a) do
    :math.tan(a)
  end
end

# Module to define a financial calculator that implements the CalculatorBehavior behavior.
defmodule FinancialCalculator do
  @moduledoc """
  This module defines a financial calculator that implements the CalculatorBehavior behavior.
  """

  use CalculatorBehavior

  @impl CalculatorBehavior
  def add(a, b) do
    a + b
  end

  @impl CalculatorBehavior
  def subtract(a, b) do
    a - b
  end

  @impl CalculatorBehavior
  def multiply(a, b) do
    a * b
  end

  @impl CalculatorBehavior
  def divide(a, b) do
    a / b
  end

  # Additional functions specific to a financial calculator.
  def calculate_interest(principal, rate, time) do
    principal * rate * time
  end

  def calculate_compound_interest(principal, rate, time) do
    principal * (1 + rate) ** time
  end

  def calculate_present_value(future_value, rate, time) do
    future_value / (1 + rate) ** time
  end

  def calculate_future_value(present_value, rate, time) do
    present_value * (1 + rate) ** time
  end
end

# Module to demonstrate the usage of the different calculators.
defmodule CalculatorDemo do
  @moduledoc """
  This module demonstrates the usage of the different calculators.
  """

  def main() do
    # Create instances of the different calculators.
    simple_calculator = SimpleCalculator.new()
    scientific_calculator = ScientificCalculator.new()
    financial_calculator = FinancialCalculator.new()

    # Perform some calculations using the simple calculator.
    IO.puts("Simple Calculator:")
    IO.puts("2 + 3 = #{simple_calculator.add(2, 3)}")
    IO.puts("5 - 2 = #{simple_calculator.subtract(5, 2)}")
    IO.puts("4 * 5 = #{simple_calculator.multiply(4, 5)}")
    IO.puts("10 / 2 = #{simple_calculator.divide(10, 2)}")

    # Perform some calculations using the scientific calculator.
    IO.puts("\nScientific Calculator:")
    IO.puts("2^3 = #{scientific_calculator.power(2, 3)}")
    IO.puts("log(10) = #{scientific_calculator.logarithm(10)}")
    IO.puts("sin(30) = #{scientific_calculator.trigonometric_sin(30)}")
    IO.puts("cos(45) = #{scientific_calculator.trigonometric_cos(45)}")
    IO.puts("tan(60) = #{scientific_calculator.trigonometric_tan(60)}")

    # Perform some calculations using the financial calculator.
    IO.puts("\nFinancial Calculator:")
    IO.puts("Interest on $1000 at 5% for 1 year = #{financial_calculator.calculate_interest(1000, 0.05, 1)}")
    IO.puts("Compound interest on $1000 at 5% for 1 year = #{financial_calculator.calculate_compound_interest(1000, 0.05, 1)}")
    IO.puts("Present value of $1000 at 5% for 1 year = #{financial_calculator.calculate_present_value(1000, 0.05, 1)}")
    IO.puts("Future value of $1000 at 5% for 1 year = #{financial_calculator.calculate_future_value(1000, 0.05, 1)}")
  end
end

# Call the main function to run the calculator demo.
CalculatorDemo.main()
```
 **Explanation:**

This Elixir code demonstrates the implementation of three different calculators: a simple calculator, a scientific calculator, and a financial calculator.

1. **CalculatorBehavior**:
   - This module defines a behavior for a calculator. It specifies the functions that a calculator must implement to perform basic arithmetic operations.

2. **SimpleCalculator**:
   - This module implements the CalculatorBehavior behavior.
   - It provides basic arithmetic operations like addition, subtraction, multiplication, and division.

3. **ScientificCalculator**:
   - This module also implements the CalculatorBehavior behavior.
   - It extends the functionality of the SimpleCalculator by providing additional functions like power, logarithm, and trigonometric functions.

4. **FinancialCalculator**:
   - This module also implements the CalculatorBehavior behavior.
   - It extends the functionality of the SimpleCalculator by providing financial calculations like interest calculation, compound interest calculation, present value calculation, and future value calculation.

5. **CalculatorDemo**:
   - This module demonstrates the usage of the different calculators.
   - It creates instances of the calculators and performs various calculations using them.

The CalculatorDemo module's `main()` function is called to run the calculator demo. This function prints the results of various calculations performed using the different calculators.