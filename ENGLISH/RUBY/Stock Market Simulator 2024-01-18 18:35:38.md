```ruby
# This Ruby code simulates a simple stock trading system.

# Define the Stock class to represent a stock.
class Stock
  attr_accessor :symbol, :name, :price

  def initialize(symbol, name, price)
    @symbol = symbol
    @name = name
    @price = price
  end

  def to_s
    "#{@symbol} (#{@name}): $#{@price}"
  end
end

# Define the Portfolio class to represent a portfolio of stocks.
class Portfolio
  attr_accessor :stocks

  def initialize
    @stocks = []
  end

  def add_stock(stock)
    @stocks << stock
  end

  def total_value
    @stocks.map(&:price).sum
  end

  def to_s
    "Portfolio:\n" + @stocks.join("\n")
  end
end

# Define the Market class to represent the stock market.
class Market
  attr_accessor :stocks

  def initialize
    @stocks = []
  end

  def add_stock(stock)
    @stocks << stock
  end

  def get_stock_by_symbol(symbol)
    @stocks.find { |stock| stock.symbol == symbol }
  end

  def to_s
    "Market:\n" + @stocks.join("\n")
  end
end

# Create a new market.
market = Market.new

# Add some stocks to the market.
market.add_stock(Stock.new("AAPL", "Apple Inc.", 120.00))
market.add_stock(Stock.new("GOOG", "Alphabet Inc.", 1000.00))
market.add_stock(Stock.new("AMZN", "Amazon.com, Inc.", 1800.00))

# Create a new portfolio.
portfolio = Portfolio.new

# Add some stocks to the portfolio.
portfolio.add_stock(market.get_stock_by_symbol("AAPL"))
portfolio.add_stock(market.get_stock_by_symbol("GOOG"))

# Print the portfolio.
puts portfolio

# Print the total value of the portfolio.
puts "Total value of the portfolio: $#{portfolio.total_value}"
```

## Explanation:

- The `Stock` class represents a single stock, with attributes `symbol` (the ticker symbol), `name` (the full name of the company), and `price` (the current price of the stock).

- The `Portfolio` class represents a collection of stocks. It has a method `add_stock` to add a new stock to the portfolio, and a method `total_value` to calculate the total value of all the stocks in the portfolio.

- The `Market` class represents the stock market. It has a method `add_stock` to add a new stock to the market, and a method `get_stock_by_symbol` to get a stock by its symbol.

- The code creates a new market, adds some stocks to it, creates a new portfolio, adds some stocks to it, and then prints the portfolio and its total value.

This code is a more complex and differentiated example of a Ruby program, with multiple classes and methods and a more complex data structure (the `Portfolio` class). It also includes a more complex example of input/output, with the use of the `puts` method to print the portfolio and its total value.