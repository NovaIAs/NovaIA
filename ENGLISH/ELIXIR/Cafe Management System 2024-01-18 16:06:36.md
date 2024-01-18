```elixir
defmodule Cafe do
  defstruct [:name, :menu, :orders]

  def start_cafe(name) do
    menu = %{
      coffee: %{
        short_black: %{name: "Short Black", price: 2.50},
        long_black: %{name: "Long Black", price: 3.00},
        flat_white: %{name: "Flat White", price: 3.50},
        cappuccino: %{name: "Cappuccino", price: 4.00},
        latte: %{name: "Latte", price: 4.50},
        mocha: %{name: "Mocha", price: 5.00}
      },
      tea: %{
        black_tea: %{name: "Black Tea", price: 2.00},
        green_tea: %{name: "Green Tea", price: 2.50},
        herbal_tea: %{name: "Herbal Tea", price: 3.00}
      },
      food: %{
        cake: %{name: "Cake", price: 4.00},
        muffin: %{name: "Muffin", price: 3.00},
        sandwich: %{name: "Sandwich", price: 5.00}
      }
    }

    orders = []

    %Cafe{name: name, menu: menu, orders: orders}
  end

  def take_order(cafe, item, quantity) do
    {:ok, order} = Cafe.get_order_item(cafe, item)
    updated_orders = [order | cafe.orders]
    %Cafe{cafe | orders: updated_orders}
  end

  def get_order_item(cafe, item) do
    case Map.fetch(cafe.menu, item) do
      {:ok, item_details} -> {:ok, %{item: item, quantity: 1, price: item_details.price}}
      :error -> {:error, "Item not found"}
    end
  end

  def get_total(cafe) do
    Enum.reduce(cafe.orders, 0, fn order, acc -> acc + order.price * order.quantity end)
  end

  def print_receipt(cafe) do
    IO.puts("Cafe: #{cafe.name}")
    IO.puts("Orders:")

    Enum.each(cafe.orders, fn order ->
      IO.puts("#{order.item} x#{order.quantity}: $#{order.price * order.quantity}")
    end)

    IO.puts("Total: $#{get_total(cafe)}")
  end
end

cafe = Cafe.start_cafe("The Coffee Shop")

order_1 = Cafe.take_order(cafe, :short_black, 2)
order_2 = Cafe.take_order(cafe, :muffin, 1)
order_3 = Cafe.take_order(cafe, :latte, 1)

Cafe.print_receipt(cafe)
```

This code simulates a simple cafe system in Elixir. It defines a `Cafe` struct with fields for the cafe's name, menu, and orders. The `start_cafe/1` function takes a cafe name and creates a new `Cafe` struct with a default menu and an empty list of orders.

The `take_order/3` function takes a `Cafe` struct, an item to order, and the quantity ordered. It gets the item's details from the menu and creates an `Order` struct with the item, quantity, and price. It then updates the cafe's orders list with the new order.

The `get_order_item/2` function takes a `Cafe` struct and an item to order. It gets the item's details from the menu and returns an `Order` struct with the item, a quantity of 1, and the price.

The `get_total/1` function takes a `Cafe` struct and returns the total price of all the orders in the cafe.

The `print_receipt/1` function takes a `Cafe` struct and prints a receipt with the cafe's name, the orders, and the total price.

The code also includes examples of how to use the `Cafe` module to create a cafe, take orders, and print a receipt.