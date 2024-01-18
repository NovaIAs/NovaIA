```elixir
# Import necessary libraries
import Ecto.Query, as: Q
import MyApp.Repo

# Define the Product struct
defmodule MyApp.Product do
  use Ecto.Schema
  schema "products" do
    field :name, :string
    field :description, :string
    field :price, :float
    timestamps()
  end
end

# Define the ProductRepository module
defmodule MyApp.ProductRepository do
  alias MyApp.Product

  # Get all products
  def get_all_products do
    Repo.all(Product)
  end

  # Get a product by ID
  def get_product_by_id(id) do
    Repo.get(Product, id)
  end

  # Create a new product
  def create_product(params) do
    Repo.insert(%Product{}, params)
  end

  # Update a product
  def update_product(%Product{} = product, params) do
    Repo.update(product, params)
  end

  # Delete a product
  def delete_product(%Product{} = product) do
    Repo.delete(product)
  end

  # Get products with a price greater than a given amount
  def get_products_with_price_greater_than(amount) do
    query = Q.from(p in Product)
                |> Q.where(p.price > ^amount)
    Repo.all(query)
  end
end

# Define the ProductController module
defmodule MyApp.ProductController do
  alias MyApp.ProductRepository

  def index(conn, _params) do
    products = ProductRepository.get_all_products()
    render(conn, "index.html", products: products)
  end

  def show(conn, %{"id" => id}) do
    product = ProductRepository.get_product_by_id(id)
    render(conn, "show.html", product: product)
  end

  def new(conn, _params) do
    changeset = ProductRepository.create_product(%{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"product" => product_params}) do
    changeset = ProductRepository.create_product(product_params)

    case Repo.insert(changeset) do
      {:ok, _product} ->
        conn
        |> put_flash(:info, "Product created successfully.")
        |> redirect(to: Routes.product_path(conn, :index))

      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def edit(conn, %{"id" => id}) do
    product = ProductRepository.get_product_by_id(id)
    changeset = ProductRepository.update_product(product, %{})
    render(conn, "edit.html", product: product, changeset: changeset)
  end

  def update(conn, %{"id" => id, "product" => product_params}) do
    product = ProductRepository.get_product_by_id(id)
    changeset = ProductRepository.update_product(product, product_params)

    case Repo.update(changeset) do
      {:ok, _product} ->
        conn
        |> put_flash(:info, "Product updated successfully.")
        |> redirect(to: Routes.product_path(conn, :index))

      {:error, changeset} ->
        render(conn, "edit.html", product: product, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    product = ProductRepository.get_product_by_id(id)
    Repo.delete(product)

    conn
    |> put_flash(:info, "Product deleted successfully.")
    |> redirect(to: Routes.product_path(conn, :index))
  end
end

# Define the ProductRoutes module
defmodule MyApp.ProductRoutes do
  use Phoenix.Router

  scope "/", MyApp do
    pipe_through :browser

    resources "/products", ProductController
  end
end

# Define the MyAppWeb module
defmodule MyAppWeb do
  use Phoenix.Web, :html
  plug Phoenix.LiveView.Flash

  pipeline :browser do
    plug Phoenix.LiveView.Manifest
  end
end
```

This code defines a Product struct, a ProductRepository module, a ProductController module, a ProductRoutes module, and a MyAppWeb module.

The Product struct defines the fields of a product, including its name, description, price, and timestamps.

The ProductRepository module provides functions for interacting with the products table in the database, such as getting all products, getting a product by ID, creating a new product, updating a product, and deleting a product.

The ProductController module handles HTTP requests related to products, such as displaying a list of products, showing a product, creating a new product, updating a product, and deleting a product.

The ProductRoutes module defines the routes for the ProductController module.

The MyAppWeb module defines the web application configuration, including the pipeline for the browser.