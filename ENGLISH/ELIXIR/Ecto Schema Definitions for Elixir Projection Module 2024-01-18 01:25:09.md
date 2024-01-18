```elixir
defmodule EventSourcing.Projection.Product do
  use Ecto.Schema

  @primary_key {:product_id, :integer, autogenerate: true}
  schema "product_projections" do
    field :product_id, :integer
    field :name, :string
    field :description, :string
    field :price, :float
    field :quantity, :integer
    field :total_price, :float
    field :created_at, :utc_datetime
    field :updated_at, :utc_datetime
  end

  @timestamps_opts [type: :utc_datetime]
end

defmodule EventSourcing.Projection.ProductRepository do
  use Ecto.Repo, otp_app: :event_sourcing

  def get_product(product_id) do
    from p in EventSourcing.Projection.Product,
      where: [product_id: ^product_id],
      select: [:product_id, :name, :description, :price, :quantity, :total_price] |> select_fields()
  end

  defp select_fields(), do: fields_for_select!(EventSourcing.Projection.Product)

  def create_product(attrs) do
    %EventSourcing.Projection.Product{}
    |> EventSourcing.Projection.Product.changeset(attrs)
    |> insert!()
  end

  def update_product(%EventSourcing.Projection.Product{} = product, attrs) do
    product
    |> EventSourcing.Projection.Product.changeset(attrs)
    |> update!()
  end

  def delete_product(%EventSourcing.Projection.Product{} = product) do
    delete(product)
  end
end

defmodule EventSourcing.Projection.ProductChangeset do
  use Ecto.Changeset

  alias EventSourcing.Projection.Product

  @required_fields [:product_id, :name, :description, :price, :quantity]
  @optional_fields [:total_price, :created_at, :updated_at]

  def validate(changeset, attrs) do
    changeset
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> update_change(:total_price, fn total_price, _ ->
      total_price || calculate_total_price(attrs)
    end)
  end

  defp calculate_total_price(attrs) do
    attrs[:price] * attrs[:quantity]
  end
end

defmodule EventSourcing.Projection.Order do
  use Ecto.Schema

  @primary_key {:order_id, :integer, autogenerate: true}
  schema "order_projections" do
    field :order_id, :integer
    field :customer_id, :integer
    field :status, :string
    field :total_price, :float
    field :created_at, :utc_datetime
    field :updated_at, :utc_datetime
  end

  @timestamps_opts [type: :utc_datetime]
end

defmodule EventSourcing.Projection.OrderRepository do
  use Ecto.Repo, otp_app: :event_sourcing

  def get_order(order_id) do
    from o in EventSourcing.Projection.Order,
      where: [order_id: ^order_id],
      select: [:order_id, :customer_id, :status, :total_price, :created_at, :updated_at] |> select_fields()
  end

  defp select_fields(), do: fields_for_select!(EventSourcing.Projection.Order)

  def create_order(attrs) do
    %EventSourcing.Projection.Order{}
    |> EventSourcing.Projection.Order.changeset(attrs)
    |> insert!()
  end

  def update_order(%EventSourcing.Projection.Order{} = order, attrs) do
    order
    |> EventSourcing.Projection.Order.changeset(attrs)
    |> update!()
  end

  def delete_order(%EventSourcing.Projection.Order{} = order) do
    delete(order)
  end
end

defmodule EventSourcing.Projection.OrderChangeset do
  use Ecto.Changeset

  alias EventSourcing.Projection.Order

  @required_fields [:order_id, :customer_id, :status, :total_price]
  @optional_fields [:created_at, :updated_at]

  def validate(changeset, attrs) do
    changeset
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end
end

defmodule EventSourcing.Projection.OrderItem do
  use Ecto.Schema

  @primary_key {:order_item_id, :integer, autogenerate: true}
  schema "order_item_projections" do
    field :order