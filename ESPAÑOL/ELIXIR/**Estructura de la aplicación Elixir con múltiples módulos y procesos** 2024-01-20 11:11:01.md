```elixir
# Módulo principal

defmodule MiModuloPrincipal do
  # Función principal
  def main() do
    # Crea un proceso anónimo
    spawn(fn ->
      # Imprime un mensaje
      IO.puts("Hola, mundo")

      # Termina el proceso
      exit(:normal)
    end)
  end
end

# Inicia el módulo principal
MiModuloPrincipal.main()
```

Este código crea un proceso anónimo, que es un proceso que se ejecuta de forma independiente al proceso principal. El proceso anónimo imprime un mensaje y luego termina. El código también utiliza la función `spawn` para crear el proceso anónimo.

```elixir
# Módulo para gestionar datos de clientes

defmodule Clientes do
  # Estructura para representar datos de clientes
  defstruct [:nombre, :apellido, :email]

  # Función para crear un nuevo cliente
  def crear_cliente(nombre, apellido, email) do
    %Clientes{nombre: nombre, apellido: apellido, email: email}
  end

  # Función para obtener todos los clientes
  def obtener_todos_clientes() do
    # Aquí se obtendrían todos los clientes de una base de datos
    # Por ahora, creamos una lista de clientes de ejemplo
    [
      crear_cliente("Juan", "García", "juan@ejemplo.com"),
      crear_cliente("María", "Pérez", "maria@ejemplo.com"),
      crear_cliente("Pedro", "Gómez", "pedro@ejemplo.com")
    ]
  end
end

# Módulo principal

defmodule MiModuloPrincipal do
  # Función principal
  def main() do
    # Obtiene todos los clientes
    clientes = Clientes.obtener_todos_clientes()

    # Imprime los datos de cada cliente
    Enum.each(clientes, fn cliente ->
      IO.puts("#{cliente.nombre} #{cliente.apellido} (#{cliente.email})")
    end)
  end
end

# Inicia el módulo principal
MiModuloPrincipal.main()
```

Este código crea un módulo para gestionar datos de clientes. El módulo tiene una estructura para representar datos de clientes, y funciones para crear nuevos clientes y obtener todos los clientes. El código también utiliza la función `Enum.each` para recorrer la lista de clientes y imprimir los datos de cada cliente.

```elixir
# Módulo para gestionar datos de productos

defmodule Productos do
  # Estructura para representar datos de productos
  defstruct [:nombre, :precio, :stock]

  # Función para crear un nuevo producto
  def crear_producto(nombre, precio, stock) do
    %Productos{nombre: nombre, precio: precio, stock: stock}
  end

  # Función para obtener todos los productos
  def obtener_todos_productos() do
    # Aquí se obtendrían todos los productos de una base de datos
    # Por ahora, creamos una lista de productos de ejemplo
    [
      crear_producto("Camiseta", 10, 25),
      crear_producto("Pantalón", 20, 15),
      crear_producto("Zapatos", 30, 10)
    ]
  end
end

# Módulo para gestionar pedidos

defmodule Pedidos do
  # Estructura para representar datos de pedidos
  defstruct [:cliente, :productos, :total]

  # Función para crear un nuevo pedido
  def crear_pedido(cliente, productos) do
    # Calcula el total del pedido
    total = Enum.reduce(productos, 0, fn producto, acc ->
      acc + producto.precio * producto.stock
    end)

    %Pedidos{cliente: cliente, productos: productos, total: total}
  end

  # Función para obtener todos los pedidos
  def obtener_todos_pedidos() do
    # Aquí se obtendrían todos los pedidos de una base de datos
    # Por ahora, creamos una lista de pedidos de ejemplo
    [
      crear_pedido("Juan García", [
        Productos.crear_producto("Camiseta", 10, 2),
        Productos.crear_producto("Pantalón", 20, 1)
      ]),
      crear_pedido("María Pérez", [
        Productos.crear_producto("Zapatos", 30, 2),
        Productos.crear_producto("Camiseta", 10, 1)