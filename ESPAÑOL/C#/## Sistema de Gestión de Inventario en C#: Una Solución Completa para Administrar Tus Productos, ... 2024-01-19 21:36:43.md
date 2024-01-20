# Sistema de Gestión de Inventario en C#

Este código es un sistema de gestión de inventario completo que permite a los usuarios administrar sus productos, pedidos y clientes. También incluye características de seguridad como la autenticación de usuarios y el control de acceso.

```csharp
// Librerías necesarias
using System;
using System.Collections.Generic;
using System.Data.SqlClient;

// Clase principal del programa
public class Inventario
{
    // Atributos
    private SqlConnection conexion; // Conexión a la base de datos
    private List<Producto> productos; // Lista de productos
    private List<Pedido> pedidos; // Lista de pedidos
    private List<Cliente> clientes; // Lista de clientes

    // Propiedades
    public List<Producto> Productos { get { return productos; } }
    public List<Pedido> Pedidos { get { return pedidos; } }
    public List<Cliente> Clientes { get { return clientes; } }

    // Constructor
    public Inventario()
    {
        // Crear la conexión a la base de datos
        conexion = new SqlConnection(@"Server=.\SQLEXPRESS;Database=Inventario;Trusted_Connection=True;");

        // Crear las listas de productos, pedidos y clientes
        productos = new List<Producto>();
        pedidos = new List<Pedido>();
        clientes = new List<Cliente>();
    }

    // Métodos

    // Método para cargar los datos de la base de datos
    public void CargarDatos()
    {
        // Cargar los productos
        productos.Clear();
        string query = "SELECT * FROM Producto";
        SqlCommand command = new SqlCommand(query, conexion);
        SqlDataReader reader = command.ExecuteReader();
        while (reader.Read())
        {
            productos.Add(new Producto(reader.GetInt32(0), reader.GetString(1), reader.GetDecimal(2), reader.GetInt32(3)));
        }
        reader.Close();

        // Cargar los pedidos
        pedidos.Clear();
        query = "SELECT * FROM Pedido";
        command = new SqlCommand(query, conexion);
        reader = command.ExecuteReader();
        while (reader.Read())
        {
            pedidos.Add(new Pedido(reader.GetInt32(0), reader.GetInt32(1), reader.GetDateTime(2), reader.GetDecimal(3)));
        }
        reader.Close();

        // Cargar los clientes
        clientes.Clear();
        query = "SELECT * FROM Cliente";
        command = new SqlCommand(query, conexion);
        reader = command.ExecuteReader();
        while (reader.Read())
        {
            clientes.Add(new Cliente(reader.GetInt32(0), reader.GetString(1), reader.GetString(2), reader.GetString(3)));
        }
        reader.Close();
    }

    // Método para guardar los datos en la base de datos
    public void GuardarDatos()
    {
        // Guardar los productos
        foreach (Producto producto in productos)
        {
            // Si el producto es nuevo
            if (producto.Id == 0)
            {
                string query = "INSERT INTO Producto (Nombre, Precio, Stock) VALUES (@Nombre, @Precio, @Stock)";
                SqlCommand command = new SqlCommand(query, conexion);
                command.Parameters.AddWithValue("@Nombre", producto.Nombre);
                command.Parameters.AddWithValue("@Precio", producto.Precio);
                command.Parameters.AddWithValue("@Stock", producto.Stock);
                command.ExecuteNonQuery();

                // Obtener el ID del producto recién creado
                query = "SELECT @@IDENTITY";
                command = new SqlCommand(query, conexion);
                producto.Id = (int)command.ExecuteScalar();
            }
            // Si el producto ya existe
            else
            {
                string query = "UPDATE Producto SET Nombre = @Nombre, Precio = @Precio, Stock = @Stock WHERE Id = @Id";
                SqlCommand command = new SqlCommand(query, conexion);
                command.Parameters.AddWithValue("@Id", producto.Id);
                command.Parameters.AddWithValue("@Nombre", producto.Nombre);
                command.Parameters.AddWithValue("@Precio", producto.Precio);
                command.Parameters.AddWithValue("@Stock", producto.Stock);
                command.ExecuteNonQuery();
            }
        }

        // Guardar los pedidos
        foreach (Pedido pedido in pedidos)
        {
            // Si el pedido es nuevo
            if (pedido.Id == 0)
            {
                string query = "INSERT INTO Pedido (ClienteId, Fecha, Total) VALUES (@ClienteId, @Fecha, @Total)";
                SqlCommand command = new SqlCommand(query, conexion);
                command.Parameters.AddWithValue("@ClienteId", pedido.ClienteId);
                command.Parameters.AddWithValue("@Fecha", pedido.Fecha);
                command.Parameters.AddWithValue("@Total", pedido.Total);
                command.ExecuteNonQuery();

                // Obtener el ID del pedido recién creado
                query = "SELECT @@IDENTITY";
                command = new SqlCommand(query, conexion);
                pedido.Id = (int)command.ExecuteScalar();

                // Guardar los detalles del pedido
                foreach (DetallePedido detalle in pedido.Detalles)
                {
                    query = "INSERT INTO DetallePedido (PedidoId, ProductoId, Cantidad, PrecioUnitario) VALUES (@PedidoId, @ProductoId, @Cantidad, @PrecioUnitario)";
                    command = new SqlCommand(query, conexion);
                    command.Parameters.AddWithValue("@PedidoId", pedido.Id);
                    command.Parameters.AddWithValue("@ProductoId", detalle.ProductoId);
                    command.Parameters.AddWithValue("@Cantidad", detalle.Cantidad);
                    command.Parameters.AddWithValue("@PrecioUnitario", detalle.PrecioUnitario);
                    command.ExecuteNonQuery();
                }
            }
            // Si el pedido ya existe
            else
            {
                string query = "UPDATE Pedido SET ClienteId = @ClienteId, Fecha = @Fecha, Total = @Total WHERE Id = @Id";
                SqlCommand command = new SqlCommand(query, conexion);
                command.Parameters.AddWithValue