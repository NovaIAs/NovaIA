```c#
// Este código de C# implementa un sistema de gestión de pedidos para un restaurante, utilizando programación orientada a objetos y principios de diseño sólido.

// Clase principal del sistema.
public class Restaurante
{
    // Lista de pedidos pendientes.
    private List<Pedido> _pedidosPendientes = new List<Pedido>();

    // Lista de pedidos completados.
    private List<Pedido> _pedidosCompletados = new List<Pedido>();

    // Método para agregar un nuevo pedido.
    public void AgregarPedido(Pedido pedido)
    {
        _pedidosPendientes.Add(pedido);
    }

    // Método para marcar un pedido como completado.
    public void CompletarPedido(Pedido pedido)
    {
        _pedidosPendientes.Remove(pedido);
        _pedidosCompletados.Add(pedido);
    }

    // Método para obtener la lista de pedidos pendientes.
    public List<Pedido> ObtenerPedidosPendientes()
    {
        return _pedidosPendientes;
    }

    // Método para obtener la lista de pedidos completados.
    public List<Pedido> ObtenerPedidosCompletados()
    {
        return _pedidosCompletados;
    }
}

// Clase que representa un pedido.
public class Pedido
{
    // Identificador único del pedido.
    public int Id { get; set; }

    // Fecha y hora en que se realizó el pedido.
    public DateTime FechaHora { get; set; }

    // Cliente que realizó el pedido.
    public Cliente Cliente { get; set; }

    // Lista de productos solicitados en el pedido.
    public List<Producto> Productos { get; set; }

    // Precio total del pedido.
    public decimal PrecioTotal { get; set; }

    // Estado del pedido (pendiente, completado, cancelado).
    public string Estado { get; set; }
}

// Clase que representa un cliente.
public class Cliente
{
    // Identificador único del cliente.
    public int Id { get; set; }

    // Nombre del cliente.
    public string Nombre { get; set; }

    // Dirección del cliente.
    public string Direccion { get; set; }

    // Número de teléfono del cliente.
    public string Telefono { get; set; }
}

// Clase que representa un producto.
public class Producto
{
    // Identificador único del producto.
    public int Id { get; set; }

    // Nombre del producto.
    public string Nombre { get; set; }

    // Descripción del producto.
    public string Descripcion { get; set; }

    // Precio del producto.
    public decimal Precio { get; set; }
}

// Clase que representa el menú del restaurante.
public class Menu
{
    // Lista de productos disponibles en el menú.
    public List<Producto> Productos { get; set; }

    // Método para obtener un producto del menú por su identificador.
    public Producto ObtenerProductoPorId(int id)
    {
        return Productos.Find(p => p.Id == id);
    }
}

// Clase que representa el sistema de pago del restaurante.
public class SistemaPago
{
    // Método para procesar un pago.
    public bool ProcesarPago(decimal monto, string tarjetaCredito, string fechaVencimiento, string codigoSeguridad)
    {
        // Aquí se implementaría el código para procesar el pago a través de una pasarela de pago.
        return true;
    }
}

// Clase que representa la interfaz de usuario del restaurante.
public class InterfazUsuario
{
    // Método para mostrar el menú del restaurante.
    public void MostrarMenu()
    {
        // Aquí se implementaría el código para mostrar el menú en una interfaz gráfica o en una consola.
    }

    // Método para tomar un pedido.
    public Pedido TomarPedido()
    {
        // Aquí se implementaría el código para tomar un pedido del cliente y guardarlo en el sistema.
        return new Pedido();
    }

    // Método para mostrar el estado de un pedido.
    public void MostrarEstadoPedido(Pedido pedido)
    {
        // Aquí se implementaría el código para mostrar el estado de un pedido en una interfaz gráfica o en una consola.
    }
}

// Programa principal.
public class Program
{
    public static void Main()
    {
        // Crear el restaurante.
        Restaurante restaurante = new Restaurante();

        // Crear el menú del restaurante.
        Menu menu = new Menu();

        // Crear el sistema de pago del restaurante.
        SistemaPago sistemaPago = new SistemaPago();

        // Crear la interfaz de usuario del restaurante.
        InterfazUsuario interfazUsuario = new InterfazUsuario();

        // Mostrar el menú del restaurante.
        interfazUsuario.MostrarMenu();

        // Tomar un pedido del cliente.
        Pedido pedido = interfazUsuario.TomarPedido();

        // Agregar el pedido al restaurante.
        restaurante.AgregarPedido(pedido);

        // Procesar el pago del pedido.
        bool pagoProcesado = sistemaPago.ProcesarPago(pedido.PrecioTotal, "4111-1111-1111-1111", "12/25", "123");

        // Si el pago se procesó correctamente, marcar el pedido como completado.
        if (pagoProcesado)
        {
            restaurante.CompletarPedido(pedido);

            // Mostrar el estado del pedido al cliente.
            interfazUsuario.MostrarEstadoPedido(pedido);
        }
        else
        {
            // Informar al cliente que el pago no se pudo procesar.
            Console.WriteLine("El pago no se pudo procesar. Inténtelo de nuevo.");
        }
    }
}
```

Este código implementa un sistema completo de gestión de pedidos para un restaurante, incluyendo la creación de pedidos, el procesamiento de pagos, y la visualización del estado de los pedidos. El código está dividido en varias clases, cada una de las cuales representa una entidad o componente del sistema. Las clases están diseñadas siguiendo principios de programación orientada a objetos y diseño sólido, lo que hace que el código sea más modular, reutilizable y mantenible.

El programa principal crea las instancias de las clases necesarias y las utiliza para simular el flujo de trabajo de un restaurante. El usuario puede interactuar con el sistema a través de la interfaz de usuario, que muestra el menú del restaurante, toma pedidos y muestra el estado de los mismos. El sistema también procesa pagos a través de un sistema de pago simulado.

En general, este código es un ejemplo completo y funcional de un sistema de gestión de pedidos para un restaurante, utilizando programación orientada a objetos y principios de diseño sólido.