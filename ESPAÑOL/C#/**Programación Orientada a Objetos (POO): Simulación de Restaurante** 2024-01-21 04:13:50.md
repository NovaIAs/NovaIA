**Programación Orientada a Objetos (POO): Simulación de un Restaurante**

**Introducción:**

Este código simula el funcionamiento de un restaurante, utilizando conceptos de Programación Orientada a Objetos (POO) y abstracción de datos. La simulación incluye la creación de objetos que representan a los clientes, los pedidos y los camareros, así como la gestión de las mesas y la cocina.

**Código:**

```c#
// Clase Cliente
public class Cliente
{
    public string Nombre { get; set; }
    public List<Pedido> Pedidos { get; set; }

    public Cliente(string nombre)
    {
        Nombre = nombre;
        Pedidos = new List<Pedido>();
    }
}

// Clase Pedido
public class Pedido
{
    public string NombrePlato { get; set; }
    public double Precio { get; set; }

    public Pedido(string nombrePlato, double precio)
    {
        NombrePlato = nombrePlato;
        Precio = precio;
    }
}

// Clase Camarero
public class Camarero
{
    public string Nombre { get; set; }
    public List<Cliente> Clientes { get; set; }

    public Camarero(string nombre)
    {
        Nombre = nombre;
        Clientes = new List<Cliente>();
    }

    public void TomarPedido(Cliente cliente, Pedido pedido)
    {
        cliente.Pedidos.Add(pedido);
    }
}

// Clase Mesa
public class Mesa
{
    public int Numero { get; set; }
    public bool Ocupada { get; set; }

    public Mesa(int numero)
    {
        Numero = numero;
        Ocupada = false;
    }
}

// Clase Cocina
public class Cocina
{
    public List<Pedido> PedidosPendientes { get; set; }

    public Cocina()
    {
        PedidosPendientes = new List<Pedido>();
    }

    public void PrepararPedido(Pedido pedido)
    {
        // Aquí se simularía la preparación del plato
        PedidosPendientes.Remove(pedido);
    }
}

// Clase Restaurante
public class Restaurante
{
    public List<Cliente> Clientes { get; set; }
    public List<Camarero> Camareros { get; set; }
    public List<Mesa> Mesas { get; set; }
    public Cocina Cocina { get; set; }

    public Restaurante()
    {
        Clientes = new List<Cliente>();
        Camareros = new List<Camarero>();
        Mesas = new List<Mesa>();
        Cocina = new Cocina();
    }

    public void IniciarSimulacion()
    {
        // Aquí se simularía la llegada de clientes, la toma de pedidos, la preparación de los platos y la entrega de los pedidos a los clientes.
    }
}

// Programa Principal
class Program
{
    static void Main(string[] args)
    {
        Restaurante restaurante = new Restaurante();

        // Crear clientes
        Cliente cliente1 = new Cliente("Juan");
        Cliente cliente2 = new Cliente("María");

        // Crear camareros
        Camarero camarero1 = new Camarero("Pedro");
        Camarero camarero2 = new Camarero("Ana");

        // Crear mesas
        Mesa mesa1 = new Mesa(1);
        Mesa mesa2 = new Mesa(2);

        // Asignar clientes a mesas
        mesa1.Ocupada = true;
        mesa2.Ocupada = true;

        // Tomar pedidos
        camarero1.TomarPedido(cliente1, new Pedido("Pizza", 10.0));
        camarero2.TomarPedido(cliente2, new Pedido("Pasta", 12.0));

        // Preparar pedidos
        restaurante.Cocina.PrepararPedido(cliente1.Pedidos[0]);
        restaurante.Cocina.PrepararPedido(cliente2.Pedidos[0]);

        // Entregar pedidos
        camarero1.TomarPedido(cliente1, new Pedido("Postre", 5.0));
        camarero2.TomarPedido(cliente2, new Pedido("Bebida", 3.0));

        // Mostrar la factura
        Console.WriteLine("Factura:");
        foreach (Cliente cliente in restaurante.Clientes)
        {
            Console.WriteLine("Cliente: {0}", cliente.Nombre);
            foreach (Pedido pedido in cliente.Pedidos)
            {
                Console.WriteLine("Plato: {0}, Precio: {1}", pedido.NombrePlato, pedido.Precio);
            }
        }
    }
}
```

**Explicación:**

* **Clase Cliente:** Representa a los clientes que visitan el restaurante. Cada cliente tiene un nombre y una lista de pedidos.
* **Clase Pedido:** Representa los pedidos que hacen los clientes. Cada pedido tiene un nombre de plato y un precio.
* **Clase Camarero:** Representa a los camareros que trabajan en el restaurante. Cada camarero tiene un nombre y una lista de clientes a los que atiende.
* **Clase Mesa:** Representa las mesas del restaurante. Cada mesa tiene un número y un estado de ocupación.
* **Clase Cocina:** Representa la cocina del restaurante. La cocina tiene una lista de pedidos pendientes que deben ser preparados.
* **Clase Restaurante:** Representa el restaurante en sí. El restaurante tiene una lista de clientes, camareros, mesas y una cocina.
* **Programa Principal:** Crea instancias de las clases anteriores y simula el funcionamiento del restaurante. El programa crea clientes, camareros, mesas y una cocina. A continuación, asigna clientes a mesas, toma pedidos, prepara pedidos y entrega pedidos. Finalmente, muestra la factura de los clientes.

Este código es un ejemplo de cómo utilizar la POO para simular el funcionamiento de un sistema del mundo real. La POO permite abstraer los datos y el comportamiento del sistema en objetos, lo que facilita el diseño, la implementación y el mantenimiento del software.