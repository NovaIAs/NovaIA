```c#
// Creación de una clase abstracta para representar un vehículo.
public abstract class Vehiculo
{
    // Propiedades comunes a todos los vehículos.
    public string Marca { get; set; }
    public string Modelo { get; set; }
    public int Año { get; set; }

    // Método abstracto para obtener el tipo de vehículo.
    public abstract string ObtenerTipo();
}

// Clase derivada que representa un automóvil.
public class Automovil : Vehiculo
{
    // Propiedades específicas de los automóviles.
    public int NumeroDePuertas { get; set; }
    public string TipoDeCombustible { get; set; }

    // Implementación del método abstracto para obtener el tipo de vehículo.
    public override string ObtenerTipo()
    {
        return "Automóvil";
    }
}

// Clase derivada que representa una motocicleta.
public class Motocicleta : Vehiculo
{
    // Propiedades específicas de las motocicletas.
    public int Cilindrada { get; set; }
    public string TipoDeMotocicleta { get; set; }

    // Implementación del método abstracto para obtener el tipo de vehículo.
    public override string ObtenerTipo()
    {
        return "Motocicleta";
    }
}

// Clase derivada que representa un camión.
public class Camion : Vehiculo
{
    // Propiedades específicas de los camiones.
    public int CapacidadDeCarga { get; set; }
    public string TipoDeCamion { get; set; }

    // Implementación del método abstracto para obtener el tipo de vehículo.
    public override string ObtenerTipo()
    {
        return "Camión";
    }
}

// Clase principal para probar las clases de vehículos.
public class Program
{
    // Método principal del programa.
    public static void Main(string[] args)
    {
        // Creación de una lista de vehículos.
        List<Vehiculo> vehiculos = new List<Vehiculo>();

        // Adición de algunos vehículos a la lista.
        vehiculos.Add(new Automovil { Marca = "Toyota", Modelo = "Corolla", Año = 2022, NumeroDePuertas = 4, TipoDeCombustible = "Gasolina" });
        vehiculos.Add(new Motocicleta { Marca = "Honda", Modelo = "CBR1000RR", Año = 2023, Cilindrada = 1000, TipoDeMotocicleta = "Deportiva" });
        vehiculos.Add(new Camion { Marca = "Ford", Modelo = "F-150", Año = 2021, CapacidadDeCarga = 1000, TipoDeCamion = "Pick-up" });

        // Recorrido de la lista de vehículos para mostrar su información.
        foreach (Vehiculo vehiculo in vehiculos)
        {
            Console.WriteLine($"Tipo de vehículo: {vehiculo.ObtenerTipo()}");
            Console.WriteLine($"Marca: {vehiculo.Marca}");
            Console.WriteLine($"Modelo: {vehiculo.Modelo}");
            Console.WriteLine($"Año: {vehiculo.Año}");

            // Mostrar información específica de cada tipo de vehículo.
            if (vehiculo is Automovil)
            {
                Automovil automovil = (Automovil)vehiculo;
                Console.WriteLine($"Número de puertas: {automovil.NumeroDePuertas}");
                Console.WriteLine($"Tipo de combustible: {automovil.TipoDeCombustible}");
            }
            else if (vehiculo is Motocicleta)
            {
                Motocicleta motocicleta = (Motocicleta)vehiculo;
                Console.WriteLine($"Cilindrada: {motocicleta.Cilindrada}");
                Console.WriteLine($"Tipo de motocicleta: {motocicleta.TipoDeMotocicleta}");
            }
            else if (vehiculo is Camion)
            {
                Camion camion = (Camion)vehiculo;
                Console.WriteLine($"Capacidad de carga: {camion.CapacidadDeCarga}");
                Console.WriteLine($"Tipo de camión: {camion.TipoDeCamion}");
            }

            Console.WriteLine();
        }
    }
}
```

Explicación:

* Se define una clase abstracta `Vehiculo` con propiedades comunes a todos los vehículos y un método abstracto `ObtenerTipo()`.
* Se definen tres clases derivadas: `Automovil`, `Motocicleta` y `Camion`, cada una con propiedades específicas de su tipo de vehículo y una implementación del método `ObtenerTipo()`.
* La clase principal `Program` crea una lista de vehículos, añade algunos vehículos a la lista y recorre la lista para mostrar su información.
* El uso de polimorfismo permite mostrar información específica de cada tipo de vehículo.