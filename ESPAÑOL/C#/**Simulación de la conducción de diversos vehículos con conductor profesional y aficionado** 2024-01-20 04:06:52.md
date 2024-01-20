```c#
// Crear una clase abstracta para definir una interfaz común para diferentes tipos de vehículos.
public abstract class Vehiculo
{
    protected string marca;
    protected string modelo;
    protected int año;

    public Vehiculo(string marca, string modelo, int año)
    {
        this.marca = marca;
        this.modelo = modelo;
        this.año = año;
    }

    public abstract void Acelerar();
    public abstract void Frenar();
    public abstract void Girar();
}

// Crear clases concretas que implementen la interfaz definida en la clase abstracta Vehiculo.
// Cada clase concreta representa un tipo específico de vehículo.

public class Coche : Vehiculo
{
    public Coche(string marca, string modelo, int año) : base(marca, modelo, año) { }

    public override void Acelerar()
    {
        Console.WriteLine("El coche acelera.");
    }

    public override void Frenar()
    {
        Console.WriteLine("El coche frena.");
    }

    public override void Girar()
    {
        Console.WriteLine("El coche gira.");
    }
}

public class Motocicleta : Vehiculo
{
    public Motocicleta(string marca, string modelo, int año) : base(marca, modelo, año) { }

    public override void Acelerar()
    {
        Console.WriteLine("La motocicleta acelera.");
    }

    public override void Frenar()
    {
        Console.WriteLine("La motocicleta frena.");
    }

    public override void Girar()
    {
        Console.WriteLine("La motocicleta gira.");
    }
}

public class Camión : Vehiculo
{
    public Camión(string marca, string modelo, int año) : base(marca, modelo, año) { }

    public override void Acelerar()
    {
        Console.WriteLine("El camión acelera.");
    }

    public override void Frenar()
    {
        Console.WriteLine("El camión frena.");
    }

    public override void Girar()
    {
        Console.WriteLine("El camión gira.");
    }
}

// Crear una clase abstracta para definir una interfaz común para diferentes tipos de conductores.
public abstract class Conductor
{
    protected string nombre;
    protected string licencia;

    public Conductor(string nombre, string licencia)
    {
        this.nombre = nombre;
        this.licencia = licencia;
    }

    public abstract void Conducir(Vehiculo vehiculo);
}

// Crear clases concretas que implementen la interfaz definida en la clase abstracta Conductor.
// Cada clase concreta representa un tipo específico de conductor.

public class ConductorProfesional : Conductor
{
    public ConductorProfesional(string nombre, string licencia) : base(nombre, licencia) { }

    public override void Conducir(Vehiculo vehiculo)
    {
        Console.WriteLine("El conductor profesional conduce el vehículo.");
    }
}

public class ConductorAficionado : Conductor
{
    public ConductorAficionado(string nombre, string licencia) : base(nombre, licencia) { }

    public override void Conducir(Vehiculo vehiculo)
    {
        Console.WriteLine("El conductor aficionado conduce el vehículo.");
    }
}

// Crear una clase principal para probar las clases creadas anteriormente.
public class Program
{
    public static void Main(string[] args)
    {
        // Crear un coche.
        Vehiculo coche = new Coche("Toyota", "Corolla", 2023);

        // Crear un conductor profesional.
        Conductor conductorProfesional = new ConductorProfesional("Juan Pérez", "123456789");

        // El conductor profesional conduce el coche.
        conductorProfesional.Conducir(coche);

        // Crear una motocicleta.
        Vehiculo motocicleta = new Motocicleta("Honda", "CBR600RR", 2022);

        // Crear un conductor aficionado.
        Conductor conductorAficionado = new ConductorAficionado("María Gómez", "987654321");

        // El conductor aficionado conduce la motocicleta.
        conductorAficionado.Conducir(motocicleta);

        // Crear un camión.
        Vehiculo camión = new Camión("Mercedes-Benz", "Actros", 2021);

        // El conductor profesional conduce el camión.
        conductorProfesional.Conducir(camión);
    }
}
```

Explicación del código:

* **Clase Vehiculo:** Representa un vehículo genérico con las propiedades marca, modelo y año.
* **Clases Coche, Motocicleta y Camión:** Implementan la interfaz definida en la clase Vehiculo para representar tipos específicos de vehículos.
* **Clase Conductor:** Representa un conductor genérico con las propiedades nombre y licencia.
* **Clases ConductorProfesional y ConductorAficionado:** Implementan la interfaz definida en la clase Conductor para representar tipos específicos de conductores.
* **Clase Program:** Es la clase principal donde se crea un coche, una motocicleta y un camión, y se les asignan conductores profesionales y aficionados para que los conduzcan.

El código demuestra polimorfismo y herencia, ya que las clases concretas heredan de las clases abstractas y comparten una interfaz común, pero implementan diferentes comportamientos.