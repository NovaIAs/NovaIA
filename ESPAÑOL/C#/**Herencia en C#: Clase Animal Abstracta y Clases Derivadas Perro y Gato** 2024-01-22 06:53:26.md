```c#
// **Clase abstracta Animal**

public abstract class Animal
{
    // **Campos**

    private string _nombre;
    private string _tipo;
    private int _edad;

    // **Propiedades**

    public string Nombre
    {
        get { return _nombre; }
        set { _nombre = value; }
    }

    public string Tipo
    {
        get { return _tipo; }
        set { _tipo = value; }
    }

    public int Edad
    {
        get { return _edad; }
        set { _edad = value; }
    }

    // **Constructores**

    public Animal(string nombre, string tipo, int edad)
    {
        _nombre = nombre;
        _tipo = tipo;
        _edad = edad;
    }

    // **Métodos**

    public abstract void Comer();
    public abstract void Dormir();
}

// **Clase Perro**

public class Perro : Animal
{
    // **Campos**

    private string _raza;

    // **Propiedades**

    public string Raza
    {
        get { return _raza; }
        set { _raza = value; }
    }

    // **Constructores**

    public Perro(string nombre, string tipo, int edad, string raza)
        : base(nombre, tipo, edad)
    {
        _raza = raza;
    }

    // **Métodos**

    public override void Comer()
    {
        Console.WriteLine("{0} el perro está comiendo.", _nombre);
    }

    public override void Dormir()
    {
        Console.WriteLine("{0} el perro está durmiendo.", _nombre);
    }

    public void Ladrar()
    {
        Console.WriteLine("{0} el perro está ladrando.", _nombre);
    }
}

// **Clase Gato**

public class Gato : Animal
{
    // **Campos**

    private string _colorPelo;

    // **Propiedades**

    public string ColorPelo
    {
        get { return _colorPelo; }
        set { _colorPelo = value; }
    }

    // **Constructores**

    public Gato(string nombre, string tipo, int edad, string colorPelo)
        : base(nombre, tipo, edad)
    {
        _colorPelo = colorPelo;
    }

    // **Métodos**

    public override void Comer()
    {
        Console.WriteLine("{0} el gato está comiendo.", _nombre);
    }

    public override void Dormir()
    {
        Console.WriteLine("{0} el gato está durmiendo.", _nombre);
    }

    public void Maullar()
    {
        Console.WriteLine("{0} el gato está maullando.", _nombre);
    }
}

// **Clase Main**

class Program
{
    static void Main(string[] args)
    {
        // **Crear una instancia de la clase Perro**

        Perro perro = new Perro("Fido", "Perro", 5, "Beagle");

        // **Llamar a los métodos de la clase Perro**

        perro.Comer();
        perro.Dormir();
        perro.Ladrar();

        // **Crear una instancia de la clase Gato**

        Gato gato = new Gato("Misi", "Gato", 3, "Blanco");

        // **Llamar a los métodos de la clase Gato**

        gato.Comer();
        gato.Dormir();
        gato.Maullar();
    }
}
```

**Explicación del código:**

* La clase `Animal` es una clase abstracta que define las propiedades y los métodos comunes a todos los animales.
* La clase `Perro` es una clase derivada de la clase `Animal` que define las propiedades y los métodos específicos de los perros.
* La clase `Gato` es una clase derivada de la clase `Animal` que define las propiedades y los métodos específicos de los gatos.
* La clase `Main` es la clase principal del programa que crea instancias de las clases `Perro` y `Gato` y llama a sus métodos.

Al ejecutar el programa, se creará una instancia de la clase `Perro` llamada "Fido" y una instancia de la clase `Gato` llamada "Misi". A continuación, se llamarán los métodos `Comer()`, `Dormir()` y `Ladrar()` de la clase `Perro`, y los métodos `Comer()`, `Dormir()` y `Maullar()` de la clase `Gato`.

El resultado de la ejecución del programa será el siguiente:

```
Fido el perro está comiendo.
Fido el perro está durmiendo.
Fido el perro está ladrando.
Misi el gato está comiendo.
Misi el gato está durmiendo.
Misi el gato está maullando.
```