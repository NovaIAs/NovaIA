```c#
// Definición de clases

// Clase base para representar una figura geométrica
public abstract class Figura
{
    public string Nombre { get; set; }
    public double Area { get; protected set; }

    public Figura(string nombre)
    {
        Nombre = nombre;
    }

    public abstract double CalcularArea();
}

// Clase que representa un cuadrado
public class Cuadrado : Figura
{
    public double Lado { get; set; }

    public Cuadrado(string nombre, double lado) : base(nombre)
    {
        Lado = lado;
    }

    public override double CalcularArea()
    {
        Area = Lado * Lado;
        return Area;
    }
}

// Clase que representa un círculo
public class Circulo : Figura
{
    public double Radio { get; set; }

    public Circulo(string nombre, double radio) : base(nombre)
    {
        Radio = radio;
    }

    public override double CalcularArea()
    {
        Area = Math.PI * Radio * Radio;
        return Area;
    }
}

// Clase que representa un triángulo
public class Triangulo : Figura
{
    public double Base { get; set; }
    public double Altura { get; set; }

    public Triangulo(string nombre, double @base, double altura) : base(nombre)
    {
        Base = @base;
        Altura = altura;
    }

    public override double CalcularArea()
    {
        Area = 0.5 * Base * Altura;
        return Area;
    }
}

// Definición de métodos

// Método para imprimir el área de una figura geométrica
public static void ImprimirArea(Figura figura)
{
    Console.WriteLine($"El área de la figura {figura.Nombre} es {figura.Area} unidades cuadradas.");
}

// Método principal
public static void Main(string[] args)
{
    // Creación de objetos de diferentes figuras geométricas
    Figura cuadrado = new Cuadrado("Cuadrado", 5);
    Figura circulo = new Circulo("Círculo", 3);
    Figura triangulo = new Triangulo("Triángulo", 4, 6);

    // Cálculo del área de cada figura geométrica
    cuadrado.CalcularArea();
    circulo.CalcularArea();
    triangulo.CalcularArea();

    // Impresión del área de cada figura geométrica
    ImprimirArea(cuadrado);
    ImprimirArea(circulo);
    ImprimirArea(triangulo);
}
```

Explicación:

1. **Definición de Clases (líneas 5-27):**

    * La clase `Figura` es una clase base abstracta que representa una figura geométrica general. Contiene una propiedad `Nombre` y de la propiedad `Area` calculada por el método `CalcularArea()`.
    * La clase `Cuadrado` hereda de la clase `Figura` y representa un cuadrado. Tiene una propiedad `Lado` que define la longitud del lado del cuadrado.
    * La clase `Círculo` hereda de la clase `Figura` y representa un círculo. Tiene una propiedad `Radio` que define el radio del círculo.
    * La clase `Triángulo` hereda de la clase `Figura` y representa un triángulo. Tiene propiedades `Base` y `Altura` que definen la base y la altura del triángulo.

2. **Definición de Métodos (líneas 29-35):**

    * El método `ImprimirArea()` imprime el área de una figura geométrica.
    * El método `Main()` es el método principal que crea objetos de diferentes figuras geométricas, calcula sus áreas y luego las imprime.

3. **Uso de las Clases y Métodos (líneas 38-56):**

    * Se crean objetos de las clases `Cuadrado`, `Círculo` y `Triángulo`.
    * Se llama al método `CalcularArea()` para cada objeto con el fin de calcular sus áreas.
    * Se llama al método `ImprimirArea()` para cada objeto con el fin de imprimir sus áreas.