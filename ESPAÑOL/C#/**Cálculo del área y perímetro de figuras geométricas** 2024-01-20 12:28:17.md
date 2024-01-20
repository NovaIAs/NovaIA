```c#
// Importar las bibliotecas necesarias
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// Definir una clase abstracta para representar figuras geométricas
public abstract class FiguraGeometrica
{
    // Propiedades comunes a todas las figuras geométricas
    public string Nombre { get; set; }
    public double Area { get; set; }
    public double Perimetro { get; set; }

    // Constructor para inicializar las propiedades comunes
    public FiguraGeometrica(string nombre)
    {
        Nombre = nombre;
    }

    // Método abstracto para calcular el área de la figura geométrica
    public abstract double CalcularArea();

    // Método abstracto para calcular el perímetro de la figura geométrica
    public abstract double CalcularPerimetro();
}

// Definir una clase que represente un cuadrado
public class Cuadrado : FiguraGeometrica
{
    // Propiedad para almacenar la longitud del lado del cuadrado
    public double Lado { get; set; }

    // Constructor para inicializar las propiedades del cuadrado
    public Cuadrado(string nombre, double lado) : base(nombre)
    {
        Lado = lado;
    }

    // Método para calcular el área del cuadrado
    public override double CalcularArea()
    {
        Area = Lado * Lado;
        return Area;
    }

    // Método para calcular el perímetro del cuadrado
    public override double CalcularPerimetro()
    {
        Perimetro = 4 * Lado;
        return Perimetro;
    }
}

// Definir una clase que represente un círculo
public class Circulo : FiguraGeometrica
{
    // Propiedad para almacenar el radio del círculo
    public double Radio { get; set; }

    // Constructor para inicializar las propiedades del círculo
    public Circulo(string nombre, double radio) : base(nombre)
    {
        Radio = radio;
    }

    // Método para calcular el área del círculo
    public override double CalcularArea()
    {
        Area = Math.PI * Radio * Radio;
        return Area;
    }

    // Método para calcular el perímetro del círculo
    public override double CalcularPerimetro()
    {
        Perimetro = 2 * Math.PI * Radio;
        return Perimetro;
    }
}

// Definir una clase que represente un triángulo
public class Triangulo : FiguraGeometrica
{
    // Propiedades para almacenar la longitud de los lados del triángulo
    public double LadoA { get; set; }
    public double LadoB { get; set; }
    public double LadoC { get; set; }

    // Constructor para inicializar las propiedades del triángulo
    public Triangulo(string nombre, double ladoA, double ladoB, double ladoC) : base(nombre)
    {
        LadoA = ladoA;
        LadoB = ladoB;
        LadoC = ladoC;
    }

    // Método para calcular el área del triángulo usando la fórmula de Heron
    public override double CalcularArea()
    {
        double semiperimetro = (LadoA + LadoB + LadoC) / 2;
        Area = Math.Sqrt(semiperimetro * (semiperimetro - LadoA) * (semiperimetro - LadoB) * (semiperimetro - LadoC));
        return Area;
    }

    // Método para calcular el perímetro del triángulo
    public override double CalcularPerimetro()
    {
        Perimetro = LadoA + LadoB + LadoC;
        return Perimetro;
    }
}

// Definir una clase que represente una lista de figuras geométricas
public class ListaFigurasGeometricas
{
    // Propiedad para almacenar la lista de figuras geométricas
    public List<FiguraGeometrica> Figuras { get; set; }

    // Constructor para inicializar la lista de figuras geométricas
    public ListaFigurasGeometricas()
    {
        Figuras = new List<FiguraGeometrica>();
    }

    // Método para agregar una figura geométrica a la lista
    public void AgregarFigura(FiguraGeometrica figura)
    {
        Figuras.Add(figura);
    }

    // Método para calcular el área total de la lista de figuras geométricas
    public double CalcularAreaTotal()
    {
        double areaTotal = 0;
        foreach (FiguraGeometrica figura in Figuras)
        {
            areaTotal += figura.CalcularArea();
        }
        return areaTotal;
    }

    // Método para calcular el perímetro total de la lista de figuras geométricas
    public double CalcularPerimetroTotal()
    {
        double perimetroTotal = 0;
        foreach (FiguraGeometrica figura in Figuras)
        {
            perimetroTotal += figura.CalcularPerimetro();
        }
        return perimetroTotal;
    }
}

// Definir una clase principal para probar la funcionalidad del código
public class Program
{
    public static void Main(string[] args)
    {
        // Crear una lista de figuras geométricas
        ListaFigurasGeometricas listaFiguras = new ListaFigurasGeometricas();

        // Agregar un cuadrado a la lista
        listaFiguras.AgregarFigura(new Cuadrado("Cuadrado 1", 5));

        // Agregar un círculo a la lista
        listaFiguras.AgregarFigura(new Circulo("Círculo 1", 3));

        // Agregar un triángulo a la lista
        listaFiguras.AgregarFigura(new Triangulo("Triángulo 1", 4, 5, 6));

        // Calcular el área total de la lista de figuras geométricas
        double areaTotal = listaFiguras.CalcularAreaTotal();

        // Calcular el perímetro total de la lista de figuras geométricas
        double perimetroTotal = listaFiguras.CalcularPerimetroTotal();

        // Mostrar el área total y el perímetro total de la lista de figuras geométricas
        Console.WriteLine("Área total: {0}", areaTotal);
        Console.WriteLine("Perímetro total: {0}", perimetroTotal);
    }
}
```

**Explicación:**

1. Se define una clase abstracta `FiguraGeometrica` que representa una figura geométrica genérica. Esta clase tiene propiedades comunes a todas las figuras geométricas, como el nombre, el área y el perímetro. También tiene métodos abstractos para calcular el área y el perímetro de la figura geométrica.

2. Se definen tres clases concretas que heredan de la clase `FiguraGeometrica`: `Cuadrado`, `Circulo` y `Triangulo`. Cada clase concreta tiene propiedades específicas de su figura geométrica correspondiente. Por ejemplo, la clase `Cuadrado` tiene una propiedad `Lado` para almacenar la longitud del lado del cuadrado.

3. Se define una clase `ListaFigurasGeometricas` que representa una lista de figuras geométricas. Esta clase tiene una propiedad `Figuras` que almacena la lista de figuras geométricas. También tiene métodos para agregar una figura geométrica a la lista, calcular el área total de la lista de figuras geométricas y calcular el perímetro total de la lista de figuras geométricas.

4. Se define una clase `Program` que es la clase principal del programa. Esta clase tiene un método `Main` que es el punto de entrada del programa. En este método se crea una instancia de la clase `ListaFigurasGeometricas` y se agregan tres figuras geométricas a la lista: un cuadrado, un círculo y un triángulo. Luego, se calculan el área total y el perímetro total de la lista de figuras geométricas y se muestran en la consola.

El código anterior es un ejemplo de cómo se pueden crear clases abstractas y clases concretas en C#. También muestra cómo se pueden usar estas clases para crear objetos y manipularlos.