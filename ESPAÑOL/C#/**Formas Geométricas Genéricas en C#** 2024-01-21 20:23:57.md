```csharp
// Definición de una clase abstracta de forma genérica
public abstract class FormaGeometrica<T>
{
    public T Lado1 { get; set; }
    public T Lado2 { get; set; }

    public abstract T CalcularArea();
    public abstract T CalcularPerimetro();
}

// Definición de la clase Cuadrado que hereda de FormaGeometrica
public class Cuadrado<T> : FormaGeometrica<T>
{
    public override T CalcularArea()
    {
        return Lado1 * Lado2;
    }

    public override T CalcularPerimetro()
    {
        return 2 * (Lado1 + Lado2);
    }
}

// Definición de la clase Rectángulo que hereda de FormaGeometrica
public class Rectángulo<T> : FormaGeometrica<T>
{
    public override T CalcularArea()
    {
        return Lado1 * Lado2;
    }

    public override T CalcularPerimetro()
    {
        return 2 * (Lado1 + Lado2);
    }
}

// Definición de la clase Círculo que hereda de FormaGeometrica
public class Círculo<T> : FormaGeometrica<T>
{
    public override T CalcularArea()
    {
        return Math.PI * Lado1 * Lado1;
    }

    public override T CalcularPerimetro()
    {
        return 2 * Math.PI * Lado1;
    }
}

// Definición de un método auxiliar para imprimir los resultados
public static void ImprimirResultados<T>(FormaGeometrica<T> forma, string nombreForma)
{
    Console.WriteLine($"Área {nombreForma}: {forma.CalcularArea()}");
    Console.WriteLine($"Perímetro {nombreForma}: {forma.CalcularPerimetro()}");
}

// Bloque principal del programa
class Program
{
    static void Main()
    {
        // Creación de un objeto Cuadrado
        var cuadrado = new Cuadrado<int> { Lado1 = 5, Lado2 = 5 };

        // Creación de un objeto Rectángulo
        var rectángulo = new Rectángulo<int> { Lado1 = 10, Lado2 = 5 };

        // Creación de un objeto Círculo
        var círculo = new Círculo<double> { Lado1 = 5.0 };

        // Impresión de los resultados
        ImprimirResultados(cuadrado, "Cuadrado");
        ImprimirResultados(rectángulo, "Rectángulo");
        ImprimirResultados(círculo, "Círculo");
    }
}
```

Explicación:

1. La clase abstracta `FormaGeometrica` define las propiedades y métodos comunes a todas las formas geométricas. En este caso, las propiedades `Lado1` y `Lado2`, y los métodos `CalcularArea` y `CalcularPerimetro`.

2. Las clases `Cuadrado`, `Rectángulo` y `Círculo` heredan de `FormaGeometrica` y proporcionan implementaciones específicas para cada forma. Por ejemplo, la clase `Cuadrado` calcula el área y el perímetro de un cuadrado, mientras que la clase `Círculo` calcula el área y el perímetro de un círculo.

3. El método `ImprimirResultados` se utiliza para imprimir los resultados obtenidos para cada objeto de forma geométrica.

4. En el bloque principal del programa (`Main`) se crean los objetos `cuadrado`, `rectángulo` y `círculo` y se llaman a los métodos `CalcularArea` y `CalcularPerímetro` para obtener los resultados.

5. Finalmente, se llama al método `ImprimirResultados` para imprimir los resultados para cada objeto.