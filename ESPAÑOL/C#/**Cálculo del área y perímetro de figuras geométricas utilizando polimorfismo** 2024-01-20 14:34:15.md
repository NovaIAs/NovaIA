```c#
// Definir una interfaz para los diferentes tipos de figuras geométricas.
public interface IFiguraGeometrica
{
    double CalcularArea();
    double CalcularPerimetro();
}

// Definir una clase para representar un cuadrado.
public class Cuadrado : IFiguraGeometrica
{
    private double lado;

    public Cuadrado(double lado)
    {
        this.lado = lado;
    }

    public double CalcularArea()
    {
        return lado * lado;
    }

    public double CalcularPerimetro()
    {
        return 4 * lado;
    }
}

// Definir una clase para representar un círculo.
public class Circulo : IFiguraGeometrica
{
    private double radio;

    public Circulo(double radio)
    {
        this.radio = radio;
    }

    public double CalcularArea()
    {
        return Math.PI * radio * radio;
    }

    public double CalcularPerimetro()
    {
        return 2 * Math.PI * radio;
    }
}

// Definir una clase para representar un triángulo.
public class Triangulo : IFiguraGeometrica
{
    private double lado1;
    private double lado2;
    private double lado3;

    public Triangulo(double lado1, double lado2, double lado3)
    {
        this.lado1 = lado1;
        this.lado2 = lado2;
        this.lado3 = lado3;
    }

    public double CalcularArea()
    {
        double semiperimetro = (lado1 + lado2 + lado3) / 2;
        return Math.Sqrt(semiperimetro * (semiperimetro - lado1) * (semiperimetro - lado2) * (semiperimetro - lado3));
    }

    public double CalcularPerimetro()
    {
        return lado1 + lado2 + lado3;
    }
}

// Definir una clase para representar un rectángulo.
public class Rectangulo : IFiguraGeometrica
{
    private double ancho;
    private double altura;

    public Rectangulo(double ancho, double altura)
    {
        this.ancho = ancho;
        this.altura = altura;
    }

    public double CalcularArea()
    {
        return ancho * altura;
    }

    public double CalcularPerimetro()
    {
        return 2 * (ancho + altura);
    }
}

// Definir una clase para representar un trapecio.
public class Trapecio : IFiguraGeometrica
{
    private double baseMayor;
    private double baseMenor;
    private double altura;

    public Trapecio(double baseMayor, double baseMenor, double altura)
    {
        this.baseMayor = baseMayor;
        this.baseMenor = baseMenor;
        this.altura = altura;
    }

    public double CalcularArea()
    {
        return ((baseMayor + baseMenor) / 2) * altura;
    }

    public double CalcularPerimetro()
    {
        double lado1 = Math.Sqrt(Math.Pow(baseMayor - baseMenor, 2) + Math.Pow(altura, 2));
        double lado2 = Math.Sqrt(Math.Pow(baseMayor, 2) + Math.Pow(altura, 2));
        double lado3 = Math.Sqrt(Math.Pow(baseMenor, 2) + Math.Pow(altura, 2));
        return baseMayor + baseMenor + lado1 + lado2 + lado3;
    }
}

// Definir una clase para representar un rombo.
public class Rombo : IFiguraGeometrica
{
    private double lado;
    private double diagonalMayor;
    private double diagonalMenor;

    public Rombo(double lado, double diagonalMayor, double diagonalMenor)
    {
        this.lado = lado;
        this.diagonalMayor = diagonalMayor;
        this.diagonalMenor = diagonalMenor;
    }

    public double CalcularArea()
    {
        return (diagonalMayor * diagonalMenor) / 2;
    }

    public double CalcularPerimetro()
    {
        return 4 * lado;
    }
}

// Definir una clase para representar un romboide.
public class Romboide : IFiguraGeometrica
{
    private double lado1;
    private double lado2;
    private double altura;

    public Romboide(double lado1, double lado2, double altura)
    {
        this.lado1 = lado1;
        this.lado2 = lado2;
        this.altura = altura;
    }

    public double CalcularArea()
    {
        return lado1 * altura;
    }

    public double CalcularPerimetro()
    {
        return 2 * (lado1 + lado2);
    }
}

// Definir una clase para representar un hexágono.
public class Hexagono : IFiguraGeometrica
{
    private double lado;

    public Hexagono(double lado)
    {
        this.lado = lado;
    }

    public double CalcularArea()
    {
        return (3 * Math.Sqrt(3) / 2) * Math.Pow(lado, 2);
    }

    public double CalcularPerimetro()
    {
        return 6 * lado;
    }
}

// Definir una clase para representar un octágono.
public class Octagono : IFiguraGeometrica
{
    private double lado;

    public Octagono(double lado)
    {
        this.lado = lado;
    }

    public double CalcularArea()
    {
        return 2 * (1 + Math.Sqrt(2)) * lado * lado;
    }

    public double CalcularPerimetro()
    {
        return 8 * lado;
    }
}

// Definir una clase para representar un decágono.
public class Decagono : IFiguraGeometrica
{
    private double lado;

    public Decagono(double lado)
    {
        this.lado = lado;
    }

    public double CalcularArea()
    {
        return (5 * Math.Sqrt(5 + 2 * Math.Sqrt(5)) / 4) * lado * lado;
    }

    public double CalcularPerimetro()
    {
        return 10 * lado;
    }
}

// Definir una clase para representar un dodecágono.
public class Dodecagono : IFiguraGeometrica