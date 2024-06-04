**Classe Complexe**

```csharp
public class CalculateurComplexe
{
    private double re;
    private double im;

    public CalculateurComplexe(double re, double im)
    {
        this.re = re;
        this.im = im;
    }

    public CalculateurComplexe Additionner(CalculateurComplexe autre)
    {
        return new CalculateurComplexe(this.re + autre.re, this.im + autre.im);
    }

    public CalculateurComplexe Soustraire(CalculateurComplexe autre)
    {
        return new CalculateurComplexe(this.re - autre.re, this.im - autre.im);
    }

    public CalculateurComplexe Multiplier(CalculateurComplexe autre)
    {
        double real = this.re * autre.re - this.im * autre.im;
        double imaginary = this.re * autre.im + this.im * autre.re;

        return new CalculateurComplexe(real, imaginary);
    }

    public CalculateurComplexe Diviser(CalculateurComplexe autre)
    {
        double denominator = autre.re * autre.re + autre.im * autre.im;
        if (denominator == 0) throw new DivideByZeroException();

        double real = (this.re * autre.re + this.im * autre.im) / denominator;
        double imaginary = (this.im * autre.re - this.re * autre.im) / denominator;

        return new CalculateurComplexe(real, imaginary);
    }

    public double Module()
    {
        return Math.Sqrt(this.re * this.re + this.im * this.im);
    }

    public double Argument()
    {
        return Math.Atan2(this.im, this.re);
    }

    public override string ToString()
    {
        return $"({this.re}, {this.im})";
    }
}
```

**Utilisation de la Classe**

```csharp
// Créer deux nombres complexes
CalculateurComplexe z1 = new CalculateurComplexe(3, 4);
CalculateurComplexe z2 = new CalculateurComplexe(5, -2);

// Effectuer des opérations sur les nombres complexes
CalculateurComplexe somme = z1.Additionner(z2);
CalculateurComplexe difference = z1.Soustraire(z2);
CalculateurComplexe produit = z1.Multiplier(z2);
CalculateurComplexe quotient = z1.Diviser(z2);

// Afficher les résultats
Console.WriteLine("Somme : " + somme);
Console.WriteLine("Différence : " + difference);
Console.WriteLine("Produit : " + produit);
Console.WriteLine("Quotient : " + quotient);
```

**Explication du Code**

* Les nombres complexes sont représentés par une partie réelle et une partie imaginaire.
* La classe `CalculateurComplexe` fournit des méthodes pour effectuer des opérations arithmétiques et trigonométriques sur des nombres complexes.
* La méthode ToString() permet de représenter les nombres complexes sous forme de chaînes de caractères.
* Le code d'utilisation crée deux nombres complexes, effectue des opérations sur eux et affiche les résultats.