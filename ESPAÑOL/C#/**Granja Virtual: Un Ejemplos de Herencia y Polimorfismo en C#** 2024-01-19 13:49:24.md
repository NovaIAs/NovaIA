```c#
// Definir una clase abstracta "Animal" como base para todas las clases de animales.
public abstract class Animal
{
    public string Nombre { get; set; }
    public int Edad { get; set; }
    public string Raza { get; set; }

    public abstract void HacerRuido();
}

// Definir una clase "Perro" que hereda de la clase "Animal".
public class Perro : Animal
{
    public string RazaPerro { get; set; }

    public override void HacerRuido()
    {
        Console.WriteLine("¡Guau!");
    }
}

// Definir una clase "Gato" que hereda de la clase "Animal".
public class Gato : Animal
{
    public string RazaGato { get; set; }

    public override void HacerRuido()
    {
        Console.WriteLine("¡Miau!");
    }
}

// Definir una clase "Caballo" que hereda de la clase "Animal".
public class Caballo : Animal
{
    public string RazaCaballo { get; set; }

    public override void HacerRuido()
    {
        Console.WriteLine("¡Relincho!");
    }
}

// Definir una clase "Granja" que contiene una lista de animales.
public class Granja
{
    public List<Animal> Animales { get; set; }

    public void AgregarAnimal(Animal animal)
    {
        Animales.Add(animal);
    }

    public void HacerRuidoTodos()
    {
        foreach (Animal animal in Animales)
        {
            animal.HacerRuido();
        }
    }
}

// Crear una granja y agregar algunos animales.
Granja miGranja = new Granja();
Perro miPerro = new Perro { Nombre = "Firulais", Edad = 5, Raza = "Pastor Alemán", RazaPerro = "Pastor Alemán" };
Gato miGato = new Gato { Nombre = "Michi", Edad = 3, Raza = "Siamés", RazaGato = "Siamés" };
Caballo miCaballo = new Caballo { Nombre = "Relámpago", Edad = 10, Raza = "Pura Sangre", RazaCaballo = "Pura Sangre" };
miGranja.AgregarAnimal(miPerro);
miGranja.AgregarAnimal(miGato);
miGranja.AgregarAnimal(miCaballo);

// Hacer que todos los animales de la granja hagan ruido.
miGranja.HacerRuidoTodos();
```

Explicación del código:

1. Definimos una clase abstracta "Animal" que contiene propiedades comunes a todos los animales, como su nombre, edad y raza. También definimos un método abstracto "HacerRuido()" que se implementará en las clases derivadas.

2. Definimos tres clases concretas que heredan de la clase "Animal": "Perro", "Gato" y "Caballo". Cada clase tiene propiedades adicionales específicas para ese tipo de animal, como la raza del perro o del gato.

3. En las clases "Perro", "Gato" y "Caballo", implementamos el método "HacerRuido()" para que cada animal haga un ruido diferente.

4. Definimos una clase "Granja" que contiene una lista de animales. Esta clase tiene un método "AgregarAnimal()" para agregar animales a la lista y un método "HacerRuidoTodos()" para hacer que todos los animales en la lista hagan ruido.

5. Creamos una instancia de la clase "Granja" y agregamos los animales "Firulais", "Michi" y "Relámpago" a la lista.

6. Llamamos al método "HacerRuidoTodos()" para hacer que todos los animales en la granja hagan ruido.

El código anterior crea una granja virtual con tres animales diferentes y hace que cada animal haga un ruido. Este es un ejemplo de cómo se puede usar la herencia y el polimorfismo en C# para crear clases y objetos con diferentes comportamientos.