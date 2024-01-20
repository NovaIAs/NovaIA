```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodigoComplejo
{
    class Program
    {
        static void Main(string[] args)
        {
            // Crear una lista de números enteros
            List<int> numeros = new List<int>();
            numeros.Add(1);
            numeros.Add(2);
            numeros.Add(3);
            numeros.Add(4);
            numeros.Add(5);

            // Crear una lista de cadenas de texto
            List<string> cadenas = new List<string>();
            cadenas.Add("Hola");
            cadenas.Add("Mundo");
            cadenas.Add("!");

            // Crear un diccionario de clave-valor
            Dictionary<string, int> diccionario = new Dictionary<string, int>();
            diccionario.Add("Uno", 1);
            diccionario.Add("Dos", 2);
            diccionario.Add("Tres", 3);

            // Iterar sobre la lista de números enteros y mostrarlos por consola
            Console.WriteLine("Lista de números enteros:");
            foreach (int numero in numeros)
            {
                Console.WriteLine(numero);
            }

            // Iterar sobre la lista de cadenas de texto y mostrarlas por consola
            Console.WriteLine("Lista de cadenas de texto:");
            foreach (string cadena in cadenas)
            {
                Console.WriteLine(cadena);
            }

            // Iterar sobre el diccionario de clave-valor y mostrar las claves y valores por consola
            Console.WriteLine("Diccionario de clave-valor:");
            foreach (KeyValuePair<string, int> parClaveValor in diccionario)
            {
                Console.WriteLine("{0}: {1}", parClaveValor.Key, parClaveValor.Value);
            }

            // Crear una función que reciba un número entero y devuelva su factorial
            Func<int, int> factorial = n =>
            {
                if (n == 0)
                {
                    return 1;
                }
                else
                {
                    return n * factorial(n - 1);
                }
            };

            // Calcular el factorial de 5 y mostrarlo por consola
            int factorialDe5 = factorial(5);
            Console.WriteLine("Factorial de 5: {0}", factorialDe5);

            // Crear una clase que represente un punto en el espacio 3D
            class Punto3D
            {
                public double x;
                public double y;
                public double z;

                public Punto3D(double x, double y, double z)
                {
                    this.x = x;
                    this.y = y;
                    this.z = z;
                }

                public double DistanciaAlOrigen()
                {
                    return Math.Sqrt(x * x + y * y + z * z);
                }
            }

            // Crear un punto 3D y mostrar su distancia al origen por consola
            Punto3D punto3D = new Punto3D(1, 2, 3);
            double distanciaAlOrigen = punto3D.DistanciaAlOrigen();
            Console.WriteLine("Distancia del punto 3D al origen: {0}", distanciaAlOrigen);

            // Crear una interfaz que represente un animal
            interface IAnimal
            {
                void Comer();
                void Dormir();
            }

            // Crear una clase que represente un perro
            class Perro : IAnimal
            {
                public void Comer()
                {
                    Console.WriteLine("El perro está comiendo");
                }

                public void Dormir()
                {
                    Console.WriteLine("El perro está durmiendo");
                }
            }

            // Crear una clase que represente un gato
            class Gato : IAnimal
            {
                public void Comer()
                {
                    Console.WriteLine("El gato está comiendo");
                }

                public void Dormir()
                {
                    Console.WriteLine("El gato está durmiendo");
                }
            }

            // Crear un perro y un gato y hacer que coman y duerman
            Perro perro = new Perro();
            perro.Comer();
            perro.Dormir();

            Gato gato = new Gato();
            gato.Comer();
            gato.Dormir();

            // Crear un evento que se dispare cuando un animal coma
            event EventHandler AnimalCome;

            // Crear un perro que dispare el evento AnimalCome cuando coma
            perro = new Perro();
            perro.AnimalCome += (sender, args) =>
            {
                Console.WriteLine("El perro ha comido");
            };

            // Hacer que el perro coma y mostrar un mensaje por consola cuando lo haga
            perro.Comer();

            // Crear una cola de mensajes
            Queue<string> colaMensajes = new Queue<string>();

            // Añadir mensajes a la cola
            colaMensajes.Enqueue("Hola");
            colaMensajes.Enqueue("Mundo");
            colaMensajes.Enqueue("!");

            // Quitar los mensajes de la cola y mostrarlos por consola
            while (colaMensajes.Count > 0)
            {
                string mensaje = colaMensajes.Dequeue();
                Console.WriteLine(mensaje);
            }

            // Crear una pila de mensajes
            Stack<string> pilaMensajes = new Stack<string