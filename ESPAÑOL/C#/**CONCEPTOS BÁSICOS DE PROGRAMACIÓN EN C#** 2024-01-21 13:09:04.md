```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ComplejoCodigoCSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            // Variables
            int numero1 = 10;
            int numero2 = 20;
            double numero3 = 30.5;
            string cadena = "Hola mundo";
            bool bandera = true;

            // Operaciones aritméticas
            int suma = numero1 + numero2;
            int resta = numero1 - numero2;
            int multiplicacion = numero1 * numero2;
            double division = numero1 / numero3;

            // Operaciones lógicas
            bool and = bandera && true;
            bool or = bandera || false;
            bool not = !bandera;

            // Operaciones relacionales
            bool mayorQue = numero1 > numero2;
            bool menorQue = numero1 < numero2;
            bool igualQue = numero1 == numero2;
            bool diferenteQue = numero1 != numero2;

            // Operaciones de asignación
            numero1 += 5; // Equivalente a numero1 = numero1 + 5
            numero2 -= 5; // Equivalente a numero2 = numero2 - 5
            numero3 *= 2; // Equivalente a numero3 = numero3 * 2
            numero1 /= 2; // Equivalente a numero1 = numero1 / 2

            // Operadores condicionales
            string resultado = numero1 > numero2 ? "Número 1 es mayor que Número 2" : "Número 2 es mayor que Número 1";

            // Operador de coalescencia nula
            string nombre = null;
            string nombreSeguro = nombre ?? "Anónimo"; // Si nombre es nulo, entonces nombreSeguro será "Anónimo", de lo contrario será el valor de nombre

            // Sentencia condicional if-else
            if (numero1 > numero2)
            {
                Console.WriteLine("Número 1 es mayor que Número 2");
            }
            else
            {
                Console.WriteLine("Número 2 es mayor que Número 1");
            }

            // Sentencia condicional switch-case
            switch (numero1)
            {
                case 10:
                    Console.WriteLine("Número 1 es igual a 10");
                    break;
                case 20:
                    Console.WriteLine("Número 1 es igual a 20");
                    break;
                default:
                    Console.WriteLine("Número 1 no es igual a 10 ni a 20");
                    break;
            }

            // Sentencia de iteración for
            for (int i = 0; i < 10; i++)
            {
                Console.WriteLine($"El valor de i es {i}");
            }

            // Sentencia de iteración while
            int contador = 0;
            while (contador < 10)
            {
                Console.WriteLine($"El valor de contador es {contador}");
                contador++;
            }

            // Sentencia de iteración do-while
            contador = 0;
            do
            {
                Console.WriteLine($"El valor de contador es {contador}");
                contador++;
            } while (contador < 10);

            // Sentencia de iteración foreach
            int[] numeros = { 1, 2, 3, 4, 5 };
            foreach (int numero in numeros)
            {
                Console.WriteLine($"El valor de numero es {numero}");
            }

            // Sentencia de ruptura break
            for (int i = 0; i < 10; i++)
            {
                if (i == 5)
                {
                    break;
                }
                Console.WriteLine($"El valor de i es {i}");
            }

            // Sentencia de continuación continue
            for (int i = 0; i < 10; i++)
            {
                if (i % 2 == 0)
                {
                    continue;
                }
                Console.WriteLine($"El valor de i es {i}");
            }

            // Sentencia de retorno return
            int cuadrado(int numero)
            {
                return numero * numero;
            }
            Console.WriteLine($"El cuadrado de 5 es {cuadrado(5)}");

            // Sentencia de lanzamiento de excepción throw
            try
            {
                // Código que puede lanzar una excepción
                int resultadoDivision = numero1 / 0;
            }
            catch (DivideByZeroException)
            {
                // Código que se ejecuta cuando se lanza la excepción
                Console.WriteLine("No se puede dividir por 0");
            }

            // Sentencia de captura de excepción catch
            try
            {
                // Código que puede lanzar una excepción
                int resultadoDivision = numero1 / 0;
            }
            catch (DivideByZeroException ex)
            {
                // Código que se ejecuta cuando se lanza la excepción
                Console.WriteLine($"Error: {ex.Message}");
            }

            // Sentencia finally
            try
            {
                // Código que puede lanzar una excepción
                int resultadoDivision = numero1 / 0;
            }
            catch (DivideByZeroException)
            {
                // Código que se ejecuta cuando se lanza la excepción
                Console.WriteLine("No se puede dividir por 0");
            }
            finally
            {
                // Código que se ejecuta siempre, independientemente de si se lanzó una excepción o no
                Console.WriteLine("Fin del bloque try-catch");
            }

            // Tipos de datos anónimos
            var persona = new { Nombre = "Juan", Apellido = "García", Edad = 30 };
            Console.WriteLine($"Nombre: {persona.Nombre}, Apellido: {persona.Apellido}, Edad: {persona.Edad}");

            // Linq (Language Integrated Query)
            int[] numeros2 = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            var numerosPares = numeros2.Where(numero => numero % 2 == 0);
            Console.WriteLine("Números pares:");
            foreach (int numero in numerosPares)
            {
                Console.WriteLine(numero);
            }

            // Expresiones lambda
            Func<int, int> cuadrado2 = numero => numero * numero;
            Console.WriteLine($"El cuadrado de 5 es {cuadrado2(5)}");

            // Expresiones delegadas
            int suma2(int a, int b)
            {
                return a + b;
            }
            Func<int, int, int> suma3 = suma2;
            Console.WriteLine($"La suma de 5 y 10 es {suma3(5, 10)}");

            // Eventos
            public class Boton
            {
                public event EventHandler Clic;

                public void Presionar()
                {
                    Clic?.Invoke(this, EventArgs.Empty);
                }
            }

            Boton boton = new Boton();
            boton.Clic += (sender, args) => Console.WriteLine("El botón ha sido presionado");
            boton.Presionar();

            // Programación asincrónica
            async Task<int> SumarAsync(int a, int b)
            {
                await Task.Delay(1000); // Simular una operación asincrónica
                return a + b;
            }

            int resultadoSuma = await SumarAsync(5, 10);
            Console.WriteLine($"La suma de 5 y 10 es {resultadoSuma}");

            // Reflection (Reflexión)
            Type tipo = typeof(int);
            Console.WriteLine($"Nombre del tipo: {tipo.Name}");
            Console.WriteLine($"Espacio de nombres del tipo: {tipo.Namespace}");
            Console.WriteLine($"Tipo base del tipo: {tipo.BaseType}");

            // Generics (Genéricos)
            public class Lista<T>
            {
                private T[] elementos;

                public Lista(int tamaño)
                {
                    elementos = new T[tamaño];
                }

                public void Agregar(T elemento)
                {
                    elementos[elementos.Length - 1] = elemento;
                }

                public T Obtener(int índice)
                {
                    return elementos[índice];
                }
            }

            Lista<int> listaNumeros = new Lista<int>(10);
            listaNumeros.Agregar(5);
            listaNumeros.Agregar(10);
            listaNumeros.Agregar(15);
            Console.WriteLine($"El primer elemento de la lista es {lista