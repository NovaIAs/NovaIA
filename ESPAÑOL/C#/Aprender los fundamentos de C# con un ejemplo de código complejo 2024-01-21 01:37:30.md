```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodigoComplejo
{
    class Programa
    {
        static void Main(string[] args)
        {
            // Declaración de variables
            int numero1 = 10;
            int numero2 = 20;
            int resultado;

            // Operaciones matemáticas
            resultado = numero1 + numero2;
            Console.WriteLine("La suma de {0} y {1} es {2}", numero1, numero2, resultado);

            resultado = numero1 - numero2;
            Console.WriteLine("La resta de {0} y {1} es {2}", numero1, numero2, resultado);

            resultado = numero1 * numero2;
            Console.WriteLine("La multiplicación de {0} y {1} es {2}", numero1, numero2, resultado);

            resultado = numero1 / numero2;
            Console.WriteLine("La división de {0} y {1} es {2}", numero1, numero2, resultado);

            // Operaciones lógicas
            bool condicion1 = true;
            bool condicion2 = false;

            bool resultadoLogico = condicion1 && condicion2;
            Console.WriteLine("El resultado lógico de {0} y {1} es {2}", condicion1, condicion2, resultadoLogico);

            resultadoLogico = condicion1 || condicion2;
            Console.WriteLine("El resultado lógico de {0} y {1} es {2}", condicion1, condicion2, resultadoLogico);

            resultadoLogico = !condicion1;
            Console.WriteLine("El resultado lógico de {0} es {1}", condicion1, resultadoLogico);

            // Operaciones de control de flujo
            if (condicion1)
            {
                Console.WriteLine("La condición {0} es verdadera", condicion1);
            }
            else
            {
                Console.WriteLine("La condición {0} es falsa", condicion1);
            }

            switch (numero1)
            {
                case 10:
                    Console.WriteLine("El número {0} es igual a 10", numero1);
                    break;
                case 20:
                    Console.WriteLine("El número {0} es igual a 20", numero1);
                    break;
                default:
                    Console.WriteLine("El número {0} no es igual a 10 ni a 20", numero1);
                    break;
            }

            // Operaciones de iteración
            for (int i = 0; i < 10; i++)
            {
                Console.WriteLine("El valor de i es {0}", i);
            }

            int[] array = new int[] { 1, 2, 3, 4, 5 };

            foreach (int elemento in array)
            {
                Console.WriteLine("El elemento actual del array es {0}", elemento);
            }

            // Operaciones de manejo de excepciones
            try
            {
                int resultadoDivision = numero1 / 0;
            }
            catch (DivideByZeroException ex)
            {
                Console.WriteLine("Se ha producido una excepción: {0}", ex.Message);
            }

            // Operaciones de entrada y salida
            Console.WriteLine("Introduce un número:");
            string input = Console.ReadLine();

            int numeroIntroducido = int.Parse(input);

            Console.WriteLine("El número introducido es {0}", numeroIntroducido);

            // Operaciones de manejo de archivos
            string rutaArchivo = @"C:\ruta\archivo.txt";

            using (StreamWriter escritor = new StreamWriter(rutaArchivo))
            {
                escritor.WriteLine("Hola mundo!");
            }

            using (StreamReader lector = new StreamReader(rutaArchivo))
            {
                string contenido = lector.ReadToEnd();

                Console.WriteLine("El contenido del archivo es: {0}", contenido);
            }

            // Operaciones de manejo de hilos
            Task tarea = new Task(() =>
            {
                Console.WriteLine("Soy una tarea asíncrona");
            });

            tarea.Start();

            tarea.Wait();

            // Operaciones de manejo de eventos
            Button boton = new Button();

            boton.Click += (sender, args) =>
            {
                Console.WriteLine("Se ha hecho clic en el botón");
            };

            boton.PerformClick();

            // Operaciones de manejo de bases de datos
            using (SqlConnection conexion = new SqlConnection("connectionString"))
            {
                conexion.Open();

                using (SqlCommand comando = new SqlCommand("consulta", conexion))
                {
                    SqlDataReader lector = comando.ExecuteReader();

                    while (lector.Read())
                    {
                        Console.WriteLine("Columna 1: {0}, Columna 2: {1}", lector[0], lector[1]);
                    }
                }
            }

            // Operaciones de manejo de XML
            XDocument documento = XDocument.Load("rutaArchivo.xml");

            var elementos = documento.Descendants("elemento");

            foreach (XElement elemento in elementos)
            {
                Console.WriteLine("Nombre del elemento: {0}, Valor del elemento: {1}", elemento.Name, elemento.Value);
            }

            // Operaciones de manejo de JSON
            string json = @"{""nombre"": ""Juan"", ""apellido"": ""Pérez""}";

            JObject objetoJson = JObject.Parse(json);

            Console.WriteLine("Nombre: {0}, Apellido: {1}", objetoJson["nombre"], objetoJson["apellido"]);

            // Operaciones de manejo de colecciones
            List<string> lista = new