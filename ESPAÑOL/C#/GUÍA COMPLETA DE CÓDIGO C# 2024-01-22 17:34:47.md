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
            // Definición de variables
            int numero1 = 10;
            int numero2 = 20;
            string nombre = "Juan";
            double precio = 12.50;
            bool esVerdadero = true;

            // Operaciones aritméticas
            int suma = numero1 + numero2;
            int resta = numero2 - numero1;
            int multiplicacion = numero1 * numero2;
            double division = (double)numero2 / numero1; // Conversión explícita a double

            // Operaciones lógicas
            bool resultadoLogico1 = esVerdadero && (numero1 > numero2); // AND
            bool resultadoLogico2 = esVerdadero || (numero1 < numero2); // OR
            bool resultadoLogico3 = !esVerdadero; // NOT

            // Sentencia condicional
            if (esVerdadero)
            {
                Console.WriteLine("La variable esVerdadero es verdadera");
            }
            else
            {
                Console.WriteLine("La variable esVerdadero es falsa");
            }

            // Sentencia switch-case
            switch (nombre)
            {
                case "Juan":
                    Console.WriteLine("El nombre es Juan");
                    break;
                case "María":
                    Console.WriteLine("El nombre es María");
                    break;
                default:
                    Console.WriteLine("El nombre no es ni Juan ni María");
                    break;
            }

            // Bucle for
            for (int i = 0; i < 10; i++)
            {
                Console.WriteLine("Valor de i: {0}", i);
            }

            // Bucle while
            int contador = 0;
            while (contador < 10)
            {
                Console.WriteLine("Valor de contador: {0}", contador);
                contador++;
            }

            // Bucle do-while
            contador = 0;
            do
            {
                Console.WriteLine("Valor de contador: {0}", contador);
                contador++;
            } while (contador < 10);

            // Arrays
            int[] arrayEnteros = new int[5];
            arrayEnteros[0] = 1;
            arrayEnteros[1] = 2;
            arrayEnteros[2] = 3;
            arrayEnteros[3] = 4;
            arrayEnteros[4] = 5;

            // Listas
            List<string> listaNombres = new List<string>();
            listaNombres.Add("Juan");
            listaNombres.Add("María");
            listaNombres.Add("Pedro");

            // Diccionarios
            Dictionary<string, int> diccionarioEdades = new Dictionary<string, int>();
            diccionarioEdades.Add("Juan", 20);
            diccionarioEdades.Add("María", 25);
            diccionarioEdades.Add("Pedro", 30);

            // Funciones
            int resultadoSuma = Suma(numero1, numero2);
            Console.WriteLine("El resultado de la suma es: {0}", resultadoSuma);

            // Procedimientos
            Saluda(nombre);

            // Clases
            Persona persona1 = new Persona("Juan", 20);
            persona1.Saludar();

            // Herencia
            Estudiante estudiante1 = new Estudiante("María", 22, "Ingeniería Informática");
            estudiante1.Saludar();
            estudiante1.Estudiar();

            // Polimorfismo
            List<Persona> personas = new List<Persona>();
            personas.Add(persona1);
            personas.Add(estudiante1);
            foreach (Persona persona in personas)
            {
                persona.Saludar();
            }

            // Excepciones
            try
            {
                int resultadoDivision = numero2 / 0; // División por cero
            }
            catch (DivideByZeroException)
            {
                Console.WriteLine("No se puede dividir por cero");
            }

            // Ficheros
            System.IO.File.WriteAllText("fichero.txt", "Hola mundo");
            string contenidoFichero = System.IO.File.ReadAllText("fichero.txt");
            Console.WriteLine("Contenido del fichero: {0}", contenidoFichero);

            // Hilos
            Thread hilo1 = new Thread(new ThreadStart(Hilo1));
            hilo1.Start();

            // Delegados
            Func<int, int, int> sumaDelegado = (x, y) => x + y;
            int resultadoSumaDelegado = sumaDelegado(numero1, numero2);
            Console.WriteLine("El resultado de la suma mediante delegado es: {0}", resultadoSumaDelegado);

            // Eventos
            Button boton = new Button();
            boton.Click += new EventHandler(Boton_Click); // Asignación de delegado a evento
            boton.PerformClick(); // Activación del evento

            // LINQ
            List<int> listaNumeros = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            var numerosPares = from numero in listaNumeros
                               where numero % 2 == 0
                               select numero;
            foreach (int numeroPar in numerosPares)
            {
                Console.WriteLine("Número par: {0}", numeroPar);
            }

            // Expresiones lambda
            List<int> listaNumerosCuadrados = listaNumeros.Select(x => x * x).ToList();
            foreach (int numeroCuadrado in listaNumerosCuadrados)
            {
                Console.WriteLine("Número cuadrado: {0}", numeroCuadrado);
            }

            // Reflection
            Type tipoPersona = typeof(Persona);
            PropertyInfo propiedadNombre = tipoPersona.GetProperty("Nombre");
            object valorNombre = propiedadNombre.GetValue(persona1);
            Console.WriteLine("El nombre de la