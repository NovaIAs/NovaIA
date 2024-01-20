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
            // Creación de una lista de enteros.
            List<int> listaEnteros = new List<int>();
            listaEnteros.Add(1);
            listaEnteros.Add(2);
            listaEnteros.Add(3);
            listaEnteros.Add(4);
            listaEnteros.Add(5);

            // Creación de un diccionario de claves y valores.
            Dictionary<string, string> diccionarioClaveValor = new Dictionary<string, string>();
            diccionarioClaveValor.Add("Clave1", "Valor1");
            diccionarioClaveValor.Add("Clave2", "Valor2");
            diccionarioClaveValor.Add("Clave3", "Valor3");

            // Creación de una clase anónima.
            var claseAnonima = new { Nombre = "Juan", Apellido = "Pérez", Edad = 25 };

            // Creación de una expresión lambda.
            Func<int, int> expresionLambda = (numero) => numero * 2;

            // Creación de un delegado.
            Delegate delegado = new Delegate(MetodoDelegado);

            // Creación de un evento.
            public event EventHandler Evento;

            // Creación de una estructura.
            struct Estructura
            {
                public int Campo1;
                public string Campo2;

                public Estructura(int campo1, string campo2)
                {
                    Campo1 = campo1;
                    Campo2 = campo2;
                }
            }

            // Creación de una interfaz.
            interface IInterfaz
            {
                void Metodo1();
                void Metodo2();
            }

            // Creación de una clase que implementa una interfaz.
            class ClaseImplementacionInterfaz : IInterfaz
            {
                public void Metodo1()
                {
                    // Código del método 1.
                }

                public void Metodo2()
                {
                    // Código del método 2.
                }
            }

            // Creación de una enumeración.
            enum Enumeracion
            {
                Opcion1,
                Opcion2,
                Opcion3
            }

            // Creación de un atributo.
            [AttributeUsage(AttributeTargets.Class | AttributeTargets.Method, AllowMultiple = true)]
            class Atributo : Attribute
            {
                public string Nombre { get; set; }

                public Atributo(string nombre)
                {
                    Nombre = nombre;
                }
            }

            // Creación de una clase con atributos.
            [Atributo("Atributo1")]
            [Atributo("Atributo2")]
            class ClaseConAtributos
            {
                // Código de la clase.
            }

            // Código principal del programa.
            Console.WriteLine("Hola mundo!");

            // Utilización de la lista de enteros.
            foreach (int numero in listaEnteros)
            {
                Console.WriteLine(numero);
            }

            // Utilización del diccionario de claves y valores.
            foreach (KeyValuePair<string, string> parClaveValor in diccionarioClaveValor)
            {
                Console.WriteLine(parClaveValor.Key + ": " + parClaveValor.Value);
            }

            // Utilización de la clase anónima.
            Console.WriteLine(claseAnonima.Nombre + " " + claseAnonima.Apellido + " (" + claseAnonima.Edad + " años)");

            // Utilización de la expresión lambda.
            int resultadoLambda = expresionLambda(10);
            Console.WriteLine("Resultado de la expresión lambda: " + resultadoLambda);

            // Utilización del delegado.
            delegado.Invoke();

            // Utilización del evento.
            Evento += new EventHandler(Evento_Manejador);
            Evento(this, EventArgs.Empty);

            // Utilización de la estructura.
            Estructura estructura = new Estructura(10, "EstructuraEjemplo");
            Console.WriteLine("Campo 1 de la estructura: " + estructura.Campo1);
            Console.WriteLine("Campo 2 de la estructura: " + estructura.Campo2);

            // Utilización de la interfaz.
            IInterfaz interfaz = new ClaseImplementacionInterfaz();
            interfaz.Metodo1();
            interfaz.Metodo2();

            // Utilización de la enumeración.
            Enumeracion opcion = Enumeracion.Opcion2;
            switch (opcion)
            {
                case Enumeracion.Opcion1:
                    // Código para la opción 1.
                    break;
                case Enumeracion.Opcion2:
                    // Código para la opción 2.
                    break;
                case Enumeracion.Opcion3:
                    // Código para la opción 3.
                    break;
            }

            // Utilización de los atributos.
            ClaseConAtributos clase = new ClaseConAtributos();
            foreach (Attribute atributo in Attribute.GetCustomAttributes(clase.GetType()))
            {
                Console.WriteLine("Atributo: " + atributo.Nombre);
            }

            // Fin del programa.
            Console.ReadKey();
        }

        // Método del delegado.
        public static void MetodoDelegado()
        {
            Console.WriteLine("Método delegado invocado.");
        }

        // Manejador del evento.
        public void Evento_Manejador(object sender, EventArgs e)
        {
            Console.WriteLine("Evento manejado.");
        }
    }
}
```

Explicación del código:

1. **Creación de una lista de enteros:** Se crea una lista de enteros llamada `listaEnteros` y se le agregan los valores 1, 2, 3, 4 y 5.
2. **Creación de un diccionario de claves y valores:** Se crea un diccionario de claves y valores llamado `diccionarioClaveValor` y se le agregan los pares de clave-valor: "Clave1" con el valor "Valor1", "Clave2" con el valor "Valor2" y "Clave3" con el valor "Valor3".
3. **Creación de una clase anónima:** Se crea una clase anónima con los campos `Nombre`, `Apellido` y `Edad` y se le asignan los valores "Juan", "Pérez" y 25 respectivamente.
4. **Creación de una expresión lambda:** Se crea una expresión lambda llamada `expresionLambda` que toma un número entero como argumento y devuelve el doble de dicho número.
5. **Creación de un delegado:** Se crea un delegado llamado `delegado` que apunta al método `MetodoDelegado`.
6. **Creación de un evento:** Se crea un evento llamado `Evento`.
7. **Creación de una estructura:** Se crea una estructura llamada `Estructura` con los campos `Campo1` y `Campo2`.
8. **Creación de una interfaz:** Se crea una interfaz llamada `IInterfaz` con los métodos `Metodo1` y `Metodo2`.
9. **Creación de una clase que implementa una interfaz:** Se crea una clase llamada `ClaseImplementacionInterfaz` que implementa la interfaz `IInterfaz`.
10. **Creación de una enumeración:** Se crea una enumeración llamada `Enumeracion` con los valores `Opcion1`, `Opcion2` y `Opcion3`.
11. **Creación de un atributo:** Se crea un atributo llamado `Atributo` que se puede utilizar para decorar clases y métodos.
12. **Creación de una clase con atributos:** Se crea una clase llamada `ClaseConAtributos` que está decorada con el atributo `Atributo`.
13. **Código principal del programa:** El código principal del programa se encuentra en el método `Main`. Este método contiene el código para utilizar las diferentes características del lenguaje C# que se han creado anteriormente.
14. **Utilización de la lista de enteros:** Se utiliza un bucle `foreach` para recorrer la lista de enteros y mostrar cada uno de los elementos en la consola.
15. **Utilización del diccionario de claves y valores:** Se utiliza un bucle `foreach` para recorrer el diccionario de claves y valores y mostrar cada uno de los pares de clave-valor en la consola.
16. **Utilización de la clase anónima:** Se utiliza la clase anónima para mostrar el nombre, el apellido y la edad de una persona en la consola.
17. **Utilización de la expresión lambda:** Se utiliza la expresión lambda para calcular el doble de un número y se muestra el resultado en la consola.
18. **Utilización del delegado:** Se invoca el delegado para ejecutar el método `MetodoDelegado`.
19. **Utilización del evento:** Se agrega un manejador al evento `Evento` y luego se activa el evento.
20. **Utilización de la estructura:** Se crea una instancia de la estructura `Estructura` y se muestran los valores de los campos `Campo1` y `Campo2` en la consola.
21. **Utilización de la interfaz:** Se crea una instancia de la clase `ClaseImplementacionInterfaz` y se llaman a los métodos `Metodo1` y `Metodo2`.
22. **Utilización de la enumeración:** Se crea una instancia de la enumeración `Enumeracion` y se comprueba el valor de la instancia utilizando una instrucción `switch`.
23. **Utilización de los atributos:** Se utilizan los atributos para obtener los nombres de los atributos que están decorando la clase `ClaseConAtributos`.
24. **Fin del programa:** El programa finaliza cuando el usuario pulsa una tecla.