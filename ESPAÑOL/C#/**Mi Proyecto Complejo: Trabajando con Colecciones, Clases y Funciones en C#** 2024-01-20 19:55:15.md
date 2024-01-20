```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MiProyectoComplejo
{
    class Program
    {
        static void Main(string[] args)
        {
            // Definimos una lista de números enteros
            List<int> numeros = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

            // Definimos una lista de cadenas de caracteres
            List<string> nombres = new List<string> { "Juan", "María", "Pedro", "Ana", "Luis", "Sara" };

            // Definimos un diccionario con claves de cadena de caracteres y valores de números enteros
            Dictionary<string, int> edades = new Dictionary<string, int>
            {
                { "Juan", 20 },
                { "María", 25 },
                { "Pedro", 30 },
                { "Ana", 22 },
                { "Luis", 35 },
                { "Sara", 28 }
            };

            // Definimos una clase llamada Persona con propiedades nombre, edad y género
            class Persona
            {
                public string nombre { get; set; }
                public int edad { get; set; }
                public string genero { get; set; }
            }

            // Creamos una lista de objetos de tipo Persona
            List<Persona> personas = new List<Persona>
            {
                new Persona { nombre = "Juan", edad = 20, genero = "Masculino" },
                new Persona { nombre = "María", edad = 25, genero = "Femenino" },
                new Persona { nombre = "Pedro", edad = 30, genero = "Masculino" },
                new Persona { nombre = "Ana", edad = 22, genero = "Femenino" },
                new Persona { nombre = "Luis", edad = 35, genero = "Masculino" },
                new Persona { nombre = "Sara", edad = 28, genero = "Femenino" }
            };

            // Definimos una función que recibe una lista de números enteros y devuelve la suma de los mismos
            int SumaNumeros(List<int> lista)
            {
                int suma = 0;
                foreach (int numero in lista)
                {
                    suma += numero;
                }
                return suma;
            }

            // Definimos una función que recibe una lista de cadenas de caracteres y devuelve la cadena de caracteres más larga
            string CadenaMasLarga(List<string> lista)
            {
                string cadenaMasLarga = "";
                foreach (string cadena in lista)
                {
                    if (cadena.Length > cadenaMasLarga.Length)
                    {
                        cadenaMasLarga = cadena;
                    }
                }
                return cadenaMasLarga;
            }

            // Definimos una función que recibe un diccionario con claves de cadena de caracteres y valores de números enteros y devuelve la clave con el valor más alto
            string ClaveConValorMasAlto(Dictionary<string, int> diccionario)
            {
                string claveConValorMasAlto = "";
                int valorMasAlto = 0;
                foreach (KeyValuePair<string, int> pareja in diccionario)
                {
                    if (pareja.Value > valorMasAlto)
                    {
                        claveConValorMasAlto = pareja.Key;
                        valorMasAlto = pareja.Value;
                    }
                }
                return claveConValorMasAlto;
            }

            // Definimos una función que recibe una lista de objetos de tipo Persona y devuelve la persona con la edad más alta
            Persona PersonaConEdadMasAlta(List<Persona> lista)
            {
                Persona personaConEdadMasAlta = null;
                int edadMasAlta = 0;
                foreach (Persona persona in lista)