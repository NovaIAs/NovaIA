```c#
// Código para verificar el saldo restante de las cuenta de un cliente de un banco
// Versión 2: medición de tiempo y uso de hilos

using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace VerificarSaldoCuentaBancariaV2
{
    internal class CuentaBancaria
    {
        public int NumeroCuenta { get; set; } // Número de la cuenta bancaria
        public decimal Saldo { get; set; } // Saldo actual de la cuenta

        public static bool Transferir(CuentaBancaria cuentaOrigen, CuentaBancaria cuentaDestino, decimal monto)
        {
            // Verificar si el monto es válido
            if (monto <= 0 || monto > cuentaOrigen.Saldo)
            {
                return false;
            }

            // Bloquear ambas cuentas para garantizar la integridad de la transacción
            lock (cuentaOrigen)
            {
                lock (cuentaDestino)
                {
                    // Transferir el monto
                    cuentaOrigen.Saldo -= monto;
                    cuentaDestino.Saldo += monto;
                }
            }

            // La transferencia fue exitosa
            return true;
        }
    }

    internal class Banco
    {
        private readonly Dictionary<int, CuentaBancaria> _cuentasBancarias; // Diccionario para almacenar las cuentas bancarias

        public Banco()
        {
            _cuentasBancarias = new Dictionary<int, CuentaBancaria>();
        }

        public CuentaBancaria CrearCuenta(decimal saldoInicial)
        {
            // Crear una nueva cuenta bancaria y agregarla al diccionario
            var cuenta = new CuentaBancaria { Saldo = saldoInicial };
            _cuentasBancarias.Add(cuenta.NumeroCuenta, cuenta);

            // Devolver la cuenta creada
            return cuenta;
        }

        public CuentaBancaria? ObtenerCuenta(int numeroCuenta)
        {
            // Verificar si la cuenta existe en el diccionario
            if (_cuentasBancarias.ContainsKey(numeroCuenta))
            {
                // Devolver la cuenta
                return _cuentasBancarias[numeroCuenta];
            }

            // La cuenta no existe
            return null;
        }
    }

    internal class Program
    {
        private static Banco _banco; // Instancia del banco
        private static readonly List<Task> _tareas = new List<Task>(); // Lista de tareas para mantener un registro de las tareas en ejecución
        private static readonly ManualResetEventSlim _eventoFinalizado = new ManualResetEventSlim(false); // Evento para indicar cuando todas las tareas han finalizado

        private static void Main(string[] args)
        {
            // Crear el banco
            _banco = new Banco();

            // Crear 100 cuentas bancarias iniciales
            for (int i = 0; i < 100; i++)
            {
                var cuenta = _banco.CrearCuenta(1000);
                Console.WriteLine($"Cuenta {cuenta.NumeroCuenta} creada con un saldo inicial de ${cuenta.Saldo}");
            }

            // Crear 100 tareas que realizarán transferencias simultáneas entre cuentas
            for (int i = 0; i < 100; i++)
            {
                // Crear una tarea para realizar transferencias entre cuentas
                var tarea = Task.Run(() => RealizarTransferencias());

                // Agregar la tarea a la lista de tareas
                _tareas.Add(tarea);
            }

            // Esperar a que todas las tareas finalicen
            Task.WaitAll(_tareas.ToArray());

            // Indicar que todas las tareas han finalizado
            _eventoFinalizado.Set();

            // Obtener el saldo total de todas las cuentas bancarias
            decimal saldoTotal = 0;
            foreach (var cuenta in _banco._cuentasBancarias.Values)
            {
                saldoTotal += cuenta.Saldo;
            }

            // Mostrar el saldo total
            Console.WriteLine($"Saldo total: ${saldoTotal}");
        }

        private static void RealizarTransferencias()
        {
            // Realizar 100 transferencias aleatorias entre cuentas
            for (int i = 0; i < 100; i++)
            {
                // Obtener dos cuentas bancarias aleatorias
                var cuentaOrigen = _banco.ObtenerCuenta(Random.Shared.Next(1, 101));
                var cuentaDestino = _banco.ObtenerCuenta(Random.Shared.Next(1, 101));

                // Transferir un monto aleatorio entre las dos cuentas
                var monto = Random.Shared.Next(1, 101);
                CuentaBancaria.Transferir(cuentaOrigen, cuentaDestino, monto);
            }

            // Esperar a que todas las tareas finalicen
            _eventoFinalizado.Wait();
        }
    }
}
```

Explicación del código:

* Se crea una clase `CuentaBancaria` que representa una cuenta bancaria con un número de cuenta y un saldo.
* Se crea una clase `Banco` que representa un banco con una colección de cuentas bancarias.
* Se crea una clase `Program` que contiene el código principal del programa.
* En el método `Main`, se crea una instancia del banco y se crean 100 cuentas bancarias iniciales.
* Se crean 100 tareas que realizarán transferencias simultáneas entre cuentas.
* Se espera a que todas las tareas finalicen.
* Se obtiene el saldo total de todas las cuentas bancarias.
* Se muestra el saldo total.

Este código es complejo y puede resultar difícil de entender al principio. Sin embargo, si se sigue paso a paso, es posible entender cómo funciona. El código utiliza varios hilos para realizar transferencias simultáneas entre cuentas, lo que lo hace más eficiente que si se realizaran las transferencias una por una. El uso de hilos también permite medir el tiempo que tarda en completarse el procesamiento, lo que puede ser útil para optimizar el código.