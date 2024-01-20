**Ejercicio:** Desarrollar un programa en C# que simule un juego de ajedrez.

```c#
using System;
using System.Collections.Generic;

namespace Ajedrez
{
    class Program
    {
        static void Main(string[] args)
        {
            // Crear el tablero de ajedrez
            Casilla[,] tablero = new Casilla[8, 8];
            for (int i = 0; i < 8; i++)
            {
                for (int j = 0; j < 8; j++)
                {
                    tablero[i, j] = new Casilla(i, j);
                }
            }

            // Crear las piezas de ajedrez
            List<Pieza> piezas = new List<Pieza>();
            piezas.Add(new Rey(0, 4, true));
            piezas.Add(new Reina(0, 3, true));
            piezas.Add(new Alfil(0, 2, true));
            piezas.Add(new Alfil(0, 5, true));
            piezas.Add(new Caballo(0, 1, true));
            piezas.Add(new Caballo(0, 6, true));
            piezas.Add(new Torre(0, 0, true));
            piezas.Add(new Torre(0, 7, true));
            piezas.Add(new Peon(1, 0, true));
            piezas.Add(new Peon(1, 1, true));
            piezas.Add(new Peon(1, 2, true));
            piezas.Add(new Peon(1, 3, true));
            piezas.Add(new Peon(1, 4, true));
            piezas.Add(new Peon(1, 5, true));
            piezas.Add(new Peon(1, 6, true));
            piezas.Add(new Peon(1, 7, true));

            piezas.Add(new Rey(7, 4, false));
            piezas.Add(new Reina(7, 3, false));
            piezas.Add(new Alfil(7, 2, false));
            piezas.Add(new Alfil(7, 5, false));
            piezas.Add(new Caballo(7, 1, false));
            piezas.Add(new Caballo(7, 6, false));
            piezas.Add(new Torre(7, 0, false));
            piezas.Add(new Torre(7, 7, false));
            piezas.Add(new Peon(6, 0, false));
            piezas.Add(new Peon(6, 1, false));
            piezas.Add(new Peon(6, 2, false));
            piezas.Add(new Peon(6, 3, false));
            piezas.Add(new Peon(6, 4, false));
            piezas.Add(new Peon(6, 5, false));
            piezas.Add(new Peon(6, 6, false));
            piezas.Add(new Peon(6, 7, false));

            // Jugar el juego
            while (true)
            {
                // Mostrar el tablero
                Console.Clear();
                for (int i = 0; i < 8; i++)
                {
                    for (int j = 0; j < 8; j++)
                    {
                        Console.Write(tablero[i, j].ToString() + " ");
                    }
                    Console.WriteLine();
                }

                // Obtener el movimiento del jugador
                Console.Write("Ingrese el movimiento (e.g. A2 A4): ");
                string movimiento = Console.ReadLine();

                // Validar el movimiento
                if (!ValidarMovimiento(movimiento, tablero, piezas))
                {
                    Console.WriteLine("Movimiento inválido.");
                    continue;
                }

                // Realizar el movimiento
                RealizarMovimiento(movimiento, tablero, piezas);

                // Comprobar si el juego ha terminado
                if (FinDelJuego(piezas))
                {
                    Console.WriteLine("Juego terminado.");
                    break;
                }

                // Cambiar el turno del jugador
                for (int i = 0; i < piezas.Count; i++)
                {
                    piezas[i].EsBlanco = !piezas[i].EsBlanco;
                }
            }
        }

        // Validar el movimiento
        static bool ValidarMovimiento(string movimiento, Casilla[,] tablero, List<Pieza> piezas)
        {
            // Comprobar si el movimiento está en el formato correcto
            if (movimiento.Length != 4)
            {
                return false;
            }

            // Obtener las coordenadas de la casilla de origen y destino
            int x1 = movimiento[0] - 'A';
            int y1 = movimiento[1] - '1';
            int x2 = movimiento[2] - 'A';
            int y2 = movimiento[3] - '1';

            // Comprobar si las coordenadas de la casilla de origen y destino están dentro del tablero
            if (x1 < 0 || x1 > 7 || y1 < 0 || y1 > 7 || x2 < 0 || x2 > 7 || y2 < 0 || y2 > 7)
            {
                return false;
            }

            // Comprobar si la casilla de origen contiene una pieza
            if (tablero[y1, x1].Pieza == null)
            {
                return false;
            }

            // Comprobar si la casilla de destino está vacía o contiene una pieza enemiga
            if (tablero[y2, x2].Pieza == null || tablero[y2, x2].Pieza.EsBlanco != tablero[y1, x1].Pieza.EsBlanco)
            {
                return true;
            }

            // Comprobar si el movimiento es válido para la pieza
            return tablero[y1, x1].Pieza.EsMovimientoVálido(x2, y2, tablero, piezas);
        }

        // Realizar el movimiento
        static void RealizarMovimiento(string movimiento, Casilla[,] tablero, List<Pieza> piezas)
