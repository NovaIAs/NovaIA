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
            // Definir una clase para representar un punto en un espacio tridimensional
            class Punto3D
            {
                public double X { get; set; }
                public double Y { get; set; }
                public double Z { get; set; }

                public Punto3D(double x, double y, double z)
                {
                    X = x;
                    Y = y;
                    Z = z;
                }

                public double Distancia(Punto3D otroPunto)
                {
                    double dx = X - otroPunto.X;
                    double dy = Y - otroPunto.Y;
                    double dz = Z - otroPunto.Z;

                    return Math.Sqrt(dx * dx + dy * dy + dz * dz);
                }
            }

            // Definir una clase para representar una línea en un espacio tridimensional
            class Linea3D
            {
                public Punto3D Punto1 { get; set; }
                public Punto3D Punto2 { get; set; }

                public Linea3D(Punto3D punto1, Punto3D punto2)
                {
                    Punto1 = punto1;
                    Punto2 = punto2;
                }

                public double Longitud()
                {
                    return Punto1.Distancia(Punto2);
                }
            }

            // Definir una clase para representar un plano en un espacio tridimensional
            class Plano3D
            {
                public Punto3D Punto { get; set; }
                public Vector3D Normal { get; set; }

                public Plano3D(Punto3D punto, Vector3D normal)
                {
                    Punto = punto;
                    Normal = normal;
                }

                public double Distancia(Punto3D otroPunto)
                {
                    double a = Normal.X;
                    double b = Normal.Y;
                    double c = Normal.Z;
                    double d = -a * Punto.X - b * Punto.Y - c * Punto.Z;

                    double numerador = a * otroPunto.X + b * otroPunto.Y + c * otroPunto.Z + d;
                    double denominador = Math.Sqrt(a * a + b * b + c * c);

                    return Math.Abs(numerador / denominador);
                }
            }

            // Definir una clase para representar un vector en un espacio tridimensional
            class Vector3D
            {
                public double X { get; set; }
                public double Y { get; set; }
                public double Z { get; set; }

                public Vector3D(double x, double y, double z)
                {
                    X = x;
                    Y = y;
                    Z = z;
                }

                public double Norma()
                {
                    return Math.Sqrt(X * X + Y * Y + Z * Z);
                }

                public Vector3D Normalizar()
                {
                    double norma = Norma();

                    return new Vector3D(X / norma, Y / norma, Z / norma);
                }

                public double ProductoEscalar(Vector3D otroVector)
                {
                    return X * otroVector.X + Y * otroVector.Y + Z * otroVector.Z;
                }

                public Vector3D ProductoVectorial(Vector3D otroVector)
                {
                    double x = Y * otroVector.Z - Z * otroVector.Y;
                    double y = Z * otroVector.X - X * otroVector.Z;
                    double z = X * otroVector.Y - Y * otroVector.X;

                    return new Vector3D(x, y, z);
                }
            }

            // Crear algunos puntos, líneas y planos
            Punto3D punto1 = new Punto3D(1, 2, 3);
            Punto3D punto2 = new Punto3D(4, 5, 6);
            Linea3D linea1 = new Linea3D(punto1, punto2);
            Plano3D plano1 = new Plano3D(punto1, new Vector3D(1, 2, 3));

            // Calcular la distancia entre el punto1 y el plano1
            double distancia = plano1.Distancia(punto1);

            // Mostrar el resultado
            Console.WriteLine($"La distancia entre el punto1 y el plano1 es {distancia}");