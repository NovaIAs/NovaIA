Una solución compleja en C# para una tarea de cifrado de archivos mediante cifrado AES de 256 bits con generación aleatoria de claves:

```csharp
using System;
using System.IO;
using System.Security.Cryptography;

class CifradoAvanzado
{
    // Clave de cifrado generada aleatoriamente
    private static byte[] GenerarClave()
    {
        byte[] clave = new byte[32]; // 256 bits
        using (var generador = new RNGCryptoServiceProvider())
        {
            generador.GetBytes(clave);
        }
        return clave;
    }

    // Vector de inicialización generado aleatoriamente
    private static byte[] GenerarVectorInicializacion()
    {
        byte[] vector = new byte[16]; // 128 bits
        using (var generador = new RNGCryptoServiceProvider())
        {
            generador.GetBytes(vector);
        }
        return vector;
    }

    // Cifrar un archivo usando AES de 256 bits y guardar la clave y el vector en un archivo .enc
    public static void CifrarArchivo(string archivoEntrada, string archivoSalida)
    {
        // Generar clave y vector
        byte[] clave = GenerarClave();
        byte[] vector = GenerarVectorInicializacion();

        // Crear cifrador AES
        using (var cifrador = Aes.Create())
        {
            // Configurar modo de operación CBC y rellenar espacios vacíos con PKCS7
            cifrador.Mode = CipherMode.CBC;
            cifrador.Padding = PaddingMode.PKCS7;

            // Establecer clave y vector
            cifrador.Key = clave;
            cifrador.IV = vector;

            // Abrir archivos
            using (var archivoLectura = File.OpenRead(archivoEntrada))
            using (var archivoEscritura = File.OpenWrite(archivoSalida))
            {
                // Crear CryptoStream para cifrar datos
                using (var cryptoStream = new CryptoStream(archivoEscritura, cifrador.CreateEncryptor(), CryptoStreamMode.Write))
                {
                    // Copiar datos cifrados al archivo de salida
                    archivoLectura.CopyTo(cryptoStream);
                }
            }
        }

        // Guardar clave y vector en un archivo .enc
        string archivoDatosClave = archivoSalida + ".enc";
        File.WriteAllBytes(archivoDatosClave, clave);
        File.AppendAllBytes(archivoDatosClave, vector);
    }

    // Descifrar un archivo usando AES de 256 bits, leyendo la clave y el vector del archivo .enc
    public static void DescifrarArchivo(string archivoEntrada, string archivoSalida)
    {
        // Leer clave y vector del archivo .enc
        string archivoDatosClave = archivoEntrada + ".enc";
        byte[] datosClave = File.ReadAllBytes(archivoDatosClave);
        byte[] clave = datosClave.Take(32).ToArray();
        byte[] vector = datosClave.Skip(32).ToArray();

        // Crear descifrador AES
        using (var descifrador = Aes.Create())
        {
            // Configurar modo de operación CBC y rellenar espacios vacíos con PKCS7
            descifrador.Mode = CipherMode.CBC;
            descifrador.Padding = PaddingMode.PKCS7;

            // Establecer clave y vector
            descifrador.Key = clave;
            descifrador.IV = vector;

            // Abrir archivos
            using (var archivoLectura = File.OpenRead(archivoEntrada))
            using (var archivoEscritura = File.OpenWrite(archivoSalida))
            {
                // Crear CryptoStream para descifrar datos
                using (var cryptoStream = new CryptoStream(archivoEscritura, descifrador.CreateDecryptor(), CryptoStreamMode.Write))
                {
                    // Copiar datos descifrados al archivo de salida
                    archivoLectura.CopyTo(cryptoStream);
                }
            }
        }
    }

    // Método principal para probar el cifrado y descifrado
    public static void Main(string[] args)
    {
        // Cifrar un archivo
        string archivoEntrada = "archivo_original.txt";
        string archivoSalidaCifrado = "archivo_cifrado.txt";
        CifrarArchivo(archivoEntrada, archivoSalidaCifrado);

        // Descifrar el archivo cifrado
        string archivoEntradaCifrado = "archivo_cifrado.txt";
        string archivoSalidaDescifrado = "archivo_descifrado.txt";
        DescifrarArchivo(archivoEntradaCifrado, archivoSalidaDescifrado);

        // Mostrar mensajes de confirmación
        Console.WriteLine("Cifrado exitoso!");
        Console.WriteLine("Descifrado exitoso!");
    }
}
```

Explicación:

- El código utiliza un cifrador AES de 256 bits para cifrar archivos.
- Genera aleatoriamente una clave de cifrado de 256 bits y un vector de inicialización de 128 bits.
- Utiliza el modo de operación CBC y rellena los espacios vacíos con PKCS7.
- Almacena la clave y el vector en un archivo .enc junto con el archivo cifrado.
- Para descifrar, lee la clave y el vector del archivo .enc y utiliza el descifrador AES para descifrar el archivo.
- El método principal prueba el cifrado y descifrado de un archivo.