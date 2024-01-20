**Objetivo:** Crear un programa en Java que:

1. **Interfaz de Usuario:** Muestra una interfaz de usuario con opciones para el usuario.
2. **Gestión de Archivos:** Permite al usuario leer y escribir en archivos de texto.
3. **Cifrado:** Utiliza la biblioteca Bouncy Castle para cifrar y descifrar archivos con el algoritmo AES.

**Código:**

```java
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.Key;
import org.bouncycastle.crypto.engines.AESEngine;
import org.bouncycastle.crypto.modes.CBCBlockCipher;
import org.bouncycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.bouncycastle.crypto.params.KeyParameter;

public class CifradoArchivos {

    public static void main(String[] args) throws Exception {
        // Opciones del usuario
        String opcion = "";
        String mensaje = "";
        String nombreArchivo = "";
        String claveCifrado = "";

        // Bucle principal
        while (!opcion.equals("Q")) {
            // Mostrar opciones
            System.out.println("Opciones:");
            System.out.println("1. Cifrar archivo");
            System.out.println("2. Descifrar archivo");
            System.out.println("3. Salir");
            System.out.println("Ingrese una opción: ");

            // Leer opción del usuario
            opcion = System.console().readLine();

            // Procesar opción
            switch (opcion) {
                case "1":
                    // Cifrar archivo
                    System.out.println("Ingrese el nombre del archivo a cifrar: ");
                    nombreArchivo = System.console().readLine();

                    System.out.println("Ingrese la clave de cifrado: ");
                    claveCifrado = System.console().readLine();

                    cifrarArchivo(nombreArchivo, claveCifrado);
                    break;
                case "2":
                    // Descifrar archivo
                    System.out.println("Ingrese el nombre del archivo a descifrar: ");
                    nombreArchivo = System.console().readLine();

                    System.out.println("Ingrese la clave de cifrado: ");
                    claveCifrado = System.console().readLine();

                    descifrarArchivo(nombreArchivo, claveCifrado);
                    break;
                case "3":
                    // Salir
                    System.out.println("Saliendo...");
                    break;
                default:
                    // Opción inválida
                    System.out.println("Opción inválida. Inténtelo de nuevo.");
                    break;
            }
        }
    }

    // Método para cifrar un archivo
    private static void cifrarArchivo(String nombreArchivo, String claveCifrado) throws Exception {
        // Obtener los bytes del archivo
        byte[] bytesArchivo = Files.readAllBytes(Paths.get(nombreArchivo));

        // Crear la clave de cifrado
        Key key = new KeyParameter(claveCifrado.getBytes());

        // Crear el cifrador
        PaddedBufferedBlockCipher cifrador = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESEngine()));

        // Inicializar el cifrador
        cifrador.init(true, key);

        // Cifrar los bytes del archivo
        byte[] bytesCifrados = cifrador.processBytes(bytesArchivo);

        // Escribir los bytes cifrados en un nuevo archivo
        String nombreArchivoCifrado = nombreArchivo + ".cif";
        Files.write(Paths.get(nombreArchivoCifrado), bytesCifrados);

        // Mostrar mensaje de éxito
        System.out.println("Archivo cifrado exitosamente en " + nombreArchivoCifrado);
    }

    // Método para descifrar un archivo
    private static void descifrarArchivo(String nombreArchivo, String claveCifrado) throws Exception {
        // Obtener los bytes del archivo cifrado
        byte[] bytesCifrados = Files.readAllBytes(Paths.get(nombreArchivo));

        // Crear la clave de cifrado
        Key key = new KeyParameter(claveCifrado.getBytes());

        // Crear el descifrador
        PaddedBufferedBlockCipher descifrador = new PaddedBufferedBlockCipher(new CBCBlockCipher(new AESEngine()));

        // Inicializar el descifrador
        descifrador.init(false, key);

        // Descifrar los bytes del archivo
        byte[] bytesDescifrados = descifrador.processBytes(bytesCifrados);

        // Escribir los bytes descifrados en un nuevo archivo
        String nombreArchivoDescifrado = nombreArchivo.replace(".cif", "");
        Files.write(Paths.get(nombreArchivoDescifrado), bytesDescifrados);

        // Mostrar mensaje de éxito
        System.out.println("Archivo descifrado exitosamente en " + nombreArchivoDescifrado);
    }
}
```

**Explicación del Código:**

1. Las clases `AESEngine` y `CBCBlockCipher` se utilizan para implementar el algoritmo de cifrado AES en modo CBC.
2. La clase `PaddedBufferedBlockCipher` se utiliza para agregar relleno a los datos antes de cifrarlos y eliminar el relleno después de descifrarlos.
3. La clase `KeyParameter` se utiliza para crear una clave de cifrado a partir de una cadena de caracteres.
4. El método `readAllBytes()` se utiliza para leer los bytes de un archivo en una matriz de bytes.
5. El método `processBytes()` se utiliza para cifrar o descifrar los bytes del archivo.
6. El método `write()` se utiliza para escribir los bytes cifrados o descifrados en un nuevo archivo.