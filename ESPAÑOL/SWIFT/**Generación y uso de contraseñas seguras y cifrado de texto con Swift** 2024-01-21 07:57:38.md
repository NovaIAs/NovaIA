```swift
import Foundation

// Definir una función para generar una contraseña aleatoria segura
func generarContraseñaSegura(longitud: Int) -> String {
    // Crear un conjunto de caracteres válidos para la contraseña
    let caracteresValidos = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+=-"

    // Crear un array vacío para almacenar la contraseña
    var contraseña = [Character]()

    // Generar un número aleatorio para cada posición de la contraseña
    for _ in 0..<longitud {
        let indiceAleatorio = Int.random(in: 0..<caracteresValidos.count)

        // Añadir el carácter correspondiente a la posición aleatoria a la contraseña
        contraseña.append(caracteresValidos[caracteresValidos.index(caracteresValidos.startIndex, offsetBy: indiceAleatorio)])
    }

    // Devolver la contraseña
    return String(contraseña)
}

// Definir una función para cifrar una cadena de texto usando el algoritmo AES-256
func cifrarTexto(texto: String, clave: String) -> String? {
    // Crear una instancia del cifrador AES-256
    let cifrador = try? AES.init(key: clave, blockMode: .CBC)

    // Cifrar el texto
    guard let textoCifrado = try? cifrador?.encrypt(plaintext: texto.data(using: .utf8)) else {
        return nil
    }

    // Devolver el texto cifrado como una cadena de bytes codificada en Base64
    return textoCifrado.base64EncodedString()
}

// Definir una función para descifrar una cadena de texto cifrada usando el algoritmo AES-256
func descifrarTexto(textoCifrado: String, clave: String) -> String? {
    // Crear una instancia del cifrador AES-256
    let cifrador = try? AES.init(key: clave, blockMode: .CBC)

    // Descifrar el texto
    guard let textoDescifrado = try? cifrador?.decrypt(ciphertext: Data(base64Encoded: textoCifrado)) else {
        return nil
    }

    // Devolver el texto descifrado como una cadena de texto
    return String(data: textoDescifrado, encoding: .utf8)
}

// Generar una contraseña aleatoria segura de 12 caracteres
let contraseña = generarContraseñaSegura(longitud: 12)

// Cifrar un texto usando la contraseña generada
let textoCifrado = cifrarTexto(texto: "Texto secreto", clave: contraseña)

// Descifrar el texto cifrado usando la contraseña generada
let textoDescifrado = descifrarTexto(textoCifrado: textoCifrado!, clave: contraseña)

// Imprimir el texto descifrado
print(textoDescifrado!)
```

Explicación:

* La función `generarContraseñaSegura` genera una contraseña aleatoria segura de una longitud especificada.
* La función `cifrarTexto` cifra una cadena de texto usando el algoritmo AES-256 y una clave especificada.
* La función `descifrarTexto` descifra una cadena de texto cifrada usando el algoritmo AES-256 y una clave especificada.

En el código principal:

* Se genera una contraseña aleatoria segura de 12 caracteres usando la función `generarContraseñaSegura`.
* Se cifra un texto usando la contraseña generada y la función `cifrarTexto`.
* Se descifra el texto cifrado usando la contraseña generada y la función `descifrarTexto`.
* Se imprime el texto descifrado.