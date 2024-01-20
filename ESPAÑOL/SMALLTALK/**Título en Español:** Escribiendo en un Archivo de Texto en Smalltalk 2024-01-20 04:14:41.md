**Código:**

```smalltalk
(StandardSystemFileStream new: "Prueba.txt")
    contents. { |archivo|
        "Archivo"
        write: "Hola Mundo!"
    }
```

**Explicación:**

1. El primer paso es crear un objeto `StandardSystemFileStream` que nos permitirá escribir en un archivo de texto. Llamamos a este objeto `archivo`.
2. A continuación, usamos el método `contents` para obtener el contenido del archivo. Esto nos devuelve un flujo de caracteres.
3. Luego, usamos el método `write` para escribir el mensaje "Hola Mundo!" en el flujo de caracteres.
4. Finalmente, cerramos el flujo de caracteres llamando al método `close`.

El código anterior creará un archivo de texto llamado "Prueba.txt" en el directorio actual y escribirá el mensaje "Hola Mundo!" en él.