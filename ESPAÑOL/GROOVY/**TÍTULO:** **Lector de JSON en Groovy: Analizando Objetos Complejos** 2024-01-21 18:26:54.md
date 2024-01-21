```groovy

import groovy.json.JsonSlurper
import groovy.transform.CompileStatic
import groovy.util.logging.Slf4j

@CompileStatic
@Slf4j
class Main {

    static void main(String[] args) {
        def jsonSlurper = new JsonSlurper()
        def json = jsonSlurper.parseText("""
        {
            "nombre": "Juan",
            "apellido": "Pérez",
            "edad": 25,
            "hobbies": ["leer", "viajar", "escuchar música"],
            "mascotas": [
                {
                    "nombre": "Rocky",
                    "tipo": "perro",
                    "edad": 5
                },
                {
                    "nombre": "Michi",
                    "tipo": "gato",
                    "edad": 3
                }
            ]
        }
        """)

        log.info("Nombre: ${json.nombre}")
        log.info("Apellido: ${json.apellido}")
        log.info("Edad: ${json.edad}")
        log.info("Hobbies:")
        log.info("\t* ${json.hobbies[0]}")
        log.info("\t* ${json.hobbies[1]}")
        log.info("\t* ${json.hobbies[2]}")
        log.info("Mascotas:")
        for (mascota in json.mascotas) {
            log.info("\tNombre: ${mascota.nombre}")
            log.info("\tTipo: ${mascota.tipo}")
            log.info("\tEdad: ${mascota.edad}")
        }
    }
}
```

Explicación del código:

1. Importamos las siguientes clases y anotaciones:

    * `groovy.json.JsonSlurper`: Esta clase se utiliza para analizar texto JSON.
    * `groovy.transform.CompileStatic`: Esta anotación indica al compilador que debe compilar el código estáticamente, lo que puede mejorar el rendimiento.
    * `groovy.util.logging.Slf4j`: Esta anotación nos permite utilizar el registro SLF4J en nuestro código Groovy.

2. Definimos la clase `Main` que contiene el método `main` que se ejecuta cuando se ejecuta el script.

3. En el método `main`, creamos un objeto `JsonSlurper` y lo usamos para analizar una cadena JSON que contiene información sobre una persona (nombre, apellido, edad, aficiones y mascotas).

4. Usamos el símbolo de dispersión `.` para acceder a las propiedades del objeto JSON. Por ejemplo, `json.nombre` obtiene el valor de la propiedad `nombre` del objeto JSON.

5. Usamos el método `log.info` para registrar información en el registro. El método `log.info` toma un mensaje de cadena como argumento y lo registra en el registro.

6. Usamos un ciclo `for` para iterar sobre la lista de mascotas en el objeto JSON. Para cada mascota, registramos su nombre, tipo y edad.

Este código es complejo y diferenciado porque:

* Utiliza una variedad de características de Groovy, incluyendo el símbolo de dispersión, el método `log.info` y los ciclos `for`.
* Analiza una cadena JSON compleja y la utiliza para crear objetos Groovy.
* Registra información en el registro utilizando el marco SLF4J.