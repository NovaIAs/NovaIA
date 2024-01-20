```smalltalk
```
Este código en Smalltalk crea una clase llamada `Persona`, que tiene dos propiedades: `nombre` y `edad`. También crea una instancia de la clase `Persona` llamada `juan` y establece sus propiedades en "Juan" y 30, respectivamente. Finalmente, imprime el nombre y la edad de `juan` en la consola.

```smalltalk
clase Persona
    propiedades [{ :nombre | String } { :edad | Número }]
    métodos [{ :inicializar | :nombre :edad |
        nombre := nombre.
        edad := edad.
    } { :nombre |
        ^nombre.
    } { :edad |
        ^edad.
    }]
fin

persona := Persona inicializar.nombre: "Juan".edad: 30.

Console println: persona nombre.
Console println: persona edad.
```

Explicación del código:

* La clase `Persona` se define con la palabra clave `clase`.
* Las propiedades de la clase se definen entre corchetes, separadas por comas. Cada propiedad tiene un nombre y un tipo.
* Los métodos de la clase se definen entre corchetes, separados por comas. Cada método tiene un nombre y un bloque de código que se ejecuta cuando se llama.
* La instancia `juan` de la clase `Persona` se crea con el mensaje `inicializar`, que toma dos argumentos: `nombre` y `edad`.
* Los mensajes `nombre` y `edad` se envían a `juan` para obtener sus propiedades.
* El mensaje `println` se envía a la `Console` para imprimir el nombre y la edad de `juan` en la consola.