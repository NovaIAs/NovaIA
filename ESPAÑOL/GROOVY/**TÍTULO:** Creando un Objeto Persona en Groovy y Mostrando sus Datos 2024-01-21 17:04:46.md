```groovy
// DEFINICIÓN DE UNA CLASE EN GROOVY

class Persona {
    String nombre
    int edad
    String sexo

    // CONSTRUCTOR DE LA CLASE
    Persona(String nombre, int edad, String sexo) {
        this.nombre = nombre
        this.edad = edad
        this.sexo = sexo
    }

    // MÉTODOS DE LA CLASE
    String getNombre() {
        return nombre
    }

    int getEdad() {
        return edad
    }

    String getSexo() {
        return sexo
    }

    // MÉTODO PARA IMPRIMIR LOS DATOS DE LA PERSONA
    String toString() {
        return "Nombre: ${nombre}, Edad: ${edad}, Sexo: ${sexo}"
    }
}

// EJECUCIÓN DEL CÓDIGO

// CREACIÓN DE UN OBJETO DE TIPO PERSONA
Persona persona1 = new Persona("Juan", 25, "Masculino")

// IMPRESIÓN DE LOS DATOS DE LA PERSONA
println persona1

```

**Explicación del código:**

1. **Definición de la clase Persona:** Se define una clase llamada `Persona` que tiene tres propiedades: `nombre`, `edad` y `sexo`.

2. **Constructor de la clase:** Se define un constructor para la clase `Persona` que inicializa las tres propiedades con los valores pasados como argumento.

3. **Métodos de la clase:** Se definen tres métodos para la clase `Persona`: `getNombre`, `getEdad` y `getSexo` que devuelven el valor de las propiedades correspondientes.

4. **Método `toString`:** Se define un método llamado `toString` que devuelve una cadena con los datos de la persona en el formato "Nombre: [nombre], Edad: [edad], Sexo: [sexo]".

5. **Ejecución del código:** Se crea un objeto de tipo `Persona` llamado `persona1` con los valores "Juan", 25 y "Masculino" para las propiedades `nombre`, `edad` y `sexo` respectivamente.

6. Se imprime el objeto `persona1` utilizando el método `println`.

Este código crea un objeto de tipo `Persona` y luego imprime sus datos utilizando el método `toString`. El código es complejo porque define una clase con varias propiedades y métodos, y utiliza el método `toString` para imprimir los datos del objeto de forma personalizada.