```cool
programa {
    funcion principal() { 
        clase Persona {
            propiedad nombre : String;
            propiedad edad : Int;

            metodo constructor(nombre : String, edad : Int) {
                self.nombre = nombre;
                self.edad = edad;
            }

            metodo saludar() {
                escribir "Hola, me llamo " + self.nombre + " y tengo " + self.edad + " años.";
            }
        }

        var persona1 = new Persona("Juan", 25);
        var persona2 = new Persona("María", 30);

        persona1.saludar();
        persona2.saludar();
    }
}
```

Explicación:

* El código define una clase `Persona` con dos propiedades, `nombre` y `edad`, y un método constructor que inicializa esas propiedades. También define un método `saludar()` que imprime un mensaje en la consola.
* Luego, el código crea dos instancias de la clase `Persona`, `persona1` y `persona2`, y llama al método `saludar()` para cada una de ellas.