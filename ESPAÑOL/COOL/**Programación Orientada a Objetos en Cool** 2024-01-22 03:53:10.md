```cool
clase Persona {
  atributos {
    nombre: String;
    edad: Int;
    altura: Float;
  }

  métodos {
    construir(nombre: String, edad: Int, altura: Float) {
      self.nombre := nombre;
      self.edad := edad;
      self.altura := altura;
    }

    imprimir() {
      Console.println("Nombre: " + self.nombre + ", Edad: " + self.edad + ", Altura: " + self.altura);
    }
  }
}

clase Estudiante : Persona {
  atributos {
    matricula: Int;
    promedio: Float;
  }

  métodos {
    construir(nombre: String, edad: Int, altura: Float, matricula: Int, promedio: Float) {
      super.construir(nombre, edad, altura);
      self.matricula := matricula;
      self.promedio := promedio;
    }

    imprimir() {
      super.imprimir();
      Console.println("Matricula: " + self.matricula + ", Promedio: " + self.promedio);
    }
  }
}

clase Profesor : Persona {
  atributos {
    asignatura: String;
    sueldo: Int;
  }

  métodos {
    construir(nombre: String, edad: Int, altura: Float, asignatura: String, sueldo: Int) {
      super.construir(nombre, edad, altura);
      self.asignatura := asignatura;
      self.sueldo := sueldo;
    }

    imprimir() {
      super.imprimir();
      Console.println("Asignatura: " + self.asignatura + ", Sueldo: " + self.sueldo);
    }
  }
}

clase Main {
  métodos {
    principal() {
      persona1 := Persona.nueva("Juan", 20, 1.80);
      persona1.imprimir();

      estudiante1 := Estudiante.nueva("María", 18, 1.65, 123456, 8.5);
      estudiante1.imprimir();

      profesor1 := Profesor.nueva("Pedro", 40, 1.75, "Matemáticas", 30000);
      profesor1.imprimir();
    }
  }
}
```

**Explicación del código:**

* La clase `Persona` es una clase base que define las propiedades y métodos comunes a todas las personas.
* La clase `Estudiante` es una clase derivada de `Persona` que define las propiedades y métodos específicos de los estudiantes.
* La clase `Profesor` es una clase derivada de `Persona` que define las propiedades y métodos específicos de los profesores.
* La clase `Main` contiene el método `principal`, que es el punto de entrada del programa.
* En el método `principal`, se crean tres objetos: un objeto `Persona`, un objeto `Estudiante` y un objeto `Profesor`.
* Cada objeto se imprime en la consola utilizando el método `imprimir()`.

**Salida del programa:**

```
Nombre: Juan, Edad: 20, Altura: 1.8
Nombre: María, Edad: 18, Altura: 1.65
Matricula: 123456, Promedio: 8.5
Nombre: Pedro, Edad: 40, Altura: 1.75
Asignatura: Matemáticas, Sueldo: 30000
```