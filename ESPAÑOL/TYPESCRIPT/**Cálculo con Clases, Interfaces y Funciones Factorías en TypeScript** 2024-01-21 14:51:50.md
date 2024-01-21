```typescript
// Crear una clase Calculadora con los métodos suma, resta, multiplicación y división
class Calculadora {
  suma(num1, num2) {
    return num1 + num2;
  }

  resta(num1, num2) {
    return num1 - num2;
  }

  multiplicacion(num1, num2) {
    return num1 * num2;
  }

  division(num1, num2) {
    return num1 / num2;
  }
}

// Crear una interfaz Operacion que tenga una función llamada calcular
interface Operacion {
  calcular(num1: number, num2: number): number;
}

// Crear una clase Suma que implemente la interfaz Operacion
class Suma implements Operacion {
  calcular(num1: number, num2: number): number {
    return num1 + num2;
  }
}

// Crear una clase Resta que implemente la interfaz Operacion
class Resta implements Operacion {
  calcular(num1: number, num2: number): number {
    return num1 - num2;
  }
}

// Crear una clase Multiplicacion que implemente la interfaz Operacion
class Multiplicacion implements Operacion {
  calcular(num1: number, num2: number): number {
    return num1 * num2;
  }
}

// Crear una clase Division que implemente la interfaz Operacion
class Division implements Operacion {
  calcular(num1: number, num2: number): number {
    return num1 / num2;
  }
}

// Crear un arreglo de operaciones
const operaciones: Operacion[] = [new Suma(), new Resta(), new Multiplicacion(), new Division()];

// Recorrer el arreglo de operaciones y mostrar el resultado de cada una
operaciones.forEach(operacion => {
  console.log(operacion.calcular(10, 5));
});

// Crear una función factory que reciba una operación y devuelva una función que ejecute la operación
const crearOperacion = (operacion: string): Function => {
  switch (operacion) {
    case "suma":
      return (num1: number, num2: number): number => {
        return num1 + num2;
      };
    case "resta":
      return (num1: number, num2: number): number => {
        return num1 - num2;
      };
    case "multiplicacion":
      return (num1: number, num2: number): number => {
        return num1 * num2;
      };
    case "division":
      return (num1: number, num2: number): number => {
        return num1 / num2;
      };
    default:
      throw new Error("Operación no válida");
  }
};

// Crear una función que reciba dos números y una operación y devuelva el resultado de la operación
const calcular = (num1: number, num2: number, operacion: string): number => {
  const funcionOperacion = crearOperacion(operacion);
  return funcionOperacion(num1, num2);
};

// Mostrar el resultado de la suma, resta, multiplicación y división de 10 y 5
console.log(calcular(10, 5, "suma"));
console.log(calcular(10, 5, "resta"));
console.log(calcular(10, 5, "multiplicacion"));
console.log(calcular(10, 5, "division"));
```

Explicación:

* Se crean las clases `Calculadora`, `Suma`, `Resta`, `Multiplicacion` y `Division` que implementan la interfaz `Operacion`.
* Se crea un arreglo de operaciones que contiene instancias de las clases anteriores.
* Se recorre el arreglo de operaciones y se muestra el resultado de cada una.
* Se crea una función `crearOperacion` que recibe una operación y devuelve una función que ejecuta la operación.
* Se crea una función `calcular` que recibe dos números, una operación y devuelve el resultado de la operación.
* Se muestra el resultado de la suma, resta, multiplicación y división de 10 y 5.