```typescript
// Creación de una clase en TypeScript
class Complejo {
  // Propiedades privadas
  private real: number;
  private imaginario: number;

  // Constructor de la clase
  constructor(real: number, imaginario: number) {
    this.real = real;
    this.imaginario = imaginario;
  }

  // Métodos de acceso a las propiedades
  getParteReal(): number {
    return this.real;
  }

  getParteImaginaria(): number {
    return this.imaginario;
  }

  // Método para sumar dos números complejos
  sumar(otroComplejo: Complejo): Complejo {
    // Se crean dos variables para almacenar la suma de las partes reales e imaginarias
    let nuevaParteReal: number = this.real + otroComplejo.getParteReal();
    let nuevaParteImaginaria: number = this.imaginario + otroComplejo.getParteImaginaria();

    // Se crea un nuevo objeto de tipo Complejo con la suma de las partes reales e imaginarias
    let resultadoSuma = new Complejo(nuevaParteReal, nuevaParteImaginaria);

    // Se devuelve el resultado de la suma
    return resultadoSuma;
  }

  // Método para restar dos números complejos
  restar(otroComplejo: Complejo): Complejo {
    // Se crean dos variables para almacenar la resta de las partes reales e imaginarias
    let nuevaParteReal: number = this.real - otroComplejo.getParteReal();
    let nuevaParteImaginaria: number = this.imaginario - otroComplejo.getParteImaginaria();

    // Se crea un nuevo objeto de tipo Complejo con la resta de las partes reales e imaginarias
    let resultadoResta = new Complejo(nuevaParteReal, nuevaParteImaginaria);

    // Se devuelve el resultado de la resta
    return resultadoResta;
  }

  // Método para multiplicar dos números complejos
  multiplicar(otroComplejo: Complejo): Complejo {
    // Se crean dos variables para almacenar la multiplicación de las partes reales e imaginarias
    let nuevaParteReal: number = this.real * otroComplejo.getParteReal() - this.imaginario * otroComplejo.getParteImaginaria();
    let nuevaParteImaginaria: number = this.real * otroComplejo.getParteImaginaria() + this.imaginario * otroComplejo.getParteReal();

    // Se crea un nuevo objeto de tipo Complejo con la multiplicación de las partes reales e imaginarias
    let resultadoMultiplicacion = new Complejo(nuevaParteReal, nuevaParteImaginaria);

    // Se devuelve el resultado de la multiplicación
    return resultadoMultiplicacion;
  }

  // Método para dividir dos números complejos
  dividir(otroComplejo: Complejo): Complejo {
    // Se calcula el denominador de la división
    let denominador: number = Math.pow(otroComplejo.getParteReal(), 2) + Math.pow(otroComplejo.getParteImaginaria(), 2);

    // Se crean dos variables para almacenar la división de las partes reales e imaginarias
    let nuevaParteReal: number = (this.real * otroComplejo.getParteReal() + this.imaginario * otroComplejo.getParteImaginaria()) / denominador;
    let nuevaParteImaginaria: number = (this.imaginario * otroComplejo.getParteReal() - this.real * otroComplejo.getParteImaginaria()) / denominador;

    // Se crea un nuevo objeto de tipo Complejo con la división de las partes reales e imaginarias
    let resultadoDivision = new Complejo(nuevaParteReal, nuevaParteImaginaria);

    // Se devuelve el resultado de la división
    return resultadoDivision;
  }

  // Método para obtener el módulo del número complejo
  modulo(): number {
    // Se calcula el módulo del número complejo
    let modulo: number = Math.sqrt(Math.pow(this.real, 2) + Math.pow(this.imaginario, 2));

    // Se devuelve el módulo del número complejo
    return modulo;
  }

  // Método para obtener el argumento del número complejo
  argumento(): number {
    // Se calcula el argumento del número complejo
    let argumento: number = Math.atan2(this.imaginario, this.real);

    // Se devuelve el argumento del número complejo
    return argumento;
  }

  // Método para obtener la conjugada del número complejo
  conjugada(): Complejo {
    // Se crea un nuevo objeto de tipo Complejo con la parte imaginaria negada
    let conjugada = new Complejo(this.real, -this.imaginario);

    // Se devuelve la conjugada del número complejo
    return conjugada;
  }

  // Método para obtener la representación en cadena del número complejo
  toString(): string {
    // Se crea una cadena con la representación en cadena del número complejo
    let cadena: string = `(${this.real}, ${this.imaginario})`;

    // Se devuelve la cadena con la representación en cadena del número complejo
    return cadena;
  }
}

// Ejemplo de uso de la clase Complejo
let complejo1 = new Complejo(3, 4);
let complejo2 = new Complejo(5, -2);

// Se suman los dos números complejos
let resultadoSuma = complejo1.sumar(complejo2);

// Se restan los dos números complejos
let resultadoResta = complejo1.restar(complejo2);

// Se multiplican los dos números complejos
let resultadoMultiplicacion = complejo1.multiplicar(complejo2);

// Se dividen los dos números complejos
let resultadoDivision = complejo1.dividir(complejo2);

// Se obtienen los módulos de los dos números complejos
let moduloComplejo1 = complejo1.modulo();
let moduloComplejo2 = complejo2.modulo();

// Se obtienen los argumentos de los dos números complejos
let argumentoComplejo1 = complejo1.argumento();
let argumentoComplejo2 = complejo2.argumento();

// Se obtienen las conjugadas de los dos números complejos
let conjugadaComplejo1 = complejo1.conjugada();
let conjugadaComplejo2 = complejo2.conjugada();

// Se imprimen los resultados en la consola
console.log(`Suma: ${resultadoSuma}`);
console.log(`Resta: ${resultadoResta}`);
console.log(`Multiplicación: ${resultadoMultiplicacion}`);
console.log(`División: ${resultadoDivision}`);
console.log(`Módulo complejo 1: ${moduloComplejo1}`);
console.log(`Módulo complejo 2: ${moduloComplejo2}`);
console.log(`Argumento complejo 1: ${argumentoComplejo1}`);
console.log(`Argumento complejo 2: ${argumentoComplejo2}`);
console.log(`Conjugada complejo 1: ${conjugadaComplejo1}`);
console.log(`Conjugada complejo 2: ${conjugadaComplejo2}`);
```

Explicación del código:

* Se crea una clase llamada `Complejo` que representa un número complejo.
* La clase `Complejo` tiene dos propiedades privadas, `real` e `imaginario`, que representan la parte real y la parte imaginaria del número complejo, respectivamente.
* La clase `Complejo` tiene un constructor que inicializa las propiedades `real` e `imaginario` con los valores pasados como argumentos.
* La clase `Complejo` tiene varios métodos para realizar operaciones con números complejos, como sumar, restar, multiplicar, dividir, obtener el módulo, obtener el argumento, obtener la conjugada y obtener la representación en cadena del número complejo.
* Se crean dos objetos de tipo `Complejo` llamados `complejo1` y `complejo2`.
* Se realizan las operaciones de suma, resta, multiplicación y división con los dos objetos `Complejo`.
* Se obtienen los módulos, los argumentos y las conjugadas de los dos objetos `Complejo`.
* Se imprimen los resultados de las operaciones en la consola.