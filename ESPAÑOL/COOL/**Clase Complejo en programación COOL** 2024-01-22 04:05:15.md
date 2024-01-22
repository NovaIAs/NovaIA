```cool
clase Complejo {
  atributos:
    real: Real;
    imaginario: Real;

  métodos:
    constructor(real, imaginario) {
      this.real = real;
      this.imaginario = imaginario;
    }

    suma(otro) {
      return new Complejo(this.real + otro.real, this.imaginario + otro.imaginario);
    }

    resta(otro) {
      return new Complejo(this.real - otro.real, this.imaginario - otro.imaginario);
    }

    producto(otro) {
      return new Complejo(
        this.real * otro.real - this.imaginario * otro.imaginario,
        this.real * otro.imaginario + this.imaginario * otro.real
      );
    }

    división(otro) {
      const denominador = otro.real * otro.real + otro.imaginario * otro.imaginario;
      return new Complejo(
        (this.real * otro.real + this.imaginario * otro.imaginario) / denominador,
        (this.imaginario * otro.real - this.real * otro.imaginario) / denominador
      );
    }

    módulo() {
      return Math.sqrt(this.real * this.real + this.imaginario * this.imaginario);
    }

    argumento() {
      return Math.atan2(this.imaginario, this.real);
    }

    conjugado() {
      return new Complejo(this.real, -this.imaginario);
    }

    inverso() {
      const denominador = this.real * this.real + this.imaginario * this.imaginario;
      return new Complejo(this.real / denominador, -this.imaginario / denominador);
    }

    toString() {
      return `${this.real} + ${this.imaginario}i`;
    }
}

const c1 = new Complejo(3, 4);
const c2 = new Complejo(5, -2);

console.log(`c1 = ${c1}`);
console.log(`c2 = ${c2}`);

console.log(`c1 + c2 = ${c1.suma(c2)}`);
console.log(`c1 - c2 = ${c1.resta(c2)}`);
console.log(`c1 * c2 = ${c1.producto(c2)}`);
console.log(`c1 / c2 = ${c1.división(c2)}`);

console.log(`|c1| = ${c1.módulo()}`);
console.log(`arg(c1) = ${c1.argumento()}`);

console.log(`c1* = ${c1.conjugado()}`);
console.log(`c1^-1 = ${c1.inverso()}`);
```

Este código define una clase llamada `Complejo` que representa números complejos. La clase tiene tres atributos: `real` e `imaginario`, que se utilizan para almacenar las partes real e imaginaria del número complejo, respectivamente, y `conjugado`, que se utiliza para almacenar el conjugado del número complejo.

La clase también tiene varios métodos:

* `constructor`: El constructor de la clase, que toma dos argumentos, `real` e `imaginario`, y los utiliza para inicializar las partes real e imaginaria del número complejo.
* `suma`: Un método que suma dos números complejos y devuelve un nuevo número complejo que es la suma de los dos números.
* `resta`: Un método que resta dos números complejos y devuelve un nuevo número complejo que es la resta de los dos números.
* `producto`: Un método que multiplica dos números complejos y devuelve un nuevo número complejo que es el producto de los dos números.
* `división`: Un método que divide dos números complejos y devuelve un nuevo número complejo que es el resultado de la división de los dos números.
* `módulo`: Un método que devuelve el módulo del número complejo, que es la raíz cuadrada de la suma de los cuadrados de las partes real e imaginaria.
* `argumento`: Un método que devuelve el argumento del número complejo, que es el ángulo entre el eje real y el vector que representa el número complejo.
* `conjugado`: Un método que devuelve el conjugado del número complejo, que es el número complejo que tiene la misma parte real pero la parte imaginaria opuesta.
* `inverso`: Un método que devuelve el inverso del número complejo, que es el número complejo que tiene la misma parte real pero la parte imaginaria opuesta.
* `toString`: Un método que devuelve una representación de cadena del número complejo.

El código también define dos objetos de la clase `Complejo`, llamados `c1` y `c2`. Estos objetos se utilizan para demostrar los métodos de la clase.

El código imprime la suma, la resta, el producto, la división, el módulo, el argumento, el conjugado y el inverso de los dos objetos `Complejo` en la consola.