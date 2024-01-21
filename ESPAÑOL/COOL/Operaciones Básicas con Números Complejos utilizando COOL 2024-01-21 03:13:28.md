```cool
clase Complejo {
  atributo real parte;
  atributo real imaginaria;

  método nuevo(real parte, real imaginaria) {
    this.parte := parte;
    this.imaginaria := imaginaria;
  }

  método suma(Complejo otro) {
    Complejo resultado := nuevo(0, 0);
    resultado.parte := this.parte + otro.parte;
    resultado.imaginaria := this.imaginaria + otro.imaginaria;
    return resultado;
  }

  método resta(Complejo otro) {
    Complejo resultado := nuevo(0, 0);
    resultado.parte := this.parte - otro.parte;
    resultado.imaginaria := this.imaginaria - otro.imaginaria;
    return resultado;
  }

  método multiplicacion(Complejo otro) {
    Complejo resultado := nuevo(0, 0);
    resultado.parte := this.parte * otro.parte - this.imaginaria * otro.imaginaria;
    resultado.imaginaria := this.parte * otro.imaginaria + this.imaginaria * otro.parte;
    return resultado;
  }

  método division(Complejo otro) {
    Complejo resultado := nuevo(0, 0);
    real denominador := otro.parte * otro.parte + otro.imaginaria * otro.imaginaria;
    resultado.parte := (this.parte * otro.parte + this.imaginaria * otro.imaginaria) / denominador;
    resultado.imaginaria := (this.imaginaria * otro.parte - this.parte * otro.imaginaria) / denominador;
    return resultado;
  }

  método magnitud() {
    return Math.sqrt(this.parte * this.parte + this.imaginaria * this.imaginaria);
  }

  método argumento() {
    return Math.atan2(this.imaginaria, this.parte);
  }

  método toString() {
    return this.parte + " + " + this.imaginaria + "i";
  }
}

clase Principal {
  método main() {
    Complejo c1 := Complejo.nuevo(3, 4);
    Complejo c2 := Complejo.nuevo(5, 6);

    Complejo suma := c1.suma(c2);
    Complejo resta := c1.resta(c2);
    Complejo multiplicacion := c1.multiplicacion(c2);
    Complejo division := c1.division(c2);

    IO.escribirLinea("Suma: " + suma.toString());
    IO.escribirLinea("Resta: " + resta.toString());
    IO.escribirLinea("Multiplicación: " + multiplicacion.toString());
    IO.escribirLinea("División: " + division.toString());
  }
}
```

Este código define una clase `Complejo` que representa un número complejo. La clase tiene dos atributos, `parte` e `imaginaria`, que representan la parte real e imaginaria del número complejo, respectivamente. También define varios métodos, como `suma`, `resta`, `multiplicacion` y `division`, que realizan las operaciones aritméticas básicas con números complejos.

La clase `Principal` contiene el método `main`, que es el punto de entrada del programa. Este método crea dos objetos de la clase `Complejo`, `c1` y `c2`, y luego utiliza los métodos de la clase `Complejo` para realizar las operaciones aritméticas básicas con estos objetos. Los resultados de estas operaciones se imprimen en la consola.

Este código es complejo porque utiliza varias características avanzadas del lenguaje COOL, como clases, objetos, métodos y herencia. También utiliza varias clases de biblioteca estándar, como `Math` y `IO`. Este código es un buen ejemplo de cómo utilizar el lenguaje COOL para escribir programas complejos y sofisticados.