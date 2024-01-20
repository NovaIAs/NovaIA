```cool
clase Mundo {
  entero x;
  entero y;
  entero z;

  construir Mundo(entero x, entero y, entero z) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  entero dameX() {
    return this.x;
  }

  entero dameY() {
    return this.y;
  }

  entero dameZ() {
    return this.z;
  }

  entero suma() {
    return this.x + this.y + this.z;
  }

  entero resta() {
    return this.x - this.y - this.z;
  }

  entero multiplicacion() {
    return this.x * this.y * this.z;
  }

  entero division() {
    return this.x / this.y / this.z;
  }

  entero modulo() {
    return this.x % this.y % this.z;
  }

  entero exponenciacion() {
    return this.x ** this.y ** this.z;
  }

  entero raizCuadrada() {
    return Math.sqrt(this.x);
  }

  entero raizCubica() {
    return Math.cbrt(this.x);
  }

  entero raizN(entero n) {
    return Math.pow(this.x, 1 / n);
  }

  entero potencia(entero n) {
    return Math.pow(this.x, n);
  }

  booleano esPar() {
    return this.x % 2 == 0;
  }

  booleano esImpar() {
    return this.x % 2 != 0;
  }

  booleano esPrimo() {
    if (this.x <= 1) {
      return false;
    }
    for (entero i = 2; i <= Math.sqrt(this.x); i++) {
      if (this.x % i == 0) {
        return false;
      }
    }
    return true;
  }

  booleano esCompuesto() {
    return !this.esPrimo();
  }

  booleano esMayorQue(entero otro) {
    return this.x > otro;
  }

  booleano esMenorQue(entero otro) {
    return this.x < otro;
  }

  booleano esMayorOIgualQue(entero otro) {
    return this.x >= otro;
  }

  booleano esMenorOIgualQue(entero otro) {
    return this.x <= otro;
  }

  booleano esIgualA(entero otro) {
    return this.x == otro;
  }

  booleano esDiferenteDe(entero otro) {
    return this.x != otro;
  }
}

clase Programa {
  entero main() {
    Mundo mundo = new Mundo(1, 2, 3);

    entero x = mundo.dameX();
    entero y = mundo.dameY();
    entero z = mundo.dameZ();

    entero suma = mundo.suma();
    entero resta = mundo.resta();
    entero multiplicacion = mundo.multiplicacion();
    entero division = mundo.division();
    entero modulo = mundo.modulo();
    entero exponenciacion = mundo.exponenciacion();
    entero raizCuadrada = mundo.raizCuadrada();
    entero raizCubica = mundo.raizCubica();
    entero raizN = mundo.raizN(2);
    entero potencia = mundo.potencia(2);

    booleano esPar = mundo.esPar();
    booleano esImpar = mundo.esImpar();
    booleano esPrimo = mundo.esPrimo();
    booleano esCompuesto = mundo.esCompuesto();

    booleano esMayorQue = mundo.esMayorQue(4);
    booleano esMenorQue = mundo.esMenorQue(4);
    booleano esMayorOIgualQue = mundo.esMayorOIgualQue(4);
    booleano esMenorOIgualQue = mundo.esMenorOIgualQue(4);
    booleano esIgualA = mundo.esIgualA(4);
    booleano esDiferenteDe = mundo.esDiferenteDe(4);

    System.out.println("x = " + x);
    System.out.println("y = " + y);
    System.out.println("z = " + z);

    System.out.println("suma = " + suma);
    System.out.println("resta = " + resta);
    System.out.println("multiplicacion = " + multiplicacion);
    System.out.println("division = " + division);
    System.out.println("modulo = " + modulo);
    System.out.println("exponenciacion = " + exponenciacion);
    System.out.println("raizCuadrada = " + raizCuadrada);
    System.out.println("raizCubica = " + raizCubica);
    System.out.println("raizN = " + raizN);
    System.out.println("potencia = " + potencia);

    System.out.println("esPar = " + esPar);
    System.out.println("esImpar = " + esImpar);
    System.out.println("esPrimo = " + esPrimo);
    System.out.println("esCompuesto = " + esCompuesto);

    System.out.println("esMayorQue = " + esMayorQue);
    System.out.println("esMenorQue = " + esMenorQue);
    System.out.println("esMayorOIgualQue = " + esMayorOIgualQue);
    System.out.println("esMenorOIgualQue = " + esMenorOIgualQue);
    System.out.println("esIgualA = " + esIgualA);
    System.out.println("esDiferenteDe = " + esDiferenteDe);

    return 0;
  }
}
```

Explicación del código:

* La clase `Mundo` representa un mundo tridimensional con tres coordenadas: `x`, `y` y `z`.
* La clase `Programa` contiene el método `main` que se ejecuta cuando se inicia el programa.
* En el método `main` se crea un objeto de la clase `Mundo` llamado `mundo` con las coordenadas `x = 1`, `y = 2` y `z = 3`.
* A continuación, se llaman a los métodos de la clase `Mundo` para obtener las coordenadas, sumarlas, restarlas, multiplicarlas, dividirlas, calcular el módulo, la exponenciación, la raíz cuadrada, la raíz cúbica, la raíz n-ésima y la potencia.
* También se llaman a los métodos de la clase `Mundo` para determinar si el número es par, impar, primo o compuesto.
* Por último, se llaman a los métodos de la clase `Mundo` para comparar el número con otro número.
* Los resultados de todas las operaciones se imprimen en la consola.