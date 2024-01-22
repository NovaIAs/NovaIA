```javascript
// 1. Variables y Objetos Complejos
let persona = {
  nombre: "Juan",
  apellido: "Pérez",
  edad: 30,
  hobbies: ["leer", "viajar", "cocinar"],
  mascotas: [
    { nombre: "Pelusa", especie: "perro" },
    { nombre: "Bigotes", especie: "gato" },
  ],
  direccion: {
    calle: "Av. Principal",
    numero: 123,
    ciudad: "Ciudad del Sol",
  },
};

// 2. Funciones Complejas
function sumar(a, b) {
  return a + b;
}

function multiplicar(a, b) {
  let resultado = 0;
  for (let i = 1; i <= b; i++) {
    resultado = sumar(resultado, a);
  }
  return resultado;
}

function factorial(n) {
  if (n === 0 || n === 1) {
    return 1;
  }
  return n * factorial(n - 1);
}

// 3. Estructuras de Control Anidadas
const notas = [90, 80, 70, 60, 50];
let promedio = 0;
for (let i = 0; i < notas.length; i++) {
  promedio += notas[i];
}
promedio /= notas.length;

if (promedio >= 90) {
  console.log("Excelente");
} else if (promedio >= 80) {
  console.log("Sobresaliente");
} else if (promedio >= 70) {
  console.log("Notables");
} else {
  console.log("Insuficiente");
}

// 4. Arrays y Matrices Multidimensionales
const matriz = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9],
];

for (let i = 0; i < matriz.length; i++) {
  for (let j = 0; j < matriz[i].length; j++) {
    console.log(matriz[i][j]);
  }
}

// 5. Objetos y Métodos Complejos
class Animal {
  constructor(nombre, especie) {
    this.nombre = nombre;
    this.especie = especie;
  }

  comer() {
    console.log(`${this.nombre} está comiendo.`);
  }

  dormir() {
    console.log(`${this.nombre} está durmiendo.`);
  }
}

class Perro extends Animal {
  constructor(nombre) {
    super(nombre, "perro");
  }

  ladrar() {
    console.log(`${this.nombre} está ladrando.`);
  }
}

const firulais = new Perro("Firulais");
firulais.comer();
firulais.dormir();
firulais.ladrar();

// 6. Callback, Funciones de Orden Superior y Closures
const numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

const cuadrados = numeros.map((numero) => {
  return numero * numero;
});

const pares = numeros.filter((numero) => {
  return numero % 2 === 0;
});

const suma = numeros.reduce((acumulador, numero) => {
  return acumulador + numero;
}, 0);

const mayorQue5 = numeros.some((numero) => {
  return numero > 5;
});

const todosMayoresQue5 = numeros.every((numero) => {
  return numero > 5;
});

console.log(cuadrados);
console.log(pares);
console.log(suma);
console.log(mayorQue5);
console.log(todosMayoresQue5);

// 7. Asincronismo y Promesas
const obtenerDatos = () => {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      resolve("Datos obtenidos correctamente.");
    }, 3000);
  });
};

obtenerDatos()
  .then((datos) => {
    console.log(datos);
  })
  .catch((error) => {
    console.error(error);
  });

// 8. Expresiones Regulares
const texto = "Hola, mundo! Este es un texto de prueba.";
const regex = /mundo/;

const resultado = texto.match(regex);
console.log(resultado);

// 9. Números Complejos
const complejo1 = new Complex(3, 4);
const complejo2 = new Complex(5, -2);

const sumaComplejos = complejo1.sumar(complejo2);
const restaComplejos = complejo1.restar(complejo2);
const multiplicacionComplejos = complejo1.multiplicar(complejo2);
const divisionComplejos = complejo1.dividir(complejo2);

console.log(sumaComplejos);
console.log(restaComplejos);
console.log(multiplicacionComplejos);
console.log(divisionComplejos);

// 10. Árbol Binario de Búsqueda
class Nodo {
  constructor(valor) {
    this.valor = valor;
    this.izquierdo = null;
    this.derecho = null;
  }
}

class ArbolBinarioDeBusqueda {
  constructor() {
    this.raiz = null;
  }

  insertar(valor) {
    const nuevoNodo = new Nodo(valor);
    if (!this.raiz) {
      this.raiz = nuevoNodo;
    } else {
      this._insertar(nuevoNodo, this.raiz);
    }
  }

  _insertar(nuevoNodo, nodoActual) {
    if (nuevoNodo.valor < nodoActual.valor) {
      if (!nodoActual.izquierdo) {
        nodoActual.izquierdo = nuevoNodo;
      } else {
        this._insertar(nuevoNodo, nodoActual.izquierdo);
      }
    } else {
      if (!nodoActual.derecho) {
        nodoActual.derecho = nuevoNodo;
      } else {
        this._insertar(nuevoNodo, nodoActual.derecho);
      }
    }
  }

  buscar(valor) {
    return this._buscar(valor, this.raiz);
  }

  _buscar(valor, nodoActual) {
    if (!nodoActual) {
      return null;
    }
    if (valor === nodoActual.valor) {
      return nodoActual;
    }
    if (valor < nodoActual.valor) {
      return this._buscar(valor, nodoActual.izquierdo);
    } else {
      return this._buscar(valor, nodoActual.derecho);
    }
  }

  eliminar(valor) {
    this.raiz = this._eliminar(valor, this.raiz);
  }

  _eliminar(valor, nodoActual) {
    if (!nodoActual) {
      return null;
    }
    if (valor === nodoActual.valor) {
      if (!nodoActual.izquierdo && !nodoActual.derecho) {
        return null;
      }
      if (!nodoActual.izquierdo) {
        return nodoActual.derecho;
      }
      if (!nodoActual.derecho) {
        return nodoActual.izquierdo;
      }
      const sucesor = this._encontrarSucesor(nodoActual.derecho);
      nodoActual.valor = sucesor.valor;
      nodoActual.derecho = this._eliminar(sucesor.valor, nodoActual.derecho);
    } else if (valor < nodoActual.valor) {
      nodoActual.izquierdo = this._eliminar(valor, nodoActual.izquierdo);
    } else {
      nodoActual.derecho = this._eliminar(valor, nodoActual.derecho);
    }
    return nodoActual;
  }

  _encontrarSucesor(nodoActual) {
    if (!nodoActual.izquierdo) {
      return nodoActual;
    }
    return this._encontrarSucesor(nodoActual.izquierdo);
  }
}

const arbol = new ArbolBinarioDeBusqueda();
arbol.insertar(10);
arbol.insertar(5);
arbol.insertar(15);
arbol.insertar(2);
arbol.insertar(7);
arbol.insertar(12);
arbol.insertar(20);

console.log(arbol.buscar(10));
console.log(arbol.buscar(15));
arbol.eliminar(10);
console.log(arbol.buscar(10));
```

Explicación:

1. **Variables y Objetos Complejos:** Se define un objeto llamado `persona` que contiene información sobre una persona. El objeto tiene propiedades como `nombre`, `edad`, `hobbies`, `mascotas` y `direccion`.

2. **Funciones Complejas:** Se definen tres funciones complejas: `sumar`, `multiplicar` y `factorial`. La función `sumar` suma dos números, la función `multiplicar` multiplica un número por otro un número determinado de veces, y la función `factorial` calcula el factorial de un número.

3. **Estructuras de Control Anidadas:** Se utiliza un bucle `for` anidado para calcular el promedio de las notas de un alumno. También se utiliza una estructura `if` anidada para determinar la calificación del alumno en función de su promedio.

4. **Arrays y Matrices Multidimensionales:** Se define un array llamado `matriz` que contiene una matriz de números de 3x3. Se utilizan bucles `for` anidados para recorrer la matriz y mostrar sus elementos.

5. **Objetos y Métodos Complejos:** Se define una clase llamada `Animal` que tiene dos propiedades: `nombre` y `especie`. También tiene dos métodos: `comer` y `dormir`. Se define otra clase llamada `Perro` que hereda de la clase `Animal`. La clase `Perro` tiene un método adicional llamado `ladrar`.

6. **Callback, Funciones de Orden Superior y Closures:** Se utilizan funciones de orden superior como `map`, `filter`, `reduce`, `some` y `every` para procesar un array de números.

7. **Asincronismo y Promesas:** Se muestra el uso de promesas para manejar operaciones asíncronas.

8. **Expresiones Regulares:** Se utiliza una expresión regular para buscar una coincidencia en un texto.

9. **Números Complejos:** Se define una clase llamada `Complejo` que representa un número complejo. La clase `Complejo` tiene métodos para realizar operaciones aritméticas con números complejos.

10. **Árbol Binario de Búsqueda:** Se define una clase llamada `ArbolBinarioDeBusqueda` que representa un árbol binario de búsqueda. El árbol binario de búsqueda tiene métodos para insertar, eliminar y buscar elementos.