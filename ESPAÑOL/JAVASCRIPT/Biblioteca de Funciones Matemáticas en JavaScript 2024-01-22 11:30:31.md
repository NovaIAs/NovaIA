```javascript
// Función recursiva para calcular el factorial de un número
function factorial(num) {
  if (num === 0) {
    return 1;
  } else {
    return num * factorial(num - 1);
  }
}

// Función para encontrar el máximo común divisor de dos números
function mcd(a, b) {
  while (b !== 0) {
    let temp = b;
    b = a % b;
    a = temp;
  }
  return a;
}

// Función para encontrar el mínimo común múltiplo de dos números
function mcm(a, b) {
  return (a * b) / mcd(a, b);
}

// Función para encontrar el número primo más cercano a un número dado
function primoCercano(num) {
  if (num <= 1) {
    return 2;
  }

  let esPrimo = false;
  let candidato = num;

  while (!esPrimo) {
    candidato++;
    esPrimo = true;

    for (let i = 2; i <= Math.sqrt(candidato); i++) {
      if (candidato % i === 0) {
        esPrimo = false;
        break;
      }
    }
  }

  return candidato;
}

// Función para encontrar la raíz cuadrada de un número usando el método de Newton-Raphson
function raizCuadrada(num, precision) {
  if (num < 0) {
    throw new Error("No se puede encontrar la raíz cuadrada de un número negativo");
  }

  let x0 = num / 2;
  let x1 = 0;

  while (Math.abs(x1 - x0) > precision) {
    x1 = (x0 + num / x0) / 2;
    x0 = x1;
  }

  return x1;
}

// Función para encontrar el área de un triángulo rectángulo
function areaTrianguloRectangulo(base, altura) {
  return (base * altura) / 2;
}

// Función para encontrar el volumen de una pirámide cuadrangular regular
function volumenPiramideCuadrada(lado, altura) {
  return (1 / 3) * lado * lado * altura;
}

// Función para encontrar la ecuación de una recta dada dos puntos
function ecuacionRecta(x1, y1, x2, y2) {
  if (x1 === x2) {
    return "La recta es vertical";
  }

  let pendiente = (y2 - y1) / (x2 - x1);
  let ordenadaAlOrigen = y1 - pendiente * x1;

  return `y = ${pendiente}x + ${ordenadaAlOrigen}`;
}

// Función para encontrar las raíces de una ecuación cuadrática
function raicesEcuacionCuadratica(a, b, c) {
  let discriminante = b * b - 4 * a * c;

  if (discriminante < 0) {
    return "La ecuación no tiene raíces reales";
  } else if (discriminante === 0) {
    return -b / (2 * a);
  } else {
    let raiz1 = (-b + Math.sqrt(discriminante)) / (2 * a);
    let raiz2 = (-b - Math.sqrt(discriminante)) / (2 * a);
    return [raiz1, raiz2];
  }
}

// Función para encontrar el valor máximo y mínimo de una función en un intervalo dado
function maximoMinimoFuncion(funcion, intervalo) {
  let [minimo, maximo] = [funcion(intervalo[0]), funcion(intervalo[0])];

  for (let i = 1; i < intervalo.length; i++) {
    let valor = funcion(intervalo[i]);
    minimo = Math.min(minimo, valor);
    maximo = Math.max(maximo, valor);
  }

  return {
    minimo,
    maximo,
  };
}

// Función para encontrar la derivada de una función
function derivada(funcion, punto) {
  let h = 0.0001;
  return (funcion(punto + h) - funcion(punto)) / h;
}

// Función para encontrar la integral de una función en un intervalo dado
function integral(funcion, intervalo, n) {
  let ancho = (intervalo[1] - intervalo[0]) / n;
  let suma = 0;

  for (let i = 1; i < n; i++) {
    let xi = intervalo[0] + i * ancho;
    suma += funcion(xi);
  }

  return ancho * suma;
}

// Función para encontrar la raíz de una función usando el método de bisección
function raizBiseccion(funcion, intervalo, tolerancia) {
  let [a, b] = intervalo;
  let fa = funcion(a);
  let fb = funcion(b);

  if (fa * fb > 0) {
    throw new Error("La función no tiene raíz en el intervalo dado");
  }

  while (Math.abs(b - a) > tolerancia) {
    let c = (a + b) / 2;
    let fc = funcion(c);

    if (fa * fc < 0) {
      b = c;
    } else {
      a = c