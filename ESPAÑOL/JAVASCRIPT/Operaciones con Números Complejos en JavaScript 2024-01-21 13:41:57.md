```javascript
// Definimos una función llamada "sumaCompleja" que recibe dos números complejos como parámetros.
function sumaCompleja(a, b) {
  // Extraemos las partes real e imaginaria de cada número complejo.
  var aReal = a[0];
  var aImaginaria = a[1];
  var bReal = b[0];
  var bImaginaria = b[1];

  // Sumamos las partes reales e imaginarias por separado.
  var sumaReal = aReal + bReal;
  var sumaImaginaria = aImaginaria + bImaginaria;

  // Devolvemos el resultado como un nuevo número complejo.
  return [sumaReal, sumaImaginaria];
}

// Definimos una función llamada "productoComplejo" que recibe dos números complejos como parámetros.
function productoComplejo(a, b) {
  // Extraemos las partes real e imaginaria de cada número complejo.
  var aReal = a[0];
  var aImaginaria = a[1];
  var bReal = b[0];
  var bImaginaria = b[1];

  // Calculamos el producto de las partes reales e imaginarias por separado.
  var productoReal = aReal * bReal - aImaginaria * bImaginaria;
  var productoImaginaria = aReal * bImaginaria + aImaginaria * bReal;

  // Devolvemos el resultado como un nuevo número complejo.
  return [productoReal, productoImaginaria];
}

// Definimos una función llamada "divisionCompleja" que recibe dos números complejos como parámetros.
function divisionCompleja(a, b) {
  // Extraemos las partes real e imaginaria de cada número complejo.
  var aReal = a[0];
  var aImaginaria = a[1];
  var bReal = b[0];
  var bImaginaria = b[1];

  // Calculamos el denominador, que es el cuadrado del módulo de b.
  var denominador = bReal * bReal + bImaginaria * bImaginaria;

  // Calculamos el numerador, que es el producto de a y el conjugado de b.
  var numeradorReal = aReal * bReal + aImaginaria * bImaginaria;
  var numeradorImaginaria = aImaginaria * bReal - aReal * bImaginaria;

  // Devolvemos el resultado como un nuevo número complejo.
  return [numeradorReal / denominador, numeradorImaginaria / denominador];
}

// Definimos una función llamada "moduloComplejo" que recibe un número complejo como parámetro.
function moduloComplejo(a) {
  // Extraemos las partes real e imaginaria del número complejo.
  var aReal = a[0];
  var aImaginaria = a[1];

  // Calculamos el módulo.
  var modulo = Math.sqrt(aReal * aReal + aImaginaria * aImaginaria);

  // Devolvemos el resultado.
  return modulo;
}

// Definimos una función llamada "conjugadoComplejo" que recibe un número complejo como parámetro.
function conjugadoComplejo(a) {
  // Extraemos las partes real e imaginaria del número complejo.
  var aReal = a[0];
  var aImaginaria = a[1];

  // Calculamos el conjugado.
  var conjugadoReal = aReal;
  var conjugadoImaginaria = -aImaginaria;

  // Devolvemos el resultado.
  return [conjugadoReal, conjugadoImaginaria];
}

// Definimos una función llamada "potenciaCompleja" que recibe un número complejo y un exponente como parámetros.
function potenciaCompleja(a, exponente) {
  // Calculamos la potencia real y la potencia imaginaria por separado.
  var potenciaReal = Math.pow(a[0], exponente);
  var potenciaImaginaria = Math.pow(a[1], exponente);

  // Devolvemos el resultado como un nuevo número complejo.
  return [potenciaReal, potenciaImaginaria];
}

// Definimos una función llamada "raizCompleja" que recibe un número complejo y un índice de la raíz como parámetros.
function raizCompleja(a, indice) {
  // Calculamos la raíz real y la raíz imaginaria por separado.
  var raizReal = Math.pow(a[0], 1 / indice);
  var raizImaginaria = Math.pow(a[1], 1 / indice);

  // Devolvemos el resultado como un nuevo número complejo.
  return [raizReal, raizImaginaria];
}

// Definimos una función llamada "expComplejo" que recibe un número complejo como parámetro.
function expComplejo(a) {
  // Calculamos la exponencial real y la exponencial imaginaria por separado.
  var expReal = Math.exp(a[0]);
  var expImaginaria = Math.sin(a[1]) * Math.cosh(a[0]) + Math.cos(a[1]) * Math.sinh(a[0]);

  // Devolvemos el resultado como un nuevo número complejo.
  return [expReal, expImaginaria];
}

// Definimos una función llamada "logComplejo" que recibe un número complejo como parámetro.
function logComplejo(a) {
  // Calculamos el logaritmo real y el logaritmo imaginario por separado.
  var logReal = Math.log(Math.sqrt(a[0] * a[0] + a[1] * a[1]));
  var logImaginaria = Math.atan2(a[1], a[0]);

  // Devolvemos el resultado como un nuevo número complejo.
  return [logReal, logImaginaria];
}

// Definimos una función llamada "senComplejo" que recibe un número complejo como parámetro.
function senComplejo(a) {
  // Calculamos el seno real y el seno imaginario por separado.
  var senReal = Math.sin(a[0]) * Math.cosh(a[1]);
  var senImaginaria = Math.cos(a[0]) * Math.sinh(a[1]);

  // Devolvemos el resultado como un nuevo número complejo.
  return [senReal, senImaginaria];
}

// Definimos una función llamada "cosComplejo" que recibe un número complejo como parámetro.
function cosComplejo(a) {
  // Calculamos el coseno real y el coseno imaginario por separado.
  var cosReal = Math.cos(a[0]) * Math.cosh(a[1]);
  var cosImaginaria = -Math.sin(a[0]) * Math.sinh(a[1]);

  // Devolvemos el resultado como un nuevo número complejo.
  return [cosReal, cosImaginaria];
}

// Definimos una función llamada "tanComplejo" que recibe un número complejo como parámetro.
function tanComplejo(a) {