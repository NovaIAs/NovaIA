```typescript
// Esta función recibe un array de números y devuelve el producto de todos ellos.
function producto(numeros: number[]): number {
  if (numeros.length === 0) {
    return 1;
  }

  return numeros[0] * producto(numeros.slice(1));
}

// Esta función recibe un array de números y devuelve la suma de todos ellos.
function suma(numeros: number[]): number {
  if (numeros.length === 0) {
    return 0;
  }

  return numeros[0] + suma(numeros.slice(1));
}

// Esta función recibe un array de números y devuelve la media de todos ellos.
function media(numeros: number[]): number {
  return suma(numeros) / numeros.length;
}

// Esta función recibe un array de números y devuelve el número máximo de todos ellos.
function maximo(numeros: number[]): number {
  if (numeros.length === 0) {
    throw new Error("El array no puede estar vacío.");
  }

  let max = numeros[0];
  for (let i = 1; i < numeros.length; i++) {
    if (numeros[i] > max) {
      max = numeros[i];
    }
  }

  return max;
}

// Esta función recibe un array de números y devuelve el número mínimo de todos ellos.
function minimo(numeros: number[]): number {
  if (numeros.length === 0) {
    throw new Error("El array no puede estar vacío.");
  }

  let min = numeros[0];
  for (let i = 1; i < numeros.length; i++) {
    if (numeros[i] < min) {
      min = numeros[i];
    }
  }

  return min;
}

// Esta función recibe un array de números y devuelve un nuevo array con los números ordenados de menor a mayor.
function ordenar(numeros: number[]): number[] {
  if (numeros.length <= 1) {
    return numeros;
  }

  const mitad = Math.floor(numeros.length / 2);
  const izquierda = ordenar(numeros.slice(0, mitad));
  const derecha = ordenar(numeros.slice(mitad));

  return merge(izquierda, derecha);
}

// Esta función recibe dos arrays de números ordenados y devuelve un nuevo array con los números ordenados de menor a mayor.
function merge(izquierda: number[], derecha: number[]): number[] {
  const resultado = [];
  let i = 0;
  let j = 0;

  while (i < izquierda.length && j < derecha.length) {
    if (izquierda[i] < derecha[j]) {
      resultado.push(izquierda[i]);
      i++;
    } else {
      resultado.push(derecha[j]);
      j++;
    }
  }

  while (i < izquierda.length) {
    resultado.push(izquierda[i]);
    i++;
  }

  while (j < derecha.length) {
    resultado.push(derecha[j]);
    j++;
  }

  return resultado;
}

// Esta función recibe un array de números y devuelve un nuevo array con los números filtrados según la condición especificada.
function filtrar(numeros: number[], condicion: (numero: number) => boolean): number[] {
  const resultado = [];
  for (let i = 0; i < numeros.length; i++) {
    if (condicion(numeros[i])) {
      resultado.push(numeros[i]);
    }
  }

  return resultado;
}

// Esta función recibe un array de números y devuelve un nuevo array con los números mapeados según la función especificada.
function mapear(numeros: number[], funcion: (numero: number) => number): number[] {
  const resultado = [];
  for (let i = 0; i < numeros.length; i++) {
    resultado.push(funcion(numeros[i]));
  }

  return resultado;
}

// Esta función recibe un array de números y devuelve un nuevo array con los números reducidos según la función especificada.
function reducir(numeros: number[], funcion: (acumulador: number, numero: number) => number, acumuladorInicial?: number): number {
  if (acumuladorInicial === undefined) {
    acumuladorInicial = numeros[0];
    numeros = numeros.slice(1);
  }

  for (let i = 0; i < numeros.length; i++) {
    acumuladorInicial = funcion(acumuladorInicial, numeros[i]);
  }

  return acumuladorInicial;
}

// Esta función recibe una cadena de texto y devuelve un objeto con la frecuencia de cada carácter en la cadena.
function contarCaracteres(cadena: string): Record<string, number> {
  const resultado = {};
  for (let i = 0; i < cadena.length; i++) {
    const caracter = cadena[i];
    if (resultado[caracter]