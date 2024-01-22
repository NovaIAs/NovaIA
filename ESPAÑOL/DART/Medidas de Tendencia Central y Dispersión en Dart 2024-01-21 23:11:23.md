```dart
// Importamos la librería 'List'.
import 'dart:collection';

// Creamos una función `promedio` que recibe una lista de números y devuelve su promedio.
double promedio(List<num> numeros) {
  // Sumamos todos los números de la lista.
  double suma = 0;
  for (num numero in numeros) {
    suma += numero;
  }

  // Dividimos la suma por la cantidad de números de la lista.
  return suma / numeros.length;
}

// Creamos una función `mediana` que recibe una lista de números y devuelve su mediana.
num mediana(List<num> numeros) {
  // Ordenamos la lista de números.
  numeros.sort();

  // Si la lista tiene un número impar de elementos, la mediana es el elemento central.
  if (numeros.length % 2 == 1) {
    return numeros[numeros.length ~/ 2];
  }
  // Si la lista tiene un número par de elementos, la mediana es el promedio de los dos elementos centrales.
  else {
    return (numeros[numeros.length ~/ 2 - 1] + numeros[numeros.length ~/ 2]) / 2;
  }
}

// Creamos una función `moda` que recibe una lista de números y devuelve su moda.
num moda(List<num> numeros) {
  // Creamos un mapa que almacena los números y su frecuencia.
  Map<num, int> frecuencias = {};
  for (num numero in numeros) {
    if (frecuencias.containsKey(numero)) {
      frecuencias[numero]++;
    } else {
      frecuencias[numero] = 1;
    }
  }

  // Buscamos el número con mayor frecuencia.
  num moda = 0;
  int frecuenciaMaxima = 0;
  for (num numero in frecuencias.keys) {
    if (frecuencias[numero]! > frecuenciaMaxima) {
      moda = numero;
      frecuenciaMaxima = frecuencias[numero]!;
    }
  }

  // Devolvemos la moda.
  return moda;
}

// Creamos una función `rango` que recibe una lista de números y devuelve su rango.
num rango(List<num> numeros) {
  // Ordenamos la lista de números.
  numeros.sort();

  // Calculamos el rango restando el primer elemento del último elemento.
  return numeros.last - numeros.first;
}

//Creamos una función `variación` que recibe una lista de números y devuelve su variación.
double variación(List<num> numeros) {
  // Calculamos el promedio de los números de la lista.
  double promedio = promedio(numeros);

  // Calculamos la suma de las desviaciones cuadradas de los números con respecto al promedio.
  double sumaDesviacionesCuadradas = 0;
  for (num numero in numeros) {
    sumaDesviacionesCuadradas += (numero - promedio) * (numero - promedio);
  }

  // Dividimos la suma de las desviaciones cuadradas por la cantidad de números de la lista.
  return sumaDesviacionesCuadradas / numeros.length;
}

//Creamos una función `desviaciónEstandar` que recibe una lista de números y devuelve su desviación estándar.
double desviaciónEstandar(List<num> numeros) {
  // Calculamos la variación de los números de la lista.
  double variación = variación(numeros);

  // Calculamos la raíz cuadrada de la variación.
  return Math.sqrt(variación);
}

//Creamos una función `cuartiles` que recibe una lista de números y devuelve sus cuartiles.
List<num> cuartiles(List<num> numeros) {
  // Ordenamos la lista de números.
  numeros.sort();

  // Calculamos el primer cuartil, el segundo cuartil (mediana) y el tercer cuartil.
  num cuartil1 = numeros[numeros.length ~/ 4];
  num cuartil2 = mediana(numeros);
  num cuartil3 = numeros[numeros.length * 3 ~/ 4];

  // Devolvemos los cuartiles en una lista.
  return [cuartil1, cuartil2, cuartil3];
}

//Creamos una función `percentiles` que recibe una lista de números y devuelve sus percentiles.
List<num> percentiles(List<num> numeros, List<num> percentiles) {
  // Ordenamos la lista de números.
  numeros.sort();

  // Calculamos los percentiles.
  List<num> percentilesCalculados = [];
  for (num percentil in percentiles) {
    int índice = (percentil / 100 * (numeros.length - 1)).round();
    percentilesCalculados.add(numeros[índice]);
  }

  // Devolvemos los percentiles en una lista.
  return percentilesCalculados;
}

//Creamos una función `histograma` que recibe una lista de números y devuelve su histograma.
Map<num, int> histograma(List<num> numeros) {
  // Creamos un mapa que almacena los intervalos y su frecuencia.
  Map<num, int> frecuencias = {};

  // Calculamos el rango de los números.
  num rango = rango(numeros);

  // Calculamos el número de intervalos.
  int numeroIntervalos = 10;

  // Calculamos el tamaño de los intervalos.
  num tamañoIntervalos = rango / numeroIntervalos;

  // Creamos los intervalos.
  List<num> intervalos = [];
  for (int i = 0; i < numeroIntervalos; i++) {
    intervalos.add(i * tamañoIntervalos);
  }

  // Calculamos la frecuencia de cada intervalo.
  for (num numero in numeros) {
    for (int i = 0; i < numeroIntervalos; i++) {
      if (numero >= intervalos[i] && numero < intervalos[i + 1]) {
        if (frecuencias.containsKey(intervalos[i])) {
          frecuencias[intervalos[i]]++;
        } else {
          frecuencias[intervalos[i]] = 1;
        }
        break;
      }
    }
  }

  // Devolvemos el histograma.
  return frecuencias;
}

// Creamos una función `dispersión` que recibe dos listas de números y devuelve su diagrama de dispersión.
Map<num, List<num>> dispersión(List<num> numeros1, List<num> numeros2) {
  // Creamos un mapa que almacena los valores de x y su lista de valores de y.
  Map<num, List<num>> dispersión = {};

  // Recorremos la lista de números 1 y la lista de números 2.
  for (int i = 0; i < numeros1.length; i++) {
    // Si el valor de x ya existe en el mapa, añadimos el valor de y a su lista.
    if (dispersión.containsKey(numeros1[i])) {
      dispersión[numeros1[i]]!.add(numeros2[i]);
    }
    // Si el valor de x no existe en el mapa, creamos una nueva entrada con el valor de x y una lista con el valor de y.
    else {
      dispersión[numeros1[i]] = [numeros2[i]];
    }
  }

  // Devolvemos el diagrama de dispersión.
  return dispersión;
}

// Creamos una función `correlación` que recibe dos listas de números y devuelve su coeficiente de correlación lineal.
double correlación(List<num> numeros1, List<num> numeros2) {
  // Calculamos el promedio de los números de la lista 1 y la lista 2.
  double promedio1 = promedio(numeros1);
  double promedio2 = promedio(numeros2);

  // Calculamos la suma de las desviaciones cuadradas de los números de la lista 1 y la lista 2 con respecto a sus promedios.
  double sumaDesviacionesCuadradas1 = 0;
  double sumaDesviacionesCuadradas2 = 0;
  for (int i = 0; i < numeros1.length; i++) {
    sumaDesviacionesCuadradas1 += (numeros1[i] - promedio1) * (numeros1[i] - promedio1);
    sumaDesviacionesCuadradas2 += (numeros2[i] - promedio2) * (numeros2[i] - promedio2);
  }

  // Calculamos la suma de los productos de las desviaciones de los números de la lista 1 y la lista 2 con respecto a sus promedios.
  double sumaProductosDesviaciones = 0;
  for (int i = 0; i < numeros1.length; i++) {
    sumaProductosDesviaciones += (numeros1[i] - promedio1) * (numeros2[i] - promedio2);
  }

  // Calculamos el coeficiente de correlación lineal.
  return sumaProductosDesviaciones / Math.sqrt(sumaDesviacionesCuadradas1 * sumaDesviacionesCuadradas2);
}
```

Explicación del código:

1. **Funciones para Medidas de Tendencia Central**:
   - `promedio()`: Calcula el promedio de una lista de números.
   - `mediana()`: Calcula la mediana de una lista de números.
   - `moda()`: Calcula la moda de una lista de números.

2. **Funciones para Medidas de Dispersión**:
   - `rango()`: Calcula el rango de una lista de números.
   - `varianza()`: Calcula la varianza de una lista de números.
   - `desviaciónEstandar()`: Calcula la desviación estándar de una lista de números.

3. **Funciones para Medidas de Posición**:
   - `cuartiles()`: Calcula los cuartiles de una lista de números.
   - `percentiles()`: Calcula los percentiles de una lista de números.

4. **Funciones para Gráficas**:
   - `histograma()`: Calcula el histograma de una lista de números.
   - `dispersión()`: Calcula el diagrama de dispersión de dos listas de números.
   - `correlación()`: Calcula el coeficiente de correlación lineal de dos listas de números.