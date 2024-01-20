**Código Complejo en Java**

```java
import java.util.*;
import java.io.*;
import java.math.*;
import java.lang.reflect.*;

public class Complejidad {

  // Función para generar números aleatorios
  private static int randomInt(int min, int max) {
    return (int) ((Math.random() * (max - min)) + min);
  }

  // Función para generar listas aleatorias de enteros
  private static List<Integer> randomList(int size, int min, int max) {
    List<Integer> list = new ArrayList<>();
    for (int i = 0; i < size; i++) {
      list.add(randomInt(min, max));
    }
    return list;
  }

  // Función para encontrar el máximo común divisor de dos números
  private static int gcd(int a, int b) {
    if (b == 0) {
      return a;
    }
    return gcd(b, a % b);
  }

  // Función para encontrar el mínimo común múltiplo de dos números
  private static int lcm(int a, int b) {
    return (a * b) / gcd(a, b);
  }

  // Función para encontrar la mediana de una lista de números
  private static double median(List<Integer> list) {
    Collections.sort(list);
    int size = list.size();
    if (size % 2 == 0) {
      return (list.get(size / 2 - 1) + list.get(size / 2)) / 2.0;
    } else {
      return list.get(size / 2);
    }
  }

  // Función para encontrar la moda de una lista de números
  private static List<Integer> mode(List<Integer> list) {
    Map<Integer, Integer> map = new HashMap<>();
    for (int num : list) {
      map.put(num, map.getOrDefault(num, 0) + 1);
    }
    int maxCount = 0;
    for (int count : map.values()) {
      maxCount = Math.max(maxCount, count);
    }
    List<Integer> modes = new ArrayList<>();
    for (Map.Entry<Integer, Integer> entry : map.entrySet()) {
      if (entry.getValue() == maxCount) {
        modes.add(entry.getKey());
      }
    }
    return modes;
  }

  // Función para encontrar las permutaciones de una cadena
  private static List<String> permutations(String str) {
    List<String> permutations = new ArrayList<>();
    if (str.length() == 1) {
      permutations.add(str);
    } else {
      for (int i = 0; i < str.length(); i++) {
        char currentChar = str.charAt(i);
        String remainingChars = str.substring(0, i) + str.substring(i + 1);
        for (String permutation : permutations(remainingChars)) {
          permutations.add(currentChar + permutation);
        }
      }
    }
    return permutations;
  }

  // Función para encontrar las combinaciones de una lista de elementos
  private static List<List<Integer>> combinations(List<Integer> list, int k) {
    List<List<Integer>> combinations = new ArrayList<>();
    if (k == 1) {
      for (int num : list) {
        List<Integer> combination = new ArrayList<>();
        combination.add(num);
        combinations.add(combination);
      }
    } else {
      for (int i = 0; i < list.size(); i++) {
        List<Integer> remainingList = list.subList(i + 1, list.size());
        for (List<Integer> combination : combinations(remainingList, k - 1)) {
          combination.add(0, list.get(i));
          combinations.add(combination);
        }
      }
    }
    return combinations;
  }

  // Función para encontrar los factores primos de un número
  private static List<Integer> primeFactors(int n) {
    List<Integer> factors = new ArrayList<>();
    for (int i = 2; i <= n / 2; i++) {
      while (n % i == 0) {
        factors.add(i);
        n /= i;
      }
    }
    if (n > 1) {
      factors.add(n);
    }
    return factors;
  }

  // Función para encontrar el número de divisores de un número
  private static int numDivisors(int n) {