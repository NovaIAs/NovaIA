```shell
#!/bin/bash

# Esta es una función que comprueba si un número es primo.

es_primo() {
  local number=$1
  for (i=2; i<number; i++) do
    if [[ $((number%i)) -eq 0 ]]; then
      return 1
    fi
  done
  return 0
}

# Esta es una función que genera los números primos hasta un límite dado.

generar_primos() {
  local limit=$1
  for (i=2; i<=limit; i++) do
    if es_primo $i; then
      echo $i
    fi
  done
}

# Este es el código principal del script.

read -p "Ingrese un número límite: " limit
echo "Los números primos hasta $limit son:"
generar_primos $limit
```

Este código es complejo porque utiliza una función recursiva para generar los números primos hasta un límite dado. La función es_primo() comprueba si un número es primo, y la función generar_primos() utiliza la función es_primo() para generar todos los números primos hasta un límite dado.

El código principal del script solicita al usuario que ingrese un límite, y luego utiliza la función generar_primos() para generar todos los números primos hasta ese límite. Los números primos se imprimen en la consola.

Este código es útil para generar números primos para una variedad de propósitos, como la criptografía y la factorización.