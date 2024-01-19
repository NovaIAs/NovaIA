```shell
#!/bin/bash

# Este es un script de shell que implementa un algoritmo de búsqueda binaria.

# Definimos la función de búsqueda binaria.
function busqueda_binaria {
  # La función toma tres argumentos:
  # - El arreglo en el que se va a buscar.
  # - El elemento que se va a buscar.
  # - El índice inicial del arreglo.
  # - El índice final del arreglo.

  # Si el índice inicial es mayor que el índice final, significa que el elemento no está en el arreglo.
  if [ $1 -gt $2 ]; then
    echo "-1"
    return
  fi

  # Calculamos el índice medio del arreglo.
  medio=$((($1 + $2) / 2))

  # Si el elemento en el índice medio es igual al elemento que se busca, retornamos el índice medio.
  if [ ${arreglo[$medio]} -eq $3 ]; then
    echo "$medio"
    return
  fi

  # Si el elemento en el índice medio es mayor que el elemento que se busca, buscamos en la mitad izquierda del arreglo.
  if [ ${arreglo[$medio]} -gt $3 ]; then
    busqueda_binaria arreglo $1 $(($medio - 1)) $3
  # Si el elemento en el índice medio es menor que el elemento que se busca, buscamos en la mitad derecha del arreglo.
  else
    busqueda_binaria arreglo $(($medio + 1)) $2 $3
  fi
}

# Leemos el arreglo de números del usuario.
echo "Ingrese los números del arreglo, separados por espacios:"
read -a arreglo

# Leemos el elemento que se va a buscar del usuario.
echo "Ingrese el elemento que desea buscar:"
read elemento

# Llamamos a la función de búsqueda binaria para buscar el elemento en el arreglo.
indice=$(busqueda_binaria arreglo 0 $((${#arreglo[@]} - 1)) $elemento)

# Imprimimos el resultado de la búsqueda.
if [ $indice -eq -1 ]; then
  echo "El elemento no está en el arreglo."
else
  echo "El elemento se encuentra en el índice $indice del arreglo."
fi
```

Explicación del código:

* La primera línea del código define un script de shell.
* La segunda línea del código define una función llamada `busqueda_binaria`.
* La función `busqueda_binaria` toma tres argumentos:
    * El arreglo en el que se va a buscar.
    * El elemento que se va a buscar.
    * El índice inicial del arreglo.
    * El índice final del arreglo.
* La función `busqueda_binaria` utiliza el algoritmo de búsqueda binaria para buscar el elemento en el arreglo.
* La función `busqueda_binaria` retorna el índice del elemento en el arreglo si lo encuentra, o -1 si el elemento no está en el arreglo.
* La tercera línea del código lee el arreglo de números del usuario.
* La cuarta línea del código lee el elemento que se va a buscar del usuario.
* La quinta línea del código llama a la función `busqueda_binaria` para buscar el elemento en el arreglo.
* La sexta línea del código imprime el resultado de la búsqueda.