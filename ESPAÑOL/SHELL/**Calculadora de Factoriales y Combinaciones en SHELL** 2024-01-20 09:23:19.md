```shell
#!/bin/bash

# Función para calcular el factorial de un número
factorial() {
  local n=$1
  if [ $n -eq 0 ]; then
    echo 1
  else
    local result=1
    while [ $n -gt 1 ]; do
      result=$(($result * $n))
      n=$(($n - 1))
    done
    echo $result
  fi
}

# Función para calcular el número de combinaciones de m elementos tomados de n en n
combinaciones() {
  local n=$1
  local m=$2
  if [ $m -gt $n ]; then
    echo 0
  elif [ $m -eq 0 ]; then
    echo 1
  else
    local result=$(factorial $n)
    result=$(($result / $(factorial $m)))
    result=$(($result / $(factorial $(($n - $m)))))
    echo $result
  fi
}

# Programa principal
echo "Calculadora de factoriales y combinaciones"
echo "---------------------------------------"

# Pedir al usuario que introduzca un número
echo "Introduzca un número:"
read n

# Calcular el factorial del número
factorial_n=$(factorial $n)

# Pedir al usuario que introduzca otro número
echo "Introduzca otro número:"
read m

# Calcular el número de combinaciones de m elementos tomados de n en n
combinaciones_nm=$(combinaciones $n $m)

# Mostrar los resultados
echo "El factorial de $n es $factorial_n"
echo "El número de combinaciones de $m elementos tomados de $n en $n es $combinaciones_nm"
```

Explicación del código:


Este código crea un programa en SHELL que calcula el factorial de un número y el número de combinaciones de m elementos tomados de n en n.

```
#!/bin/bash
```

Esta línea indica que este es un script de bash.


```
# Función para calcular el factorial de un número
```

Esta línea define una función llamada factorial que calcula el factorial de un número.

```
factorial() {
```

Esta línea comienza la definición de la función factorial.

```
local n=$1
```

Esta línea toma el primer argumento pasado a la función (el número del que se quiere calcular el factorial) y lo asigna a la variable local n.

```
if [ $n -eq 0 ]; then
```

Esta línea comprueba si n es igual a 0. Si lo es, entonces el factorial de n es 1, así que devuelve 1.

```
else
```

Si n no es igual a 0, entonces el factorial se calcula multiplicando los números desde n hasta 1.

```
  local result=1
  while [ $n -gt 1 ]; do
    result=$(($result * $n))
    n=$(($n - 1))
  done
```

Estas líneas inicializan la variable result a 1, y luego entran en un bucle que multiplica result por n y disminuye n por 1 hasta que n sea igual a 1.

```
  echo $result
```

Esta línea imprime el valor de result.

```
fi
}
```

Estas líneas cierran la definición de la función factorial.


```
# Función para calcular el número de combinaciones de m elementos tomados de n en n
```

Esta línea define una función llamada combinaciones que calcula el número de combinaciones de m elementos tomados de n en n.

```
combinaciones() {
```

Esta línea comienza la definición de la función combinaciones.

```
local n=$1
local m=$2
```

Estas líneas toman los dos primeros argumentos pasados a la función (n y m) y los asignan a las variables locales n y m.

```
if [ $m -gt $n ]; then
```

Esta línea comprueba si m es mayor que n. Si lo es, entonces no hay combinaciones posibles, así que devuelve 0.

```
elif [ $m -eq 0 ]; then
```

Esta línea comprueba si m es igual a 0. Si lo es, entonces hay una sola combinación posible (la combinación vacía), así que devuelve 1.

```
else
```

Si m no es mayor que n y no es igual a 0, entonces el número de combinaciones se calcula utilizando la fórmula:

```
C(n, m) = n! / (m! * (n - m)!)
```

```
  local result=$(factorial $n)
  result=$(($result / $(factorial $m)))
  result=$(($result / $(factorial $(($n - $m)))))
```

Estas líneas calculan el factorial de n, el factorial de m y el factorial de (n - m), y luego dividen el factorial de n por el factorial de m y el factorial de (n - m) para obtener el número de combinaciones.

```
  echo $result
```

Esta línea imprime el valor de result.

```
fi
}
```

Estas líneas cierran la definición de la función combinaciones.


```
# Programa principal
```

Esta línea marca el comienzo del programa principal.

```
echo "Calculadora de factoriales y combinaciones"
echo "---------------------------------------"
```

Estas líneas imprimen un mensaje de bienvenida al usuario y una línea de separación.

```
# Pedir al usuario que introduzca un número
```

Esta línea imprime un mensaje pidiendo al usuario que introduzca un número.

```
echo "Introduzca un número:"
read n
```

Estas líneas leen el número introducido por el usuario y lo asignan a la variable n.

```
# Calcular el factorial del número
```

Esta línea imprime un mensaje indicando que se va a calcular el factorial del número introducido por el usuario.

```
factorial_n=$(factorial $n)
```

Esta línea llama a la función factorial para calcular el factorial de n y lo asigna a la variable factorial_n.

```
# Pedir al usuario que introduzca otro número
```

Esta línea imprime un mensaje pidiendo al usuario que introduzca otro número.

```
echo "Introduzca otro número:"
read m
```

Estas líneas leen el número introducido por el usuario y lo asignan a la variable m.

```
# Calcular el número de combinaciones de m elementos tomados de n en n
```

Esta línea imprime un mensaje indicando que se va a calcular el número de combinaciones de m elementos tomados de n en n.

```
combinaciones_nm=$(combinaciones $n $m)
```

Esta línea llama a la función combinaciones para calcular el número de combinaciones de m elementos tomados de n en n y lo asigna a la variable combinaciones_nm.

```
# Mostrar los resultados
```

Esta línea imprime un mensaje indicando que se van a mostrar los resultados.

```
echo "El factorial de $n es $factorial_n"
echo "El número de combinaciones de $m elementos tomados de $n en $n es $combinaciones_nm"
```

Estas líneas imprimen los valores de factorial_n y combinaciones_nm, que son el factorial de