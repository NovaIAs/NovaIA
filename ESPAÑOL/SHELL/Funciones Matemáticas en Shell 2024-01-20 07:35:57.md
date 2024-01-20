```shell
# Define una función para comprobar si una cadena es un número.
function es_numero() {
  # Utiliza la expresión regular '^[0-9]+$' para comprobar si la cadena contiene sólo dígitos.
  [[ "$1" =~ ^[0-9]+$ ]]
}

# Define una función para calcular el factorial de un número.
function factorial() {
  # Si el número es 0 o 1, el factorial es 1.
  if [[ "$1" -eq 0 || "$1" -eq 1 ]]; then
    echo 1
  else
    # Si el número es mayor que 1, el factorial se calcula multiplicando el número por el factorial del número anterior.
    echo $(( "$1" * $(factorial $(($1 - 1))) ))
  fi
}

# Define una función para calcular la suma de los dígitos de un número.
function suma_digitos() {
  # Si el número es 0, la suma de los dígitos es 0.
  if [[ "$1" -eq 0 ]]; then
    echo 0
  else
    # Si el número es mayor que 0, la suma de los dígitos se calcula sumando el último dígito del número a la suma de los dígitos del número sin el último dígito.
    echo $((( "$1" % 10 ) + $(suma_digitos $(($1 / 10)))))
  fi
}

# Define una función para calcular el máximo común divisor de dos números.
function mcd() {
  # Si el segundo número es 0, el máximo común divisor es el primer número.
  if [[ "$2" -eq 0 ]]; then
    echo "$1"
  else
    # Si el segundo número es mayor que 0, el máximo común divisor se calcula calculando el máximo común divisor del segundo número y el resto de dividir el primer número entre el segundo número.
    echo $(mcd "$2" $(( "$1" % "$2" )))
  fi
}

# Define una función para calcular el mínimo común múltiplo de dos números.
function mcm() {
  # El mínimo común múltiplo es el producto de los dos números dividido por el máximo común divisor de los dos números.
  echo $(( "$1" * "$2" / $(mcd "$1" "$2") ))
}

# Pide al usuario que introduzca un número.
echo "Introduce un número:"
read numero

# Comprueba si el número introducido es un número.
if ! es_numero "$numero"; then
  # Si el número no es un número, muestra un mensaje de error.
  echo "El número introducido no es válido."
  exit 1
fi

# Calcula el factorial del número introducido.
factorial_numero=$(factorial "$numero")

# Calcula la suma de los dígitos del número introducido.
suma_digitos_numero=$(suma_digitos "$numero")

# Calcula el máximo común divisor del número introducido y 100.
mcd_100=$(mcd "$numero" 100)

# Calcula el mínimo común múltiplo del número introducido y 100.
mcm_100=$(mcm "$numero" 100)

# Muestra los resultados de los cálculos.
echo "El factorial de $numero es $factorial_numero"
echo "La suma de los dígitos de $numero es $suma_digitos_numero"
echo "El máximo común divisor de $numero y 100 es $mcd_100"
echo "El mínimo común múltiplo de $numero y 100 es $mcm_100"
```

Explicación del código:

1. Se definen cuatro funciones:

    * `es_numero()`: Comprueba si una cadena es un número.
    * `factorial()`: Calcula el factorial de un número.
    * `suma_digitos()`: Calcula la suma de los dígitos de un número.
    * `mcd()`: Calcula el máximo común divisor de dos números.
    * `mcm()`: Calcula el mínimo común múltiplo de dos números.

2. Se pide al usuario que introduzca un número.

3. Se comprueba si el número introducido es un número. Si no lo es, se muestra un mensaje de error y se termina el programa.

4. Se calculan el factorial, la suma de los dígitos, el máximo común divisor y el mínimo común múltiplo del número introducido.

5. Se muestran los resultados de los cálculos.