```shell
#!/bin/bash

# Variables
archivos=$(find /home -name '*.txt')
palabras_unicas=""

# Función para contar las palabras únicas en un archivo
contar_palabras_unicas() {
  archivo=$1
  palabras=$(cat $archivo | tr -s ' ' '\n' | sort | uniq)
  for palabra in $palabras; do
    palabras_unicas+="$palabra "
  done
}

# Función para contar las palabras repetidas en un archivo
contar_palabras_repetidas() {
  archivo=$1
  palabras=$(cat $archivo | tr -s ' ' '\n' | sort)
  anterior=""
  repeticiones=0
  for palabra in $palabras; do
    if [ "$palabra" == "$anterior" ]; then
      repeticiones=$((repeticiones+1))
    else
      if [ $repeticiones -gt 0 ]; then
        echo "$anterior $repeticiones"
      fi
      anterior=$palabra
      repeticiones=0
    fi
  done
  if [ $repeticiones -gt 0 ]; then
    echo "$anterior $repeticiones"
  fi
}

# Recorrer los archivos y contar las palabras únicas y repetidas
for archivo in $archivos; do
  contar_palabras_unicas $archivo
  contar_palabras_repetidas $archivo
done

# Imprimir las palabras únicas
echo "Palabras únicas:"
echo $palabras_unicas

```

Este código busca todos los archivos con extensión `.txt` en el directorio `/home` y cuenta las palabras únicas y repetidas en cada uno de ellos.

El código consta de tres partes:

* Definición de variables:

```
archivos=$(find /home -name '*.txt')
palabras_unicas=""
```

* Definición de funciones:

```
contar_palabras_unicas() {
  archivo=$1
  palabras=$(cat $archivo | tr -s ' ' '\n' | sort | uniq)
  for palabra in $palabras; do
    palabras_unicas+="$palabra "
  done
}

contar_palabras_repetidas() {
  archivo=$1
  palabras=$(cat $archivo | tr -s ' ' '\n' | sort)
  anterior=""
  repeticiones=0
  for palabra in $palabras; do
    if [ "$palabra" == "$anterior" ]; then
      repeticiones=$((repeticiones+1))
    else
      if [ $repeticiones -gt 0 ]; then
        echo "$anterior $repeticiones"
      fi
      anterior=$palabra
      repeticiones=0
    fi
  done
  if [ $repeticiones -gt 0 ]; then
    echo "$anterior $repeticiones"
  fi
}
```

* Recorrer los archivos y contar las palabras únicas y repetidas:

```
for archivo in $archivos; do
  contar_palabras_unicas $archivo
  contar_palabras_repetidas $archivo
done
```

* Imprimir las palabras únicas:

```
echo "Palabras únicas:"
echo $palabras_unicas
```

El resultado del código será una lista de palabras únicas y una lista de palabras repetidas, junto con el número de repeticiones.