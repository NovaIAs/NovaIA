```shell
#!/bin/bash

# Función para comprobar si una cadena es un número entero.
es_entero() {
  [[ $1 =~ ^[0-9]+$ ]]
}

# Función para comprobar si una cadena es una dirección IP válida.
es_ip() {
  [[ $1 =~ ^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$ ]]
}

# Función para obtener la dirección IP de la interfaz especificada.
obtener_ip() {
  ifconfig $1 | grep 'inet ' | awk '{print $2}'
}

# Función para obtener la máscara de subred de la interfaz especificada.
obtener_mascara() {
  ifconfig $1 | grep 'netmask ' | awk '{print $4}'
}

# Función para obtener la puerta de enlace predeterminada de la interfaz especificada.
obtener_gateway() {
  ip route | grep 'default ' | awk '{print $3}'
}

# Función para obtener el DNS primario de la interfaz especificada.
obtener_dns_primario() {
  resolv.conf | grep 'nameserver ' | awk '{print $2}' | head -n 1
}

# Función para obtener el DNS secundario de la interfaz especificada.
obtener_dns_secundario() {
  resolv.conf | grep 'nameserver ' | awk '{print $2}' | tail -n 1
}

# Función para mostrar la información de red de la interfaz especificada.
mostrar_informacion_red() {
  echo "Dirección IP: $(obtener_ip $1)"
  echo "Máscara de subred: $(obtener_mascara $1)"
  echo "Puerta de enlace predeterminada: $(obtener_gateway $1)"
  echo "DNS primario: $(obtener_dns_primario $1)"
  echo "DNS secundario: $(obtener_dns_secundario $1)"
}

# Función principal del script.
main() {
  # Comprobar si se ha especificado una interfaz.
  if [ $# -eq 0 ]; then
    echo "Uso: $0 <interfaz>"
    exit 1
  fi

  # Comprobar si la interfaz existe.
  if ! ifconfig $1 &> /dev/null; then
    echo "La interfaz $1 no existe."
    exit 1
  fi

  # Comprobar si la interfaz está activa.
  if ! ifconfig $1 | grep 'UP' &> /dev/null; then
    echo "La interfaz $1 está desactivada."
    exit 1
  fi

  # Comprobar si la interfaz es una dirección IP válida.
  if ! es_ip $(obtener_ip $1); then
    echo "La dirección IP de la interfaz $1 no es válida."
    exit 1
  fi

  # Comprobar si la interfaz tiene una máscara de subred válida.
  if ! es_ip $(obtener_mascara $1); then
    echo "La máscara de subred de la interfaz $1 no es válida."
    exit 1
  fi

  # Comprobar si la interfaz tiene una puerta de enlace predeterminada válida.
  if ! es_ip $(obtener_gateway $1); then
    echo "La puerta de enlace predeterminada de la interfaz $1 no es válida."
    exit 1
  fi

  # Comprobar si la interfaz tiene un DNS primario válido.
  if ! es_ip $(obtener_dns_primario $1); then
    echo "El DNS primario de la interfaz $1 no es válido."
    exit 1
  fi

  # Comprobar si la interfaz tiene un DNS secundario válido.
  if ! es_ip $(obtener_dns_secundario $1); then
    echo "El DNS secundario de la interfaz $1 no es válido."
    exit 1
  fi

  # Mostrar la información de red de la interfaz.
  mostrar_informacion_red $1
}

# Llamar a la función principal del script.
main "$@"
```

Este script de shell es un ejemplo de un código complejo que es difícil de repetir. El script realiza una serie de comprobaciones para asegurarse de que la interfaz especificada es válida y tiene una configuración de red correcta. El script también muestra la información de red de la interfaz. El script utiliza varias funciones para realizar las comprobaciones y mostrar la información de red. Las funciones están definidas al principio del script y se llaman desde la función principal del script. El script también utiliza la sentencia `ifconfig` para obtener la información de red de la interfaz. La sentencia `ifconfig` es una herramienta de línea de comandos que se utiliza para configurar y mostrar la información de las interfaces de red. El script también utiliza la herramienta `grep` para filtrar la información de red que se muestra. La herramienta `grep` es una herramienta de línea de comandos que se utiliza para buscar una cadena de texto dentro de un archivo. El script también utiliza la herramienta `awk` para extraer la información de red de la salida de la sentencia `ifconfig`. La herramienta `awk` es una herramienta de línea de comandos que se utiliza para analizar y procesar texto.