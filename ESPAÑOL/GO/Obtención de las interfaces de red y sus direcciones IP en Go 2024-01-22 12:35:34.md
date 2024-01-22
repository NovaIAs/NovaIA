```go
package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

func main() {
	// Definimos una función para ejecutar comandos en el sistema operativo.
	var ejecutarComando = func(comando string) (string, error) {
		// Creamos un nuevo comando.
		cmd := exec.Command("sh", "-c", comando)

		// Ejecutamos el comando y capturamos la salida.
		stdout, err := cmd.CombinedOutput()

		// Si hubo un error, devolvemos el error.
		if err != nil {
			return "", err
		}

		// Devolvemos la salida del comando como una cadena de caracteres.
		return string(stdout), nil
	}

	// Definimos una función para obtener el nombre de las interfaces de red.
	var obtenerInterfacesDeRed = func() ([]string, error) {
		// Ejecutamos el comando `ip link show` para obtener las interfaces de red.
		salida, err := ejecutarComando("ip link show")

		// Si hubo un error, devolvemos el error.
		if err != nil {
			return nil, err
		}

		// Dividimos la salida en líneas.
		lineas := strings.Split(salida, "\n")

		// Creamos una lista para almacenar los nombres de las interfaces de red.
		var interfacesDeRed []string

		// Recorremos las líneas y extraemos los nombres de las interfaces de red.
		for _, linea := range lineas {
			// Si la línea comienza con "link/" y no contiene "lo", es una interfaz de red.
			if strings.HasPrefix(linea, "link/") && !strings.Contains(linea, "lo") {
				// Extraemos el nombre de la interfaz de red.
				nombre := strings.Fields(linea)[1]

				// Añadimos el nombre de la interfaz de red a la lista.
				interfacesDeRed = append(interfacesDeRed, nombre)
			}
		}

		// Devolvemos la lista de nombres de las interfaces de red.
		return interfacesDeRed, nil
	}

	// Definimos una función para obtener las direcciones IP de una interfaz de red.
	var obtenerDireccionesIP = func(interfazDeRed string) ([]string, error) {
		// Ejecutamos el comando `ip addr show $interfazDeRed` para obtener las direcciones IP de la interfaz de red.
		salida, err := ejecutarComando(fmt.Sprintf("ip addr show %s", interfazDeRed))

		// Si hubo un error, devolvemos el error.
		if err != nil {
			return nil, err
		}

		// Dividimos la salida en líneas.
		lineas := strings.Split(salida, "\n")

		// Creamos una lista para almacenar las direcciones IP.
		var direccionesIP []string

		// Recorremos las líneas y extraemos las direcciones IP.
		for _, linea := range lineas {
			// Si la línea contiene "inet " y no contiene "127.0.0.1", es una dirección IP.
			if strings.Contains(linea, "inet ") && !strings.Contains(linea, "127.0.0.1") {
				// Extraemos la dirección IP.
				direccionIP := strings.Fields(linea)[1]

				// Añadimos la dirección IP a la lista.
				direccionesIP = append(direccionesIP, direccionIP)
			}
		}

		// Devolvemos la lista de direcciones IP.
		return direccionesIP, nil
	}

	// Obtenemos las interfaces de red.
	interfacesDeRed, err := obtenerInterfacesDeRed()

	// Si hubo un error, lo mostramos y salimos del programa.
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// Recorremos las interfaces de red.
	for _, interfazDeRed := range interfacesDeRed {
		// Obtenemos las direcciones IP de la interfaz de red.
		direccionesIP, err := obtenerDireccionesIP(interfazDeRed)

		// Si hubo un error, lo mostramos y pasamos a la siguiente interfaz de red.
		if err != nil {
			fmt.Println(err)
			continue
		}

		// Mostramos el nombre de la interfaz de red y sus direcciones IP.
		fmt.Println(interfazDeRed)
		for _, direccionIP := range direccionesIP {
			fmt.Println("\t", direccionIP)
		}
	}
}
```

Este código es un programa en Go que obtiene las interfaces de red y sus direcciones IP. El programa utiliza la función `exec.Command()` para ejecutar comandos en el sistema operativo. La función `obtenerInterfacesDeRed()` obtiene los nombres de las interfaces de red y la función `obtenerDireccionesIP()` obtiene las direcciones IP de una interfaz de red. El programa recorre las interfaces de red y muestra el nombre de cada interfaz de red y sus direcciones IP.