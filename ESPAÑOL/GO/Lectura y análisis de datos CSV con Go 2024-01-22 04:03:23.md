```go
package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"sort"
	"strings"
)

// Estructura para los datos de cada línea del archivo CSV
type Ciudad struct {
	Nombre      string
	Población   int
	Densidad    float64
	Superficie  float64
	Continente  string
	País        string
	ÍndiceHuman string
}

// Estructura para el JSON de salida
type DatosJSON struct {
	Ciudades []Ciudad
	Continentes []string
}

// Función para leer los datos de un archivo CSV
func leerCSV(archivo string) ([]Ciudad, error) {
	// Abrir el archivo CSV
	f, err := os.Open(archivo)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	// Crear un escáner para leer el archivo
	scanner := bufio.NewScanner(f)
	scanner.Split(bufio.ScanLines)

	// Leer las líneas del archivo
	var ciudades []Ciudad
	i := 0
	for scanner.Scan() {
		// Obtener la línea actual
		linea := scanner.Text()

		// Dividir la línea en campos
		campos := strings.Split(linea, ",")

		// Crear una nueva ciudad
		ciudad := Ciudad{
			Nombre:      campos[0],
			Población:   toInt(campos[1]),
			Densidad:    toFloat(campos[2]),
			Superficie:  toFloat(campos[3]),
			Continente:  campos[4],
			País:        campos[5],
			ÍndiceHuman: campos[6],
		}

		// Añadir la ciudad a la lista de ciudades
		ciudades = append(ciudades, ciudad)
		i++
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return ciudades, nil
}

// Función para convertir una cadena a un número entero
func toInt(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		return 0
	}
	return i
}

// Función para convertir una cadena a un número decimal
func toFloat(s string) float64 {
	f, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return 0
	}
	return f
}

// Función para obtener los continentes de las ciudades
func getContinentes(ciudades []Ciudad) []string {
	// Crear un mapa para almacenar los continentes
	continentes := make(map[string]bool)

	// Añadir los continentes de las ciudades al mapa
	for _, ciudad := range ciudades {
		continentes[ciudad.Continente] = true
	}

	// Convertir el mapa de continentes en una lista
	var listaContinentes []string
	for continente := range continentes {
		listaContinentes = append(listaContinentes, continente)
	}

	// Ordenar la lista de continentes
	sort.Strings(listaContinentes)

	return listaContinentes
}

// Función para crear el JSON de salida
func createJSON(ciudades []Ciudad, continentes []string) []byte {
	// Crear una estructura de datos para el JSON
	datos := DatosJSON{
		Ciudades:   ciudades,
		Continentes: continentes,
	}

	// Convertir la estructura de datos a JSON
	jsonBytes, err := json.MarshalIndent(datos, "", "  ")
	if err != nil {
		return nil
	}

	return jsonBytes
}

// Función para escribir el JSON de salida a un archivo
func writeJSON(jsonBytes []byte, archivo string) {
	// Crear un archivo para el JSON
	f, err := os.Create(archivo)
	if err != nil {
		return
	}
	defer f.Close()

	// Escribir el JSON al archivo
	f.Write(jsonBytes)
}

func main() {
	// Leer los datos del archivo CSV
	ciudades, err := leerCSV("datos.csv")
	if err != nil {
		log.Fatalf("Error al leer el archivo CSV: %v", err)
	}

	// Obtener los continentes de las ciudades
	continentes := getContinentes(ciudades)

	// Crear el JSON de salida
	jsonBytes := createJSON(ciudades, continentes)

	// Escribir el JSON de salida a un archivo
	writeJSON(jsonBytes, "datos.json")

	// Enviar una solicitud HTTP POST al endpoint de la API
	resp, err := http.Post("http://localhost:8080/datos", "application/json", bytes.NewReader(jsonBytes))
	if err != nil {
		log.Fatalf("Error al enviar la solicitud HTTP: %v", err)
	}
	defer resp.Body.Close()

	// Leer el cuerpo de la respuesta HTTP
	bodyBytes, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatalf("Error al leer el cuerpo de la respuesta HTTP: %v", err)
	}

	// Imprimir el cuerpo de la respuesta HTTP
	fmt.Println(string(bodyBytes))
}
```

Este código lee los datos de un archivo CSV, los convierte a una estructura de datos JSON y los envía a un endpoint de una API. El código utiliza varias funciones para realizar estas tareas, incluyendo una función para leer el archivo CSV, una función para obtener los continentes de las ciudades, una función para crear el JSON de salida y una función para escribir el JSON de salida a un archivo. El código también envía una solicitud HTTP POST al endpoint de la API y lee el cuerpo de la respuesta HTTP.