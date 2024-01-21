```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/joho/godotenv"
)

// Esta estructura representa un registro de temperatura.
type Temperatura struct {
	Fecha      string  `json:"fecha"`
	Hora       string  `json:"hora"`
	Temperatura float64 `json:"temperatura"`
}

// Esta estructura representa la respuesta del servicio web.
type Respuesta struct {
	Codigo    int    `json:"codigo"`
	Mensaje   string `json:"mensaje"`
	Datos     []Temperatura `json:"datos"`
}

// Esta función carga el archivo .env con las variables de entorno.
func cargarVariablesDeEntorno(archivo string) error {
	err := godotenv.Load(archivo)
	if err != nil {
		return fmt.Errorf("error al cargar las variables de entorno: %w", err)
	}
	return nil
}

// Esta función valida si una cadena de caracteres es un número entero.
func esEntero(s string) bool {
	_, err := strconv.Atoi(s)
	return err == nil
}

// Esta función valida si una cadena de caracteres es un número de punto flotante.
func esFlotante(s string) bool {
	_, err := strconv.ParseFloat(s, 64)
	return err == nil
}

// Esta función valida si una cadena de caracteres es una fecha en formato "dd-mm-aaaa".
func esFecha(s string) bool {
	_, err := time.Parse("02-01-2006", s)
	return err == nil
}

// Esta función valida si una cadena de caracteres es una hora en formato "hh:mm:ss".
func esHora(s string) bool {
	_, err := time.Parse("15:04:05", s)
	return err == nil
}

// Esta función valida si una cadena de caracteres es una temperatura en formato "número de punto flotante".
func esTemperatura(s string) bool {
	return esFlotante(s) && strings.Contains(s, ".")
}

// Esta función valida los parámetros de la solicitud HTTP.
func validarParametros(c *gin.Context) (string, string, string, error) {
	fecha := c.Query("fecha")
	hora := c.Query("hora")
	temperatura := c.Query("temperatura")

	if fecha == "" || hora == "" || temperatura == "" {
		return "", "", "", fmt.Errorf("los parámetros fecha, hora y temperatura son obligatorios")
	}

	if !esFecha(fecha) {
		return "", "", "", fmt.Errorf("el formato de la fecha es inválido. El formato correcto es dd-mm-aaaa")
	}

	if !esHora(hora) {
		return "", "", "", fmt.Errorf("el formato de la hora es inválido. El formato correcto es hh:mm:ss")
	}

	if !esTemperatura(temperatura) {
		return "", "", "", fmt.Errorf("el formato de la temperatura es inválido. El formato correcto es un número de punto flotante")
	}

	return fecha, hora, temperatura, nil
}

// Esta función guarda un registro de temperatura en el archivo de datos.
func guardarRegistro(fecha string, hora string, temperatura float64) error {
	archivo := os.Getenv("ARCHIVO_DATOS")

	f, err := os.OpenFile(archivo, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return fmt.Errorf("error al abrir el archivo de datos: %w", err)
	}
	defer f.Close()

	_, err = f.WriteString(fmt.Sprintf("%s,%s,%f\n", fecha, hora, temperatura))
	if err != nil {
		return fmt.Errorf("error al escribir en el archivo de datos: %w", err)
	}

	return nil
}

// Esta función obtiene los registros de temperatura del archivo de datos.
func obtenerRegistros(fechaInicial string, fechaFinal string) ([]Temperatura, error) {
	archivo := os.Getenv("ARCHIVO_DATOS")

	f, err := os.Open(archivo)
	if err != nil {
		return nil, fmt.Errorf("error al abrir el archivo de datos: %w", err)
	}
	defer f.Close()

	var registros []Temperatura

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ",")
		fecha, hora, temperatura := parts[0], parts[1], parts[2]

		if fechaInicial <= fecha && fecha <= fechaFinal {
			floatTemperatura, err := strconv.ParseFloat(temperatura, 64)
			if err != nil {
				log.Printf("error al convertir la temperatura a número: %v", err)
				continue
			}

			registros = append(registros, Temperatura{Fecha: fecha, Hora: hora, Temperatura: floatTemperatura})