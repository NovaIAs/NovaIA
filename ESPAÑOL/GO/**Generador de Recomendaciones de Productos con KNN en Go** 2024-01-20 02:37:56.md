```go
// Objetivo: crear un código complejo en GO que sea difícil de repetir.

// Este código implementa un sistema de recomendación de productos basado en un algoritmo de aprendizaje automático.
// El sistema utiliza el algoritmo K-Vecinos Cercanos (K-NN) para recomendar productos a los usuarios en función de sus preferencias anteriores.

package main

import (
	"fmt"
	"log"

	"github.com/google/go-github/v32/github"
)

// Producto representa un producto que se puede recomendar a los usuarios.
type Producto struct {
	ID    int64
	Nombre string
	Precio float64
}

// Usuario representa a un usuario que puede recibir recomendaciones de productos.
type Usuario struct {
	ID           int64
	Nombre       string
	ProductosComprados []int64
}

// SistemaRecomendacion es el sistema que realiza las recomendaciones de productos.
type SistemaRecomendacion struct {
	Productos   []Producto
	Usuarios     []Usuario
	AlgoritmoKNN *KNN
}

// KNN es un algoritmo de aprendizaje automático que se utiliza para realizar recomendaciones de productos.
type KNN struct {
	K int
}

// Entrenar entrena el algoritmo KNN con los datos de los usuarios y los productos.
func (knn *KNN) Entrenar(productos []Producto, usuarios []Usuario) {
	// Crear una matriz de distancias entre los usuarios.
	distancias := make([][]float64, len(usuarios))
	for i, usuario1 := range usuarios {
		distancias[i] = make([]float64, len(usuarios))
		for j, usuario2 := range usuarios {
			distancias[i][j] = knn.Distancia(usuario1, usuario2)
		}
	}

	// Almacenar los K vecinos más cercanos de cada usuario.
	vecinos := make([][]int, len(usuarios))
	for i, usuario := range usuarios {
		vecinos[i] = knn.VecinosCercanos(distancias[i], k)
	}

	// Almacenar los pesos de los vecinos de cada usuario.
	pesos := make([][]float64, len(usuarios))
	for i, usuario := range usuarios {
		pesos[i] = knn.PesosVecinos(distancias[i], vecinos[i])
	}

	// Almacenar los datos del entrenamiento.
	knn.Datos = struct {
		Distancias [][]float64
		Vecinos   [][]int
		Pesos    [][]float64
	}{
		Distancias: distancias,
		Vecinos:   vecinos,
		Pesos:    pesos,
	}
}

// Distancia calcula la distancia entre dos usuarios.
func (knn *KNN) Distancia(usuario1, usuario2 Usuario) float64 {
	// Calcular la distancia euclidiana entre los usuarios.
	var distancia float64
	for _, producto1 := range usuario1.ProductosComprados {
		for _, producto2 := range usuario2.ProductosComprados {
			distancia += (productos[producto1].Precio - productos[producto2].Precio) * (productos[producto1].Precio - productos[producto2].Precio)
		}
	}
	return distancia
}

// VecinosCercanos devuelve los K vecinos más cercanos de un usuario.
func (knn *KNN) VecinosCercanos(distancias []float64, k int) []int {
	// Ordenar las distancias en orden ascendente.
	sort.Slice(distancias, func(i, j int) bool {
		return distancias[i] < distancias[j]
	})

	// Devolver los K vecinos más cercanos.
	return distancias[:k]
}

// PesosVecinos devuelve los pesos de los vecinos de un usuario.
func (knn *KNN) PesosVecinos(distancias []float64, vecinos []int) []float64 {
	// Calcular los pesos de los vecinos.
	pesos := make([]float64, len(vecinos))
	for i, vecino := range vecinos {
		pesos[i] = 1 / distancias[vecino]
	}

	// Normalizar los pesos.
	sumaPesos := 0.0
	for _, peso := range pesos {
		sumaPesos += peso
	}
	for i, peso := range pesos {
		pesos[i] /= sumaPesos
	}

	// Devolver los pesos de los vecinos.
	return pesos
}

// RecomendarProductos recomienda productos a un usuario.
func (sistema *SistemaRecomendacion) RecomendarProductos(usuario Usuario) []Producto {
	// Obtener los K vecinos más cercanos del usuario.
	vecinos := sistema.AlgoritmoKNN.VecinosCercanos(sistema.AlgoritmoKNN.Datos.Distancias[usuario.ID], sistema.AlgoritmoKNN.K)

	// Obtener los pesos de los vecinos del usuario.
	pesos := sistema.AlgoritmoKNN.PesosVecinos(sistema.AlgoritmoKNN.Datos.Distancias[usuario.ID], vecinos)

	// Obtener los productos recomendados para el usuario.
	recomendaciones := make([]Producto, 0)
	for i, vecino := range vecinos {
		for _, producto := range sistema.Productos {
			if !contiene(usuario.ProductosComprados, producto.ID) {
				recomendaciones = append(recomendaciones, Producto{
					ID:    producto.ID,
					Nombre: producto.Nombre,
					Precio: producto.Precio * pesos[i],
				})
			}
		}
	}

	// Ordenar las recomendaciones por precio.
	sort.Slice(recomendaciones, func(i, j int) bool {
		return recomendaciones[i].Precio < recomendaciones[j].Precio
	})

	// Devolver las recomendaciones.
	return recomendaciones
}

// contiene comprueba si un elemento está contenido en una lista.
func contiene(lista []int64, elemento int64) bool {
	for _, i := range lista {
		if i == elemento {
			return true
		}
	}
	return false
}

func main() {
	// Crear los productos.
	productos := []Producto{
		{ID: 1, Nombre: "Camiseta", Precio: 10.0},
		{ID: 2, Nombre: "Pantalón", Precio: 20.0},
		{ID: 3, Nombre: "Zapatillas", Precio: 30.0},
		{ID: 4, Nombre: "Mochila", Precio: 40.0},
		{ID: 5, Nombre: "Reloj", Precio: 50.0},
	}

	// Crear los usuarios.
	usuarios := []Usuario{
		{ID: 1, Nombre: "Juan", ProductosComprados: []int64{1, 2}},
		{ID: 2, Nombre: "María", ProductosComprados: []int64{2, 3}},
		{ID: 3, Nombre: "Pedro", ProductosComprados: []int64{3, 4}},
		{ID: 4, Nombre: "Ana", ProductosComprados: []int64{4, 5}},
	}

	// Crear el sistema de recomendación.
	sistema := SistemaRecomendacion{
		Productos:   productos,
		Usuarios:     usuarios,
		AlgoritmoKNN: &KNN{K: 3},
	}

	// Entrenar el sistema de recomendación.
	sistema.AlgoritmoKNN.Entrenar(productos, usuarios)

	// Obtener las recomendaciones para un usuario.
	recomendaciones := sistema.RecomendarProductos(usuarios[0])

	// Mostrar las recomendaciones.
	for _,