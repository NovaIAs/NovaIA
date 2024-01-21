**Tarea:** Crear un programa que genere un laberinto aleatorio y permita al usuario resolverlo.

**Código:**

```
import UIKit
import GameplayKit

class Laberinto: GKGraph {

    // Tamaño del laberinto
    var ancho: Int
    var alto: Int

    // Celdas del laberinto
    var celdas: [[Celda]]

    // Posición inicial y final
    var inicio: CGPoint
    var fin: CGPoint

    // Inicializar el laberinto
    init(ancho: Int, alto: Int) {
        self.ancho = ancho
        self.alto = alto

        // Crear las celdas del laberinto
        celdas = [[Celda]](repeating: [Celda](repeating: Celda(), count: ancho), count: alto)

        // Generar el laberinto
        generarLaberinto()

        // Establecer la posición inicial y final
        inicio = CGPoint(x: 0, y: 0)
        fin = CGPoint(x: ancho - 1, y: alto - 1)
    }

    // Generar el laberinto
    func generarLaberinto() {
        // Crear un generador de números aleatorios
        let generador = GKRandomSource()

        // Crear una pila para almacenar las celdas visitadas
        var pila = [Celda]()

        // Añadir la primera celda a la pila
        pila.append(celdas[0][0])

        // Mientras haya celdas en la pila
        while !pila.isEmpty {
            // Obtener la celda actual de la pila
            let celdaActual = pila.removeLast()

            // Marcar la celda actual como visitada
            celdaActual.visitada = true

            // Obtener las celdas adyacentes
            let celdasAdyacentes = getCeldasAdyacentes(celdaActual)

            // Barajar las celdas adyacentes
            celdasAdyacentes.shuffle(using: generador)

            // Para cada celda adyacente
            for celdaAdyacente in celdasAdyacentes {
                // Si la celda adyacente no ha sido visitada
                if !celdaAdyacente.visitada {
                    // Añadir la celda adyacente a la pila
                    pila.append(celdaAdyacente)

                    // Crear un muro entre la celda actual y la celda adyacente
                    celdaActual.muros[getDireccion(celdaActual, celdaAdyacente)] = true
                    celdaAdyacente.muros[getDireccion(celdaAdyacente, celdaActual)] = true
                }
            }
        }
    }

    // Obtener las celdas adyacentes a una celda
    func getCeldasAdyacentes(_ celda: Celda) -> [Celda] {
        var celdasAdyacentes = [Celda]()

        // Obtener la posición de la celda
        let x = Int(celda.posicion.x)
        let y = Int(celda.posicion.y)

        // Comprobar si hay una celda adyacente a la izquierda
        if x > 0 {
            celdasAdyacentes.append(celdas[y][x - 1])
        }

        // Comprobar si hay una celda adyacente a la derecha
        if x < ancho - 1 {
            celdasAdyacentes.append(celdas[y][x + 1])
        }

        // Comprobar si hay una celda adyacente arriba
        if y > 0 {
            celdasAdyacentes.append(celdas[y - 1][x])
        }

        // Comprobar si hay una celda adyacente abajo
        if y < alto - 1 {
            celdasAdyacentes.append(celdas[y + 1][x])
        }

        return celdasAdyacentes
    }

    // Obtener la dirección entre dos celdas
    func getDireccion(_ celda1: Celda, _ celda2: Celda) -> Direccion {
        let x1 = Int(celda1.posicion.x)
        let y1 = Int(celda1.posicion.y)
        let x2 = Int(celda2.posicion.x)
        let y2 = Int(celda2.posicion.y)

        if x1 == x2 {
            return y1 > y2 ? .arriba : .abajo
        } else {
            return x1 > x2 ? .izquierda : .derecha