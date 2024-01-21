```swift
// Importar las bibliotecas necesarias
import Foundation
import Algorithms

// Crear una enumeración para representar los estados de un semáforo
enum EstadoSemaforo {
    case rojo
    case amarillo
    case verde
}

// Crear una clase para representar un semáforo
class Semaforo {
    // Propiedades del semáforo
    var estadoActual: EstadoSemaforo
    var tiempoRestante: Int

    // Inicializador del semáforo
    init(estadoInicial: EstadoSemaforo, tiempoInicial: Int) {
        self.estadoActual = estadoInicial
        self.tiempoRestante = tiempoInicial
    }

    // Método para cambiar el estado del semáforo
    func cambiarEstado() {
        switch estadoActual {
        case .rojo:
            estadoActual = .verde
            tiempoRestante = 30
        case .amarillo:
            estadoActual = .rojo
            tiempoRestante = 5
        case .verde:
            estadoActual = .amarillo
            tiempoRestante = 5
        }
    }

    // Método para decrementar el tiempo restante
    func decrementarTiempo() {
        tiempoRestante -= 1
    }
}

// Crear una lista de semáforos
var semaforos: [Semaforo] = []

// Añadir algunos semáforos a la lista
semaforos.append(Semaforo(estadoInicial: .rojo, tiempoInicial: 30))
semaforos.append(Semaforo(estadoInicial: .amarillo, tiempoInicial: 5))
semaforos.append(Semaforo(estadoInicial: .verde, tiempoInicial: 30))

// Crear un ciclo para simular el funcionamiento de los semáforos
while true {
    // Cambiar el estado de los semáforos
    for semaforo in semaforos {
        semaforo.cambiarEstado()
    }

    // Decrementar el tiempo restante de los semáforos
    for semaforo in semaforos {
        semaforo.decrementarTiempo()
    }

    // Imprimir el estado de los semáforos
    for semaforo in semaforos {
        print("Semáforo \(semaforo.estadoActual) con tiempo restante \(semaforo.tiempoRestante)")
    }

    // Dormir durante 1 segundo
    sleep(1)
}
```

Este código simula el funcionamiento de una lista de semáforos. La clase `Semaforo` representa un semáforo individual, con propiedades para el estado actual, el tiempo restante y métodos para cambiar el estado y decrementar el tiempo restante. La lista de semáforos se crea utilizando la clase `Array`, y el ciclo `while` simula el funcionamiento de los semáforos, cambiando su estado y decrementando el tiempo restante cada segundo. El estado actual y el tiempo restante de cada semáforo se imprimen en la consola.