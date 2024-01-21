```swift
// Importación de bibliotecas necesarias
import Foundation
import UIKit

// Definición de un protocolo para manejar eventos de la interfaz de usuario
protocol MiProtocolo {
    func eventoOcurrido(sender: UIButton)
}

// Definición de una clase que implementa MiProtocolo y contiene la interfaz de usuario
class MiClase: UIView, MiProtocolo {
    // Variable para almacenar el delegado de MiProtocolo
    var delegado: MiProtocolo?

    // Método para manejar el evento de pulsación de un botón
    func eventoOcurrido(sender: UIButton) {
        // Llamada al método eventoOcurrido del delegado
        delegado?.eventoOcurrido(sender: sender)
    }
}

// Definición de una clase que utiliza MiClase y gestiona el evento de pulsación de un botón
class MiControlador: UIViewController {
    // Variables para almacenar la vista y el botón
    var vista: MiClase!
    var botón: UIButton!

    // Método que se ejecuta al cargar la vista
    override func viewDidLoad() {
        super.viewDidLoad()

        // Creación de la vista y el botón
        vista = MiClase(frame: view.bounds)
        botón = UIButton(frame: CGRect(x: 100, y: 100, width: 100, height: 50))
        botón.setTitle("Pulsar", for: .normal)

        // Adición de la vista y el botón a la vista del controlador
        view.addSubview(vista)
        view.addSubview(botón)

        // Establecimiento del delegado de MiProtocolo
        vista.delegado = self

        // Asignación de la acción del botón
        botón.addTarget(self, action: #selector(botónPulsado), for: .touchUpInside)
    }

    // Método que se ejecuta cuando se pulsa el botón
    @objc func botónPulsado(sender: UIButton) {
        // Llamada al método eventoOcurrido de la vista
        vista.eventoOcurrido(sender: sender)
    }

    // Método que implementa el protocolo MiProtocolo
    func eventoOcurrido(sender: UIButton) {
        // Impresión de un mensaje en la consola
        print("Botón pulsado")
    }
}

// Creación de una instancia de MiControlador y lanzamiento de la aplicación
let controlador = MiControlador()
UIApplication.shared.windows.first?.rootViewController = controlador
```

Explicación del código:

* Se importan las bibliotecas necesarias para el desarrollo de la interfaz de usuario.
* Se define un protocolo `MiProtocolo` que contiene un método `eventoOcurrido` que será llamado cuando se produzca un evento en la interfaz de usuario.
* Se define una clase `MiClase` que implementa el protocolo `MiProtocolo` y contiene la interfaz de usuario. Esta clase incluye una variable `delegado` de tipo `MiProtocolo?` que almacenará el delegado de `MiProtocolo`.
* Se define una clase `MiControlador` que utiliza `MiClase` y gestiona el evento de pulsación de un botón. Esta clase incluye variables para almacenar la vista y el botón, así como métodos para manejar el evento de pulsación del botón.
* En el método `viewDidLoad` de `MiControlador`, se crean la vista y el botón, se añaden a la vista del controlador y se establece el delegado de `MiProtocolo`.
* Se asigna la acción del botón para que llame al método `botónPulsado` de `MiControlador`.
* El método `botónPulsado` de `MiControlador` llama al método `eventoOcurrido` de la vista, pasando el botón pulsado como argumento.
* El método `eventoOcurrido` de `MiClase` imprime un mensaje en la consola.
* Se crea una instancia de `MiControlador` y se lanza la aplicación.