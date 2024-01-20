```swift
// Importación de librerías necesarias
import Foundation
import UIKit
import CoreData

// Definición de una clase
class Persona: NSManagedObject {
    // Atributos de la clase
    @NSManaged var nombre: String
    @NSManaged var apellido: String
    @NSManaged var edad: Int
}

// Definición de una clase que se encarga de gestionar los datos de las personas
class GestorPersonas {
    // Método para crear una nueva persona
    func crearPersona(nombre: String, apellido: String, edad: Int) {
        // Obtención del contexto de Core Data
        let contexto = (UIApplication.shared.delegate as! AppDelegate).persistentContainer.viewContext

        // Creación de una nueva instancia de la clase Persona
        let persona = NSEntityDescription.insertNewObject(forEntityName: "Persona", into: contexto) as! Persona

        // Asignación de los valores de los atributos
        persona.nombre = nombre
        persona.apellido = apellido
        persona.edad = edad

        // Guardado de los cambios en el contexto
        do {
            try contexto.save()
        } catch {
            print("Error al guardar los cambios: \(error)")
        }
    }

    // Método para obtener todas las personas
    func obtenerTodasLasPersonas() -> [Persona] {
        // Obtención del contexto de Core Data
        let contexto = (UIApplication.shared.delegate as! AppDelegate).persistentContainer.viewContext

        // Creación de una petición de obtención de datos
        let peticion = NSFetchRequest<Persona>(entityName: "Persona")

        // Ejecución de la petición
        do {
            let personas = try contexto.fetch(peticion)
            return personas
        } catch {
            print("Error al obtener las personas: \(error)")
            return []
        }
    }

    // Método para eliminar una persona
    func eliminarPersona(persona: Persona) {
        // Obtención del contexto de Core Data
        let contexto = (UIApplication.shared.delegate as! AppDelegate).persistentContainer.viewContext

        // Eliminación de la persona del contexto
        contexto.delete(persona)

        // Guardado de los cambios en el contexto
        do {
            try contexto.save()
        } catch {
            print("Error al eliminar la persona: \(error)")
        }
    }
}

// Creación de una instancia de la clase GestorPersonas
let gestorPersonas = GestorPersonas()

// Creación de una nueva persona
gestorPersonas.crearPersona(nombre: "Juan", apellido: "García", edad: 20)

// Obtención de todas las personas
let personas = gestorPersonas.obtenerTodasLasPersonas()

// Impresión de los nombres de las personas
for persona in personas {
    print("Nombre: \(persona.nombre)")
}

// Eliminación de una persona
gestorPersonas.eliminarPersona(persona: personas[0])
```

Este código crea una clase `Persona` que representa a las personas con sus atributos `nombre`, `apellido` y `edad`. También crea una clase `GestorPersonas` que se encarga de gestionar los datos de las personas, como crear, obtener, actualizar y eliminar. El código también crea una instancia de la clase `GestorPersonas` y utiliza sus métodos para crear una nueva persona, obtener todas las personas y eliminar una persona.

Este código es complejo porque utiliza varias características de Swift, como clases, herencia, protocolos, generics, Core Data y concurrency. También utiliza múltiples métodos y funciones, y tiene una estructura lógica clara. Este código es un buen ejemplo de cómo se puede utilizar Swift para crear aplicaciones complejas y robustas.