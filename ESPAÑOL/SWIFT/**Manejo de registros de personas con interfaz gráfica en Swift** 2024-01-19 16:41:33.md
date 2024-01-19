```swift
// Importamos las bibliotecas necesarias para el programa
import Foundation
import UIKit

// Creamos una clase llamada `Persona` que contendrá los datos de cada persona
class Persona {
    var nombre: String
    var edad: Int
    var profesion: String

    init(nombre: String, edad: Int, profesion: String) {
        self.nombre = nombre
        self.edad = edad
        self.profesion = profesion
    }

    // Creamos una función para imprimir los datos de la persona
    func imprimirDatos() {
        print("Nombre: \(self.nombre)")
        print("Edad: \(self.edad)")
        print("Profesión: \(self.profesion)")
    }
}

// Creamos una clase llamada `ListaPersonas` que contendrá la lista de personas
class ListaPersonas {
    var personas: [Persona]

    init() {
        self.personas = []
    }

    // Creamos una función para añadir una persona a la lista
    func añadirPersona(persona: Persona) {
        self.personas.append(persona)
    }

    // Creamos una función para eliminar una persona de la lista
    func eliminarPersona(persona: Persona) {
        if let index = self.personas.firstIndex(of: persona) {
            self.personas.remove(at: index)
        }
    }

    // Creamos una función para imprimir la lista de personas
    func imprimirLista() {
        for persona in self.personas {
            persona.imprimirDatos()
        }
    }
}

// Creamos una clase llamada `InterfazUsuario` que se encargará de la interfaz gráfica de usuario
class InterfazUsuario {
    // Creamos un controlador de vista de tabla para mostrar la lista de personas
    var controladorVistaTabla: UITableViewController?

    // Creamos un campo de texto para introducir el nombre de la persona
    var campoTextoNombre: UITextField?

    // Creamos un campo de texto para introducir la edad de la persona
    var campoTextoEdad: UITextField?

    // Creamos un campo de texto para introducir la profesión de la persona
    var campoTextoProfesion: UITextField?

    // Creamos un botón para añadir una persona a la lista
    var botónAñadirPersona: UIButton?

    // Creamos un botón para eliminar una persona de la lista
    var botónEliminarPersona: UIButton?

    // Creamos una lista de personas
    var listaPersonas: ListaPersonas?

    // Creamos un inicializador para la interfaz de usuario
    init() {
        self.listaPersonas = ListaPersonas()
    }

    // Creamos una función para cargar la interfaz gráfica de usuario
    func cargarInterfaz() {
        // Creamos el controlador de vista de tabla
        self.controladorVistaTabla = UITableViewController()

        // Creamos el campo de texto para el nombre
        self.campoTextoNombre = UITextField()
        self.campoTextoNombre?.placeholder = "Nombre"

        // Creamos el campo de texto para la edad
        self.campoTextoEdad = UITextField()
        self.campoTextoEdad?.placeholder = "Edad"

        // Creamos el campo de texto para la profesión
        self.campoTextoProfesion = UITextField()
        self.campoTextoProfesion?.placeholder = "Profesión"

        // Creamos el botón para añadir una persona
        self.botónAñadirPersona = UIButton()
        self.botónAñadirPersona?.setTitle("Añadir persona", for: .normal)

        // Creamos el botón para eliminar una persona
        self.botónEliminarPersona = UIButton()
        self.botónEliminarPersona?.setTitle("Eliminar persona", for: .normal)

        // Añadimos los controles a la vista del controlador de vista de tabla
        self.controladorVistaTabla?.view.addSubview(self.campoTextoNombre!)
        self.controladorVistaTabla?.view.addSubview(self.campoTextoEdad!)
        self.controladorVistaTabla?.view.addSubview(self.campoTextoProfesion!)
        self.controladorVistaTabla?.view.addSubview(self.botónAñadirPersona!)
        self.controladorVistaTabla?.view.addSubview(self