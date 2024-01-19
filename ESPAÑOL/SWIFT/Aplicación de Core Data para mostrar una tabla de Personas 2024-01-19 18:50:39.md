```swift
// Importamos las librerías que vamos a utilizar
import Foundation
import UIKit
import CoreData

// Definimos una clase llamada "Persona" que representará los datos de una persona en nuestra aplicación.
class Persona: NSManagedObject {
    // Definimos las propiedades de la clase "Persona" que son mapeadas a las columnas de la tabla "Persona" en nuestra base de datos.
    @NSManaged var nombre: String?
    @NSManaged var apellido: String?
    @NSManaged var edad: Int16
}

// Definimos una clase llamada "ViewController" que representará la interfaz de usuario de nuestra aplicación.
class ViewController: UIViewController {

    // Definimos un contexto de Core Data que será utilizado para interactuar con la base de datos.
    let contexto = (UIApplication.shared.delegate as! AppDelegate).persistentContainer.viewContext
    
    // Definimos un arreglo llamado "personas" que contendrá las instancias de la clase "Persona" que se muestran en nuestra interfaz de usuario.
    var personas: [Persona] = []

    // Definimos un objeto de tipo UITableView que representará nuestra tabla de datos.
    var tablaDatos: UITableView!

    // Definimos un objeto de tipo UIBarButtonItem que representará el botón de "Agregar Persona".
    var botonAgregarPersona: UIBarButtonItem!

    // Definimos una función llamada "viewDidLoad" que se ejecuta cuando la interfaz de usuario se carga.
    override func viewDidLoad() {
        super.viewDidLoad()

        // Creamos el botón de "Agregar Persona" y lo añadimos a la barra de navegación.
        botonAgregarPersona = UIBarButtonItem(barButtonSystemItem: .add, target: self, action: #selector(agregarPersona))
        navigationItem.rightBarButtonItem = botonAgregarPersona

        // Creamos la tabla de datos y la agregamos a la vista.
        tablaDatos = UITableView(frame: view.bounds, style: .plain)
        tablaDatos.dataSource = self
        tablaDatos.delegate = self
        view.addSubview(tablaDatos)

        // Obtenemos las personas de la base de datos y las añadimos al arreglo "personas".
        do {
            personas = try contexto.fetch(Persona.fetchRequest())
        } catch {
            print("Error al obtener las personas de la base de datos: \(error)")
        }
    }

    // Definimos una función llamada "agregarPersona" que se ejecuta cuando el botón de "Agregar Persona" es pulsado.
    @objc func agregarPersona() {
        // Creamos una nueva instancia de la clase "Persona".
        let persona = Persona(context: contexto)

        // Establecemos los valores de las propiedades de la nueva instancia de la clase "Persona".
        persona.nombre = "Juan"
        persona.apellido = "Pérez"
        persona.edad = 20

        // Guardamos los cambios en la base de datos.
        do {
            try contexto.save()
        } catch {
            print("Error al guardar los cambios en la base de datos: \(error)")
        }

        // Actualizamos el arreglo "personas" y recargamos la tabla de datos.
        personas.append(persona)
        tablaDatos.reloadData()
    }
}

// Definimos la extensión de la clase "ViewController" que implementa los métodos de los protocolos UITableViewDataSource y UITableViewDelegate.
extension ViewController: UITableViewDataSource, UITableViewDelegate {

    // Definimos el número de filas en la tabla de datos.
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return personas.count
    }

    // Definimos la celda para cada fila en la tabla de datos.
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        // Obtenemos la persona correspondiente a la fila actual.
        let persona = personas[indexPath.row]

        // Creamos una nueva celda de tabla.
        let celda = UITableViewCell(style: .default, reuseIdentifier: "Celda")

        // Establecemos el texto de la celda con el nombre y el apellido de la persona.
        celda.textLabel?.text = "\(persona.nombre!) \(persona.apellido!)"

        // Devolvemos la celda.
        return celda
    }

    // Definimos la acción que se ejecuta cuando una fila de la tabla de datos es pulsada.
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        // Obtenemos la persona correspondiente a la fila actual.
        let persona = personas[indexPath.row]

        // Mostramos un mensaje con el nombre y el apellido de la persona.
        let mensaje = "Nombre: \(persona.nombre!)\nApellido: \(persona.apellido!)"
        let alerta = UIAlertController(title: "Persona", message: mensaje, preferredStyle: .alert)
        alerta.addAction(UIAlertAction(title: "OK", style: .default, handler: nil))
        present(alerta, animated: true, completion: nil)
    }
}
```

Este código crea una aplicación sencilla en Swift que muestra una tabla de datos con una lista de personas. La aplicación permite agregar nuevas personas a la tabla de datos y mostrar un mensaje con el nombre y el apellido de la persona cuando se pulsa una fila de la tabla de datos.

El código está dividido en dos clases:

* Clase "Persona": Esta clase representa los datos de una persona en nuestra aplicación. Las propiedades de esta clase están mapeadas a las columnas de la tabla "Persona" en nuestra base de datos.
* Clase "ViewController": Esta clase representa la interfaz de usuario de nuestra aplicación. Esta clase contiene el código para crear la tabla de datos, agregar nuevas personas a la tabla de datos y mostrar un mensaje con el nombre y el apellido de la persona cuando se pulsa una fila de la tabla de datos.

El código utiliza Core Data para interactuar con la base de datos. Core Data es una tecnología de persistencia de datos que permite a los desarrolladores de iOS almacenar y recuperar datos de una base de datos de forma eficiente y segura.