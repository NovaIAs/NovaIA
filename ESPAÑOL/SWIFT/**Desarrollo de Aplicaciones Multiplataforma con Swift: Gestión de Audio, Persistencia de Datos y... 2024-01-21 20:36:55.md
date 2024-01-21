```swift
// Importación de bibliotecas necesarias
import UIKit
import AVFoundation
import CoreData
import Alamofire
import Firebase

// Definición de una clase para gestionar la interfaz de usuario
class ViewController: UIViewController {
    
    // Declaración de variables y objetos
    var reproductorAudio: AVAudioPlayer?
    var contextoCoreData: NSManagedObjectContext?
    var administradorPeticionesAlamofire: SessionManager?
    var referenciaFirebase: DatabaseReference?
    
    // Método que se ejecuta al cargar la vista
    override func viewDidLoad() {
        super.viewDidLoad()
        
        // Inicialización de variables y objetos
        reproductorAudio = try? AVAudioPlayer(contentsOf: URL(fileURLWithPath: Bundle.main.path(forResource: "musica", ofType: "mp3")!))
        contextoCoreData = (UIApplication.shared.delegate as! AppDelegate).persistentContainer.viewContext
        administradorPeticionesAlamofire = Alamofire.SessionManager.default
        referenciaFirebase = Database.database().reference()
        
        // Reproducción de audio
        reproductorAudio?.play()
        
        // Almacenamiento de datos en CoreData
        let nuevaEntidad = NSEntityDescription.insertNewObject(forEntityName: "Entidad", into: contextoCoreData!)
        nuevaEntidad.setValue("Valor", forKey: "clave")
        try? contextoCoreData?.save()
        
        // Realización de petición HTTP a una API externa
        administradorPeticionesAlamofire?.request("https://jsonplaceholder.typicode.com/posts").responseJSON { response in
            if let datos = response.result.value as? [[String: Any]] {
                for dato in datos {
                    print(dato["title"]!)
                }
            }
        }
        
        // Almacenamiento de datos en Firebase
        referenciaFirebase.child("mensajes").childByAutoId().setValue(["texto": "Hola, mundo!"])
    }
}
```

Explicación del código:

* El código importa las bibliotecas necesarias para el funcionamiento de la aplicación (AVFoundation para la reproducción de audio, CoreData para la persistencia de datos, Alamofire para la realización de peticiones HTTP y Firebase para el almacenamiento de datos en la nube).
* Se define una clase `ViewController` que extiende de `UIViewController`, la cual es la clase base para todas las vistas en iOS.
* En el método `viewDidLoad`, se inicializan las variables y objetos necesarios para el funcionamiento de la aplicación (reproductor de audio, contexto de CoreData, administrador de peticiones Alamofire y referencia a Firebase).
* Se reproduce el audio mediante el reproductor de audio.
* Se almacena un nuevo objeto en CoreData.
* Se realiza una petición HTTP a una API externa mediante Alamofire y se imprimen los títulos de los posts obtenidos en la consola.
* Se almacena un nuevo mensaje en Firebase.