```swift

// Importamos Cocoa para la interfaz de usuario
import Cocoa

// Definimos la clase principal de la aplicación
class AppDelegate: NSResponder, NSApplicationDelegate {

    // La ventana principal de la aplicación
    @IBOutlet var window: NSWindow!

    // La vista principal de la ventana
    @IBOutlet var view: NSView!

    // Creamos una referencia al controlador de dibujo
    var renderer: Renderer!

    // Función que se ejecuta cuando se inicia la aplicación
    func applicationDidFinishLaunching(_ notification: Notification) {

        // Creamos el controlador de dibujo
        renderer = Renderer(view: view)

        // Cargamos la imagen original
        let originalImage = NSImage(named: "original")!

        // Creamos una imagen procesada
        let processedImage = renderer.process(image: originalImage)

        // Mostramos la imagen procesada en la vista
        view.image = processedImage
    }
}

// Definimos la clase del controlador de dibujo
class Renderer {

    // La vista en la que dibujaremos
    var view: NSView!

    // Creamos un contexto de dibujo
    var context: NSGraphicsContext!

    // El filtro que aplicaremos a la imagen
    var filter: CIFilter!

    // Creamos un diccionario de filtros
    let filters: [String: CIFilter] = [

        // Filtro de desenfoque gaussiano
        "Gaussian Blur": CIFilter(name: "CIGaussianBlur")!,

        // Filtro de sepia
        "Sepia Tone": CIFilter(name: "CISepiaTone")!,

        // Filtro de noir
        "Noir": CIFilter(name: "CIPhotoEffectNoir")!
    ]

    // Creamos una variable para almacenar el filtro actual
    var currentFilter: String = "Gaussian Blur"

    // Inicializamos el controlador de dibujo
    init(view: NSView) {

        // Asignamos la vista
        self.view = view

        // Creamos el contexto de dibujo
        context = NSGraphicsContext(view: view)!

        // Obtenemos el filtro actual del diccionario
        filter = filters[currentFilter]!
    }

    // Función que procesa una imagen y aplica un filtro
    func process(image: NSImage) -> NSImage {

        // Convertimos la imagen en una imagen CIImage
        let ciImage = CIImage(cgImage: image.cgImage!)

        // Aplicamos el filtro a la imagen
        filter.setValue(ciImage, forKey: "inputImage")

        // Obtenemos la imagen procesada
        let outputImage = filter.outputImage!

        // Convertimos la imagen CIImage en una NSImage
        let nsImage = NSImage(ciImage: outputImage, size: image.size)

        // Devolvemos la imagen procesada
        return nsImage
    }

    // Función que cambia el filtro actual
    func setFilter(name: String) {

        // Obtenemos el filtro del diccionario
        filter = filters[name]!

        // Aplicamos el filtro a la imagen actual
        let processedImage = process(image: view.image!)

        // Mostramos la imagen procesada en la vista
        view.image = processedImage
    }
}

```

Explicación:

* **Importamos Cocoa:** Importamos la biblioteca Cocoa, que proporciona clases y funciones para crear interfaces de usuario en macOS.
* **Definimos la clase principal de la aplicación:** La clase AppDelegate es la clase principal de la aplicación. Es responsable de crear la ventana principal de la aplicación y de cargar la vista principal.
* **Creamos una referencia al controlador de dibujo:** Creamos una referencia al controlador de dibujo, que es una clase que se encarga de procesar y mostrar imágenes en la vista principal.
* **Función que se ejecuta cuando se inicia la aplicación:** La función applicationDidFinishLaunching se ejecuta cuando se inicia la aplicación. En esta función, creamos el controlador de dibujo, cargamos la imagen original y mostramos la imagen procesada en la vista.
* **Definimos la clase del controlador de dibujo:** La clase Renderer es la clase que se encarga de procesar y mostrar imágenes en la vista principal.
* **Creamos un contexto de dibujo:** Creamos un contexto de dibujo, que es un objeto que permite dibujar en la vista.
* **El filtro que aplicaremos a la imagen:** Creamos una variable filter para almacenar el filtro que aplicaremos a la imagen.
* **Creamos un diccionario de filtros:** Creamos un diccionario de filtros, que contiene los filtros disponibles y sus nombres.
* **Creamos una variable para almacenar el filtro actual:** Creamos una variable currentFilter para almacenar el filtro actual.
* **Inicializamos el controlador de dibujo:** En el método init, inicializamos el controlador de dibujo asignando la vista, creando el contexto de dibujo y obteniendo el filtro actual del diccionario.
* **Función que procesa una imagen y aplica un filtro:** La función process procesa una imagen y aplica un filtro. Primero, convertimos la imagen en una imagen CIImage. A continuación, aplicamos el filtro a la imagen y obtenemos la imagen procesada. Finalmente, convertimos la imagen