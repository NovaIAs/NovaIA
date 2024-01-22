```swift
// Importamos la biblioteca UIKit para trabajar con la interfaz de usuario
import UIKit

// Definimos una clase personalizada que heredará de la clase UIView
class MiVistaPersonalizada: UIView {

    // Creamos una variable opcional de tipo UILabel para almacenar la etiqueta de texto
    var etiqueta: UILabel?

    // Creamos una variable opcional de tipo UIButton para almacenar el botón
    var boton: UIButton?

    // Sobreescribimos el método init() para inicializar la vista personalizada
    override init(frame: CGRect) {
        // Llamamos al método init() de la superclase
        super.init(frame: frame)

        // Creamos la etiqueta de texto
        etiqueta = UILabel()

        // Establecemos el texto de la etiqueta
        etiqueta?.text = "Hola mundo!"

        // Establecemos el tamaño y la posición de la etiqueta
        etiqueta?.frame = CGRect(x: 10, y: 10, width: 200, height: 20)

        // Añadimos la etiqueta a la vista personalizada
        self.addSubview(etiqueta!)

        // Creamos el botón
        boton = UIButton()

        // Establecemos el título del botón
        boton?.setTitle("Presioname", for: .normal)

        // Establecemos el tamaño y la posición del botón
        boton?.frame = CGRect(x: 100, y: 100, width: 100, height: 50)

        // Añadimos el botón a la vista personalizada
        self.addSubview(boton!)

        // Añadimos un evento de pulsación al botón
        boton?.addTarget(self, action: #selector(botonPulsado), for: .touchUpInside)
    }

    // Sobreescribimos el método required init?(coder:) para inicializar la vista personalizada
    required init?(coder aDecoder: NSCoder) {
        // Llamamos al método required init?(coder:) de la superclase
        super.init(coder: aDecoder)

        // Creamos la etiqueta de texto
        etiqueta = UILabel()

        // Establecemos el texto de la etiqueta
        etiqueta?.text = "Hola mundo!"

        // Establecemos el tamaño y la posición de la etiqueta
        etiqueta?.frame = CGRect(x: 10, y: 10, width: 200, height: 20)

        // Añadimos la etiqueta a la vista personalizada
        self.addSubview(etiqueta!)

        // Creamos el botón
        boton = UIButton()

        // Establecemos el título del botón
        boton?.setTitle("Presioname", for: .normal)

        // Establecemos el tamaño y la posición del botón
        boton?.frame = CGRect(x: 100, y: 100, width: 100, height: 50)

        // Añadimos el botón a la vista personalizada
        self.addSubview(boton!)

        // Añadimos un evento de pulsación al botón
        boton?.addTarget(self, action: #selector(botonPulsado), for: .touchUpInside)
    }

    // Definimos el método botonPulsado() para manejar el evento de pulsación del botón
    @objc func botonPulsado() {
        // Mostramos un mensaje de alerta
        let alerta = UIAlertController(title: "Alerta", message: "Has pulsado el botón", preferredStyle: .alert)

        // Añadimos un botón de aceptar a la alerta
        let botonAceptar = UIAlertAction(title: "Aceptar", style: .default, handler: nil)

        // Añadimos el botón de aceptar a la alerta
        alerta.addAction(botonAceptar)

        // Presentamos la alerta
        self.window?.rootViewController?.present(alerta, animated: true, completion: nil)
    }
}

// Creamos una clase personalizada que heredará de la clase UIViewController
class MiViewController: UIViewController {

    // Creamos una variable opcional de tipo MiVistaPersonalizada para almacenar la vista personalizada
    var vistaPersonalizada: MiVistaPersonalizada?

    // Sobreescribimos el método viewDidLoad() para inicializar la vista del controlador
    override func viewDidLoad() {
        // Llamamos al método viewDidLoad() de la superclase
        super.viewDidLoad()

        // Creamos la vista personalizada
        vistaPersonalizada = MiVistaPersonalizada(frame: self.view.frame)

        // Añadimos la vista personalizada a la vista del controlador
        self.view.addSubview(vistaPersonalizada!)
    }
}

// Creamos una instancia de la clase MiViewController y la presentamos en la pantalla
let vistaControlador = MiViewController()
vistaControlador.present(vistaControlador, animated: true, completion: nil)
```

**Explicación del código:**

* Definimos una clase personalizada `MiVistaPersonalizada` que hereda de la clase `UIView`. Esta clase es una vista personalizada que contiene una etiqueta de texto y un botón.
* En el método `init()` de la clase `MiVistaPersonalizada`, inicializamos la vista personalizada y añadimos la etiqueta de texto y el botón a la vista.
* También añadimos un evento de pulsación al botón, que mostrará un mensaje de alerta cuando se pulse.
* Definimos una clase personalizada `MiViewController` que hereda de la clase `UIViewController`. Esta clase es un controlador de vista que contiene una vista personalizada.
* En el método `viewDidLoad()` de la clase `MiViewController`, inicializamos la vista personalizada y la añadimos a la vista del controlador.
* Finalmente, creamos una instancia de la clase `MiViewController` y la presentamos en la pantalla.

Este código es complejo porque combina varias características de Swift y UIKit, como clases personalizadas, herencia, eventos de pulsación y el uso de UIAlertController para mostrar mensajes de alerta.