```swift
// Importamos las bibliotecas necesarias.
import Foundation
import UIKit

// Creamos una clase llamada "ViewController". Esta clase se encargará de gestionar la interfaz gráfica de la aplicación.
class ViewController: UIViewController {

    // Creamos una variable que almacenará una referencia a la vista principal de la aplicación.
    var vistaPrincipal: UIView!

    // Creamos una variable que almacenará una referencia al botón principal de la aplicación.
    var botonPrincipal: UIButton!

    // Creamos una variable que almacenará una referencia al texto principal de la aplicación.
    var textoPrincipal: UILabel!

    // El método "viewDidLoad()" se llama cuando la vista se carga por primera vez.
    override func viewDidLoad() {
        super.viewDidLoad()

        // Creamos la vista principal de la aplicación.
        vistaPrincipal = UIView(frame: UIScreen.main.bounds)
        vistaPrincipal.backgroundColor = UIColor.white

        // Creamos el botón principal de la aplicación.
        botonPrincipal = UIButton(frame: CGRect(x: 100, y: 100, width: 100, height: 50))
        botonPrincipal.setTitle("Presioname", for: .normal)
        botonPrincipal.backgroundColor = UIColor.blue
        botonPrincipal.addTarget(self, action: #selector(botonPrincipalPulsado), for: .touchUpInside)

        // Creamos el texto principal de la aplicación.
        textoPrincipal = UILabel(frame: CGRect(x: 100, y: 200, width: 200, height: 50))
        textoPrincipal.text = "Hola mundo!"
        textoPrincipal.textAlignment = .center

        // Añadimos la vista principal, el botón principal y el texto principal a la vista actual.
        view.addSubview(vistaPrincipal)
        view.addSubview(botonPrincipal)
        view.addSubview(textoPrincipal)
    }

    // El método "botonPrincipalPulsado()" se llama cuando el botón principal es tocado.
    @objc func botonPrincipalPulsado() {
        // Cambiamos el texto del texto principal.
        textoPrincipal.text = "Hola mundo, he sido tocado!"
    }
}
```

Este código crea una aplicación muy simple que tiene un botón y un texto. Cuando el botón es tocado, el texto cambia a "Hola mundo, he sido tocado!".

El código está dividido en varias partes:

* La primera parte importa las bibliotecas necesarias.
* La segunda parte crea la clase "ViewController". Esta clase se encarga de gestionar la interfaz gráfica de la aplicación.
* El método "viewDidLoad()" se llama cuando la vista se carga por primera vez. Este método crea la vista principal de la aplicación, el botón principal y el texto principal.
* El método "botonPrincipalPulsado()" se llama cuando el botón principal es tocado. Este método cambia el texto del texto principal.