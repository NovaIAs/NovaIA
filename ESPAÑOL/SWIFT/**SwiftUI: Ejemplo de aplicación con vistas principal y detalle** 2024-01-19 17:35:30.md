```swift
import SwiftUI

struct Contenido: View {
    @State private var texto = ""
    @State private var número = 0
    @State private var colores = [Color.rojo, Color.azul, Color.verde]

    var cuerpo: some View {
        VStack {
            TextField("Escribe algo", texto: $texto).padding()

            Text("Escribiste: \(texto)").padding()

            Button(action: { número += 1 }) {
                Text("Suma uno al número")
            }.padding()

            Text("El número es \(número)").padding()

            Picker("Elige un color", selección: $colores[0]) {
                ForEach(colores, id: \.self) { color in
                    Text(color.description).tag(color)
                }
            }.padding()

            Text("El color elegido es \(colores[0].description)").padding()

            NavigationLink(destination: Detalle(texto: $texto, número: $número, colores: $colores)) {
                Text("Ir a la siguiente vista")
            }.padding()
        }.navigationTitle("Vista principal")
    }
}

struct Detalle: View {
    @Binding var texto: String
    @Binding var número: Int
    @Binding var colores: [Color]

    var cuerpo: some View {
        VStack {
            Text("El texto es \(texto)").padding()

            Text("El número es \(número)").padding()

            Text("El color elegido es \(colores[0].description)").padding()

            Button(action: { texto = "Texto modificado" }) {
                Text("Modificar el texto")
            }.padding()

            Button(action: { número += 1 }) {
                Text("Suma uno al número")
            }.padding()

            Button(action: { colores[0] = .naranja }) {
                Text("Cambiar el color")
            }.padding()

            NavigationLink("Volver a la vista principal") {
                Contenido()
            }.padding()
        }.navigationTitle("Vista detalle")
    }
}

@main
struct MiAplicación: App {
    var cuerpo: some Scene {
        WindowGroup {
            Contenido()
        }
    }
}
```
Este código crea una aplicación SwiftUI que consta de dos vistas, una vista principal y una vista de detalle. La vista principal tiene un campo de texto, un botón para aumentar un número, un selector de color y un botón para navegar a la vista de detalle. La vista de detalle muestra el texto, el número y el color elegidos en la vista principal. También tiene botones para modificar el texto, aumentar el número y cambiar el color.

El código utiliza las siguientes técnicas de SwiftUI:

* **Estado**: Las variables de estado `@State` se utilizan para almacenar los datos que pueden cambiar con el tiempo, como el texto, el número y el color.
* **Controles de entrada**: El campo de texto, el botón y el selector de color son controles de entrada que permiten al usuario interactuar con la aplicación.
* **Navegación**: El enlace de navegación se utiliza para navegar entre la vista principal y la vista de detalle.
* **Vinculación de datos**: La vinculación de datos se utiliza para sincronizar los datos entre las vistas principal y de detalle.

Este código es un ejemplo complejo de cómo utilizar SwiftUI para crear una aplicación con múltiples vistas y controles de entrada.