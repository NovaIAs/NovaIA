```swift
// Importamos las bibliotecas necesarias.
import UIKit
import CoreLocation
import MapKit

// Definimos una clase para crear un mapa personalizado.
class MapaPersonalizado: MKMapView {

    // Sobrescribimos el método `drawMapRect:` para personalizar el mapa.
    override func drawMapRect(_ mapRect: MKMapRect, zoomScale: MKZoomScale, in context: CGContext) {
        // Obtenemos el tamaño del contexto gráfico.
        let contextSize = context.size

        // Creamos un rectángulo que cubra todo el contexto gráfico.
        let fullRect = CGRect(x: 0, y: 0, width: contextSize.width, height: contextSize.height)

        // Creamos un rectángulo que represente el área del mapa que queremos dibujar.
        let mapRect = self.convert(mapRect, toRectTo: self)

        // Dibujamos el mapa en el contexto gráfico.
        self.drawMapRect(mapRect, zoomScale: zoomScale, in: context)

        // Dibujamos un círculo rojo en el centro del mapa.
        context.setFillColor(UIColor.red.cgColor)
        context.fillEllipse(in: CGRect(x: contextSize.width / 2 - 5, y: contextSize.height / 2 - 5, width: 10, height: 10))
    }
}

// Definimos una clase para crear una vista de tabla personalizada.
class TablaPersonalizada: UITableView {

    // Sobrescribimos el método `drawRect:` para personalizar la vista de tabla.
    override func drawRect(_ rect: CGRect) {
        // Obtenemos el tamaño del contexto gráfico.
        let contextSize = self.bounds.size

        // Creamos un rectángulo que cubra todo el contexto gráfico.
        let fullRect = CGRect(x: 0, y: 0, width: contextSize.width, height: contextSize.height)

        // Dibujamos el fondo de la vista de tabla en color azul claro.
        context.setFillColor(UIColor(red: 0.9, green: 0.9, blue: 1.0, alpha: 1.0).cgColor)
        context.fill(fullRect)

        // Dibujamos las líneas de separación de las celdas en color gris claro.
        context.setStrokeColor(UIColor(red: 0.8, green: 0.8, blue: 0.8, alpha: 1.0).cgColor)
        context.setLineWidth(1.0)
        for i in 1...self.numberOfSections {
            let sectionRect = self.rect(forSection: i)
            context.move(to: CGPoint(x: 0, y: sectionRect.minY))
            context.addLine(to: CGPoint(x: contextSize.width, y: sectionRect.minY))
            context.strokePath()
        }

        // Dibujamos los separadores de las celdas en color gris oscuro.
        context.setStrokeColor(UIColor(red: 0.6, green: 0.6, blue: 0.6, alpha: 1.0).cgColor)
        context.setLineWidth(2.0)
        for i in 0..<self.numberOfRows(inSection: 0) {
            let cellRect = self.rectForRow(at: IndexPath(row: i, section: 0))
            context.move(to: CGPoint(x: 0, y: cellRect.minY))
            context.addLine(to: CGPoint(x: contextSize.width, y: cellRect.minY))
            context.strokePath()
        }

        // Llamamos al método `drawRect:` de la superclase para dibujar el contenido de la vista de tabla.
        super.drawRect(rect)
    }
}

// Creamos un controlador de vista para la aplicación.
class ViewController: UIViewController {

    // Declaramos las propiedades del controlador de vista.
    private let mapa: MapaPersonalizado = MapaPersonalizado()
    private let tabla: TablaPersonalizada = TablaPersonalizada()
    private let datos: [String] = ["Elemento 1", "Elemento 2", "Elemento 3", "Elemento 4", "Elemento 5"]

    // Sobrescribimos el método `viewDidLoad()` para inicializar la interfaz de usuario.
    override func viewDidLoad() {
        super.viewDidLoad()

        // Añadimos el mapa a la vista del controlador de vista.
        self.view.addSubview(self.mapa)

        // Añadimos la tabla a la vista del controlador de vista.
        self.view.addSubview(self.tabla)

        // Configuramos el delegado y el origen de datos de la tabla.
        self.tabla.delegate = self
        self.tabla.dataSource = self

        // Centramos el mapa en la ciudad de Madrid.
        let madrid = CLLocationCoordinate2D(latitude: 40.416775, longitude: -3.703790)
        let region = MKCoordinateRegion(center: madrid, span: MKCoordinateSpan(latitudeDelta: 0.01, longitudeDelta: 0.01))
        self.mapa.setRegion(region, animated: true)

        // Añadimos una anotación al mapa.
        let anotacion = MKPointAnnotation()
        anotacion.coordinate = madrid
        anotacion.title = "Madrid"
        self.mapa.addAnnotation(anotacion)
    }

    // Sobrescribimos el método `viewDidLayoutSubviews()` para ajustar el tamaño del mapa y de la tabla.
    override func viewDidLayoutSubviews() {
        super.viewDidLayoutSubviews()

        // Ajustamos el tamaño del mapa.
        self.mapa.frame = CGRect(x: 0, y: 0, width: self.view.bounds.width, height: self.view.bounds.height / 2)

        // Ajustamos el tamaño de la tabla.
        self.tabla.frame = CGRect(x: 0, y: self.view.bounds.height / 2, width: self.view.bounds.width, height: self.view.bounds.height / 2)
    }
}

// Extendemos la clase `ViewController` para implementar el protocolo `UITableViewDelegate`.
extension ViewController: UITableViewDelegate {

    // Sobrescribimos el método `tableView(_:heightForRowAt:)` para especificar la altura de las celdas de la tabla.
    func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        return 50