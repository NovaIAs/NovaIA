```swift
// Importamos las bibliotecas necesarias.
import UIKit
import SceneKit
import CoreMotion

// Creamos una nueva aplicación.
class MiAplicación: UIViewController {

    // Creamos una nueva escena.
    let escena = SCNScene()

    // Creamos una nueva cámara.
    let cámara = SCNCamera()

    // Creamos un nuevo nodo para la cámara.
    let nodoCámara = SCNNode()

    // Creamos un nuevo nodo para la luz.
    let nodoLuz = SCNNode()

    // Creamos un nuevo nodo para la caja.
    let nodoCaja = SCNNode()

    // Creamos un nuevo nodo para la esfera.
    let nodoEsfera = SCNNode()

    // Creamos un nuevo nodo para el cilindro.
    let nodoCilindro = SCNNode()

    // Creamos un nuevo nodo para el cono.
    let nodoCono = SCNNode()

    // Creamos un nuevo nodo para la pirámide.
    let nodoPirámide = SCNNode()

    // Creamos un nuevo nodo para el torus.
    let nodoTorus = SCNNode()

    // Creamos un nuevo nodo para el plano.
    let nodoPlano = SCNNode()

    // Creamos un nuevo nodo para el cubo.
    let nodoCubo = SCNNode()

    // Creamos un nuevo nodo para el texto.
    let nodoTexto = SCNNode()

    // Creamos un nuevo nodo para el vídeo.
    let nodoVídeo = SCNNode()

    // Creamos un nuevo nodo para el audio.
    let nodoAudio = SCNNode()

    // Creamos un nuevo nodo para el control de movimiento.
    let nodoControlMovimiento = SCNNode()

    // Creamos un nuevo nodo para el control de gestos.
    let nodoControlGestos = SCNNode()

    // Creamos un nuevo nodo para la interfaz de usuario.
    let nodoInterfazUsuario = SCNNode()

    // Creamos una nueva vista para la escena.
    let vistaEscena = SCNView()

    // Creamos un nuevo controlador de movimiento.
    let controladorMovimiento = CMMotionManager()

    // Creamos un nuevo controlador de gestos.
    let controladorGestos = UIGestureRecognizer()

    // Creamos un nuevo controlador de interfaz de usuario.
    let controladorInterfazUsuario = UIViewController()

    // Creamos una nueva función para cargar la escena.
    func cargarEscena() {

        // Añadimos la cámara al nodo de la cámara.
        nodoCámara.camera = cámara

        // Posicionamos la cámara.
        nodoCámara.position = SCNVector3(x: 0, y: 0, z: 10)

        // Añadimos la luz al nodo de la luz.
        nodoLuz.light = SCNLight()

        // Posicionamos la luz.
        nodoLuz.position = SCNVector3(x: 0, y: 10, z: 0)

        // Añadimos la caja al nodo de la caja.
        nodoCaja.geometry = SCNBox(width: 1, height: 1, length: 1)

        // Posicionamos la caja.
        nodoCaja.position = SCNVector3(x: 0, y: 0, z: 0)

        // Añadimos la esfera al nodo de la esfera.
        nodoEsfera.geometry = SCNSphere(radius: 1)

        // Posicionamos la esfera.
        nodoEsfera.position = SCNVector3(x: 1, y: 0, z: 0)

        // Añadimos el cilindro al nodo del cilindro.
        nodoCilindro.geometry = SCNCylinder(radius: 1, height: 2)

        // Posicionamos el cilindro.
        nodoCilindro.position = SCNVector3(x: 2, y: 0, z: 0)

        // Añadimos el cono al nodo del cono.
        nodoCono.geometry = SCNCone(topRadius: 0, bottomRadius: 2, height: 2)

        // Posicionamos el cono.
        nodoCono.position = SCNVector3(x: 3, y: 0, z: 0)

        // Añadimos la pirámide al nodo de la pirámide.
        nodoPirámide.geometry = SCNPyramid(width: 2, height: 2, length: 2)

        // Posicionamos la pirámide.
        nodoPirámide.position = SCNVector3(x: 4, y: 0, z: 0)

        // Añadimos el torus al nodo del torus.
        nodoTorus.geometry = SCNTorus(ringRadius: 2, pipeRadius: 1)

        // Posicionamos el torus.
        nodoTorus.position = SCNVector3(x: 5, y: 0, z: 0)

        // Añadimos el plano al nodo del plano.
        nodoPlano.geometry = SCNPlane(width: 10, height: 10)

        // Posicionamos el plano.
        nodoPlano.position = SCNVector3(x: 0, y: -1, z: 0)

        // Añadimos el cubo al nodo del cubo.
        nodoCubo.geometry = SCNBox(width: 1, height: 1, length: 1)

        // Posicionamos el cubo.
        nodoCubo.position = SCNVector3(x: 0, y: 1, z: 0)

        // Añadimos el texto al nodo del texto.
        nodoTexto.geometry = SCNText(string: "Hola Mundo", extrusionDepth: 0.1)

        // Posicionamos el texto.
        nodoTexto.position = SCNVector3(x: 0, y: 2, z: 0)

        // Añadimos el vídeo al nodo del vídeo.
        nodoVídeo.geometry = SCNVideo(url: URL(fileURLWithPath: "video.mp4"))

        // Posicionamos el vídeo.
        nodoVídeo.position = SCNVector3(x: 0, y: 3, z: 0)

        // Añadimos el audio al nodo del audio.
        nodoAudio.geometry = SCNAudioSource(url: URL(fileURLWithPath: "audio.mp3"))

        // Posicionamos el audio.
        nodoAudio.position = SCNVector3(x: 0, y: 4, z: 0)

        // Añadimos el control de movimiento al nodo del control de movimiento.
        nodoControlMovimiento.geometry = SCNBox(width: 1, height: 1, length: 1)

        // Posicionamos el control de movimiento.
        nodoControlMovimiento.position = SCNVector3(x: 0, y: 5, z: 0)

        // Añadimos el control de gestos al nodo del control de gestos.
        nodoControlGestos.geometry = SCNBox(width: 1, height: 1, length: 1)

        // Posicionamos el control de gestos.
        nodoControlGestos.position = SCNVector3(x: 0, y: 6, z: 0)

        // Añadimos la interfaz de usuario al nodo de la interfaz de usuario.
        nodoInterfazUsuario.geometry = SCNBox(width: 1, height: 1, length: 1)

        // Posicionamos la interfaz de usuario.
        nodoInterfazUsuario.position = SCNVector3(x: 0, y: 7, z: 0)

        // Añadimos todos los nodos a la escena.
        escena.rootNode.addChildNode(nodoCámara)
        escena.rootNode.addChildNode(nodoLuz)
        escena.rootNode.addChildNode(nodoCaja)
        escena.rootNode.addChildNode(nodoEsfera)
        escena.rootNode.addChildNode(nodoCilindro)
        escena.rootNode.addChildNode(nodoCono)
        escena.rootNode.addChildNode(nodoPirámide)
        escena.rootNode.addChildNode(nodoTorus)
        escena.rootNode.addChildNode(nodoPlano)
        escena.rootNode.addChildNode(nodoCubo)
        escena.rootNode.addChildNode(nodoTexto)
        escena.rootNode.addChildNode(nodoVídeo)
        escena.rootNode.addChildNode(nodoAudio)
        escena.rootNode.addChildNode(nodoControlMovimiento)
        escena.rootNode.addChildNode(nodoControlGestos)
        escena.rootNode.addChildNode(nodoInterfazUsuario)

        // Añadimos la vista de la escena a la vista principal.
        vistaEscena.scene = escena

        // Añadimos la vista de la escena a la vista del controlador.
        view.addSubview(vistaEscena)

        // Iniciamos el controlador de movimiento.
        controladorMovimiento.startDeviceMotionUpdates(to: OperationQueue.main) { (datos, error) in

            // Obtenemos la orientación del dispositivo.
            let orientación = datos?.attitude

            // Actualizamos la posición de la cámara.
            nodoCámara.position = SCNVector3(x: orientación!.roll