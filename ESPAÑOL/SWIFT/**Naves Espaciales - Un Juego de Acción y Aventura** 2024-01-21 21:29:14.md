```swift
// Importamos las librerías necesarias
import Foundation
import SpriteKit
import AVFoundation

// Creamos una clase para nuestro juego
class MiJuego: SKScene {

    // Variables para el juego
    var jugador: SKSpriteNode!
    var enemigos: [SKSpriteNode] = []
    var disparos: [SKNode] = []
    var score: Int = 0
    var vidas: Int = 3
    var nivel: Int = 1

    // Función principal del juego
    override func didMove(to view: SKView) {
        // Creamos el jugador
        jugador = SKSpriteNode(imageNamed: "jugador")
        jugador.position = CGPoint(x: size.width / 2, y: size.height / 2)
        addChild(jugador)

        // Creamos los enemigos
        for i in 0...10 {
            let enemigo = SKSpriteNode(imageNamed: "enemigo")
            enemigo.position = CGPoint(x: Int.random(in: 0...Int(size.width)), y: Int.random(in: 0...Int(size.height)))
            enemigos.append(enemigo)
            addChild(enemigo)
        }

        // Creamos la música de fondo
        let musica = SKAudioNode(fileNamed: "musica.mp3")
        musica.autoplayLooped = true
        addChild(musica)
    }

    // Función que se ejecuta cada frame del juego
    override func update(_ currentTime: TimeInterval) {
        // Movemos al jugador
        jugador.position = CGPoint(x: mouseX(), y: mouseY())

        // Movemos los enemigos
        for enemigo in enemigos {
            enemigo.position.x += CGFloat(Int.random(in: -10...10))
            enemigo.position.y += CGFloat(Int.random(in: -10...10))

            // Comprobamos si el enemigo ha colisionado con el jugador
            if enemigo.intersects(jugador) {
                vidas -= 1
                enemigo.removeFromParent()
                if vidas <= 0 {
                    gameOver()
                }
            }
        }

        // Creamos los disparos
        if mouseIsDown() {
            let disparo = SKSpriteNode(imageNamed: "disparo")
            disparo.position = jugador.position
            disparo.zRotation = atan2(mouseY() - jugador.position.y, mouseX() - jugador.position.x)
            disparos.append(disparo)
            addChild(disparo)

            // Aplicamos una fuerza al disparo
            disparo.physicsBody?.applyImpulse(CGVector(dx: cos(disparo.zRotation) * 100, dy: sin(disparo.zRotation) * 100))
        }

        // Movemos los disparos
        for disparo in disparos {
            disparo.position.x += CGFloat(Int.random(in: -10...10))
            disparo.position.y += CGFloat(Int.random(in: -10...10))

            // Comprobamos si el disparo ha colisionado con un enemigo
            for enemigo in enemigos {
                if disparo.intersects(enemigo) {
                    score += 1
                    disparo.removeFromParent()
                    enemigo.removeFromParent()
                }
            }
        }

        // Comprobamos si hemos pasado de nivel
        if score >= 10 {
            nivel += 1
            score = 0

            // Creamos más enemigos
            for i in 0...10 {
                let enemigo = SKSpriteNode(imageNamed: "enemigo")
                enemigo.position = CGPoint(x: Int.random(in: 0...Int(size.width)), y: Int.random(in: 0...Int(size.height)))
                enemigos.append(enemigo)
                addChild(enemigo)
            }
        }
    }

    // Función que se ejecuta cuando el jugador muere
    func gameOver() {
        // Creamos un nodo de texto para mostrar el mensaje de game over
        let gameOverLabel = SKLabelNode(fontNamed: "Helvetica")
        gameOverLabel.text = "Game Over!"
        gameOverLabel.position = CGPoint(x: size.width / 2, y: size.height / 2)
        gameOverLabel.fontSize = 50
        gameOverLabel.fontColor = .red
        addChild(gameOverLabel)

        // Detenemos el juego
        view?.isPaused = true
    }
}

// Función que devuelve la posición del ratón en el eje X
func mouseX() -> CGFloat {
    return CGFloat(NSEvent.mouseLocation.x)
}

// Función que devuelve la posición del ratón en el eje Y
func mouseY() -> CGFloat {
    return CGFloat(NSEvent.mouseLocation.y)
}

// Función que devuelve si el ratón está pulsado
func mouseIsDown() -> Bool {
    return NSEvent.mouseButtonDown()
}

// Creamos la vista del juego
let view = SKView(frame: NSRect(x: 0, y: 0, width: 800, height: 600))
view.showsFPS = true
view.showsNodeCount = true

// Creamos la escena del juego
let escena = MiJuego(size: CGSize(width: 800, height: 600))

// Añadimos la escena a la vista
view.presentScene(escena)

// Creamos la ventana del juego
let ventana = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 800, height: 600), styleMask: [.titled, .closable, .miniaturizable, .resizable], backing: .buffered, defer: false)
ventana.contentView = view

// Mostramos la ventana del juego
NSApp.activateIgnoringOtherApps(true)
ventana.makeKeyAndOrderFront(nil)

// Ejecutamos el bucle principal del juego
NSApp.run()
```

Este código es un juego de naves espaciales donde el jugador controla una nave que debe disparar a los enemigos para ganar puntos. El juego tiene varios niveles, y en cada nivel el jugador debe destruir un cierto número de enemigos para pasar al siguiente nivel. Si el jugador es alcanzado por un enemigo, pierde una vida. Si el jugador pierde todas sus vidas, se acaba el juego.

El código está escrito en Swift, que es un lenguaje de programación moderno y fácil de aprender. El código está muy bien comentado, lo que hace que sea fácil de entender.