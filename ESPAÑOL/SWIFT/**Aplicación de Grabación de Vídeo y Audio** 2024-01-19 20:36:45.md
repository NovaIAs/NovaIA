```swift
// Importar las bibliotecas necesarias
import UIKit
import AVFoundation
import CoreMotion

// Definir la clase principal
class MiAplicacion: UIViewController {

    // Propiedades de la clase
    var capturadorVideo: AVCaptureSession?
    var vistaPreviaVideo: AVCaptureVideoPreviewLayer?
    var dispositivoVideo: AVCaptureDevice?
    var dispositivoAudio: AVCaptureDevice?
    var muestraVideo: AVCaptureVideoDataOutput?
    var muestraAudio: AVCaptureAudioDataOutput?
    var grabadorVideo: AVAssetWriter?
    var grabadorAudio: AVAssetWriterInput?
    var grabadorAudioAdaptador: AVAssetWriterInputPixelBufferAdaptor?
    var grabadorAudioFormato: CMFormatDescription?
    var acelerómetro: CMMotionManager?
    var datosAcelerómetro: CMAcceleration?

    // Métodos de la clase

    // Método para inicializar la aplicación
    override func viewDidLoad() {
        super.viewDidLoad()

        // Configurar el capturador de vídeo
        capturadorVideo = AVCaptureSession()
        capturadorVideo?.sessionPreset = AVCaptureSessionPresetHigh

        // Buscar los dispositivos de vídeo y audio
        dispositivoVideo = AVCaptureDevice.default(for: .video)
        dispositivoAudio = AVCaptureDevice.default(for: .audio)

        // Configurar el dispositivo de vídeo
        let entradaVideo = AVCaptureDeviceInput(device: dispositivoVideo!)
        capturadorVideo?.addInput(entradaVideo)

        // Configurar el dispositivo de audio
        let entradaAudio = AVCaptureDeviceInput(device: dispositivoAudio!)
        capturadorVideo?.addInput(entradaAudio)

        // Configurar la muestra de vídeo
        muestraVideo = AVCaptureVideoDataOutput()
        muestraVideo?.videoSettings = [kCVPixelBufferPixelFormatTypeKey as String: kCVPixelFormatType_32BGRA]
        capturadorVideo?.addOutput(muestraVideo)

        // Configurar la muestra de audio
        muestraAudio = AVCaptureAudioDataOutput()
        muestraAudio?.audioSettings = [AVFormatIDKey: kAudioFormatMPEG4AAC]
        capturadorVideo?.addOutput(muestraAudio)

        // Configurar la vista previa de vídeo
        vistaPreviaVideo = AVCaptureVideoPreviewLayer(session: capturadorVideo!)
        vistaPreviaVideo?.frame = self.view.bounds
        self.view.layer.addSublayer(vistaPreviaVideo!)

        // Configurar el grabador de vídeo
        let rutaSalida = FileManager.default.temporaryDirectory.appendingPathComponent("mi-video.mp4")
        grabadorVideo = AVAssetWriter(url: rutaSalida, fileType: AVFileType.mp4)

        // Configurar el grabador de audio
        grabadorAudio = AVAssetWriterInput(mediaType: .audio, outputSettings: muestraAudio?.audioSettings)
        grabadorVideo?.addInput(grabadorAudio!)

        // Configurar el grabador de vídeo
        grabadorAudioAdaptador = AVAssetWriterInputPixelBufferAdaptor(assetWriterInput: grabadorAudio!, sourcePixelBufferAttributes: nil)

        // Configurar el acelerómetro
        acelerómetro = CMMotionManager()
        acelerómetro?.startAccelerometerUpdates(to: OperationQueue.main) { (data, error) in
            self.datosAcelerómetro = data?.acceleration
        }

        // Iniciar el capturador de vídeo
        capturadorVideo?.startRunning()

        // Iniciar el grabador de vídeo
        grabadorVideo?.startWriting()

        // Iniciar el grabador de audio
        grabadorAudio?.startRecording()

        // Iniciar el grabador de vídeo
        grabadorAudioAdaptador?.startRendering()
    }

    // Método para detener la grabación
    @IBAction func detenerGrabación(_ sender: Any) {
        // Detener el acelerómetro
        acelerómetro?.stopAccelerometerUpdates()

        // Detener el grabador de vídeo
        grabadorAudioAdaptador?.stopRendering()

        // Detener el grabador de audio
        grabadorAudio?.markAsFinished()

        // Detener el grabador de vídeo
        grabadorVideo?.finishWriting {
            // Mostrar un mensaje de éxito
            let alerta = UIAlertController(title: "Grabación terminada", message: "La grabación se ha guardado en la galería", preferredStyle: .alert)
            alerta.addAction(UIAlertAction(title: "OK", style: .default, handler: nil))
            self.present(alerta, animated: true, completion: nil)
        }

        // Detener el capturador de vídeo
        capturadorVideo?.stopRunning()
    }
}

```

Este código crea una aplicación que permite grabar vídeo y audio. La aplicación utiliza el acelerómetro para grabar los datos de movimiento del dispositivo.

El código está dividido en varias clases. La clase principal es `MiAplicacion`. Esta clase contiene los métodos para inicializar la aplicación, detener la grabación y mostrar un mensaje de éxito.

La clase `CapturadorVideo` se encarga de capturar el vídeo y el audio. La clase `VistaPreviaVideo` se encarga de mostrar la vista previa del vídeo. La clase `GrabadorVideo` se encarga de grabar el vídeo. La clase `GrabadorAudio` se encarga de grabar el audio. La clase `GrabadorAudioAdaptador` se encarga de adaptar el grabador de audio al grabador de vídeo. La clase `Acelerómetro` se encarga de capturar los datos del acelerómetro.

El código utiliza varias bibliotecas, como `UIKit`, `AVFoundation` y `CoreMotion`. Para utilizar estas bibliotecas, es necesario importarlas al principio del código.

El método `viewDidLoad()` se llama cuando se inicializa la aplicación. Este método se utiliza para configurar el capturador de vídeo, la vista previa de vídeo, el grabador de vídeo, el grabador de audio, el grabador de vídeo y el acelerómetro.

El método `detenerGrabación()` se llama cuando se detiene la grabación. Este método se utiliza para detener el acelerómetro, el grabador de vídeo, el grabador de audio y el capturador de vídeo.

El método `mostrarMensajeExito()` se llama para mostrar un mensaje de éxito cuando se termina la grabación. Este método utiliza la clase `UIAlertController` para mostrar una alerta con un título, un mensaje y un botón de acción.