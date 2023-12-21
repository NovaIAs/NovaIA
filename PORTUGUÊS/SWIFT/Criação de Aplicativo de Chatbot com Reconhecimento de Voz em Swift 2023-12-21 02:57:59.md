Certamente, vou criar um código complexo em Swift para você. Este código será uma aplicação de chatbot em que o usuário pode interagir com o bot através de comandos de voz. A aplicação será capaz de reconhecer comandos específicos e responder adequadamente. Vou dividi-lo em três partes: configuração inicial, reconhecimento de voz e respostas.

Primeiramente, vamos fazer a configuração inicial da aplicação. É importante importar as bibliotecas necessárias e definir algumas variáveis globais:

```swift
import Foundation
import Speech

var recognitionTask: SFSpeechRecognitionTask?
var audioEngine: AVAudioEngine?
```

Agora, vamos criar a função para reconhecimento de voz:

```swift
func startRecording() throws {
    let speechRecognizer = SFSpeechRecognizer(locale: Locale(identifier: "pt-BR"))
    let recognitionRequest = SFSpeechAudioBufferRecognitionRequest()
    
    audioEngine = AVAudioEngine()
    guard let audioEngine = audioEngine else { return }
    
    let inputNode = audioEngine.inputNode
    recognitionTask = speechRecognizer?.recognitionTask(with: recognitionRequest) { result, error in
        if let result = result {
            let command = result.bestTranscription.formattedString
            handleCommand(command)
        } else if let error = error {
            print("Erro de reconhecimento de voz: \(error)")
        }
    }
    
    let recordingFormat = inputNode.outputFormat(forBus: 0)
    inputNode.installTap(onBus: 0, bufferSize: 1024, format: recordingFormat) { buffer, _ in
        recognitionRequest.append(buffer)
    }
    
    audioEngine.prepare()
    try audioEngine.start()
}
```

Nesta função, criamos um reconhecedor de fala para o idioma português do Brasil (pt-BR). Em seguida, configuramos o reconhecimento e começamos a gravar o áudio. Quando o reconhecimento é concluído, chamamos a função `handleCommand` para processar o comando recebido.

Agora, vamos implementar a função `handleCommand` para responder aos comandos recebidos:

```swift
func handleCommand(_ command: String) {
    switch command.lowercased() {
        case "olá":
            print("Olá! Como posso ajudar?")
        case "qual é a previsão do tempo?":
            print("A previsão do tempo é de sol com algumas nuvens.")
        case "tchau":
            print("Até logo!")
        default:
            print("Desculpe, não entendi o comando.")
    }
}
```

Neste exemplo, implementamos apenas algumas respostas básicas para alguns comandos específicos. Você pode adicionar mais comandos e respostas conforme necessário.

Por fim, vamos adicionar o código para iniciar a aplicação e chamar a função de gravação de voz:

```swift
let speechRecognizer = SFSpeechRecognizer(locale: Locale(identifier: "pt-BR"))
SFSpeechRecognizer.requestAuthorization { authStatus in
    switch authStatus {
    case .authorized:
        try? startRecording()
    case .denied:
        print("Permissão de reconhecimento de voz negada pelo usuário.")
    case .restricted:
        print("Reconhecimento de voz restrito neste dispositivo.")
    case .notDetermined:
        print("Permissão de reconhecimento de voz ainda não determinada.")
    @unknown default:
        fatalError("Erro desconhecido.")
    }
}
```

Este código solicita a autorização do usuário para utilizar o reconhecimento de voz. Em seguida, chama a função `startRecording` para iniciar a gravação de voz.

Esse é um exemplo básico de como criar uma aplicação de chatbot com reconhecimento de voz em Swift. Você pode expandir esse código para adicionar mais funcionalidades e personalizações.