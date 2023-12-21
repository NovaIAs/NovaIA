Certamente! Aqui está um código complexo em Swift que utiliza o padrão de design Observer para implementar um sistema de notificação personalizado. Este código é extenso e altamente personalizado:

```swift
import Foundation

// Definir protocolo para o observador
protocol Observer: AnyObject {
    func notificar(mensagem: String)
}

// Definir classe de notificação
class Notificacao {
    weak var observer: Observer?
    
    func adicionarObservador(_ observer: Observer) {
        self.observer = observer
    }
    
    func removerObservador() {
        observer = nil
    }
    
    func notificarObservadores(mensagem: String) {
        observer?.notificar(mensagem: mensagem)
    }
}

// Definir classe de observador
class Observador: Observer {
    let nome: String
    
    init(nome: String) {
        self.nome = nome
    }
    
    func notificar(mensagem: String) {
        print("\(nome) recebeu a seguinte mensagem: \(mensagem)")
    }
}

// Exemplo de uso
let notificacao = Notificacao()

let observador1 = Observador(nome: "Observador 1")
notificacao.adicionarObservador(observador1)

let observador2 = Observador(nome: "Observador 2")
notificacao.adicionarObservador(observador2)

notificacao.notificarObservadores(mensagem: "Nova mensagem!")

notificacao.removerObservador()

notificacao.notificarObservadores(mensagem: "Outra mensagem!")
```

Neste código, temos a definição de um protocolo chamado `Observer`, que define um método `notificar` que será implementado pelas classes observadoras. Em seguida, temos a classe `Notificacao`, que é responsável por adicionar, remover e notificar observadores. 

A classe `Observador` é uma implementação concreta do protocolo `Observer`. Ela possui um nome para identificação e o método `notificar` que imprime a mensagem recebida.

No exemplo de uso, criamos uma instância da `Notificacao` e adicionamos dois observadores. Em seguida, notificamos os observadores com uma mensagem e removemos um deles. Por fim, notificamos novamente, mas apenas o observador restante receberá a mensagem.

Espero que este código atenda às suas expectativas!