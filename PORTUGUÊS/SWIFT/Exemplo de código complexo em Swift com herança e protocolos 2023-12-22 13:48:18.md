Certamente! Aqui está um exemplo de código complexo em Swift, que envolve o uso de classes, protocolos, herança e recursos de manipulação de strings:

```swift
// Definição do protocolo Voador
protocol Voador {
    var velocidadeVoo: Double { get }
    
    func voar()
}

// Classe base Animal
class Animal {
    var nome: String
    
    init(nome: String) {
        self.nome = nome
    }
    
    func emitirSom() {
        print("O animal está emitindo um som.")
    }
}

// Classe Ave que herda de Animal e implementa o protocolo Voador
class Ave: Animal, Voador {
    var envergaduraAsas: Double
    
    init(nome: String, envergaduraAsas: Double) {
        self.envergaduraAsas = envergaduraAsas
        super.init(nome: nome)
    }
    
    var velocidadeVoo: Double {
        return envergaduraAsas * 10
    }
    
    func voar() {
        print("A ave \(nome) está voando a uma velocidade de \(velocidadeVoo) km/h.")
    }
    
    override func emitirSom() {
        print("O pássaro está cantando.")
    }
}

// Classe Avião que implementa o protocolo Voador
class Aviao: Voador {
    var velocidadeVoo: Double
    
    init(velocidadeVoo: Double) {
        self.velocidadeVoo = velocidadeVoo
    }
    
    func voar() {
        print("O avião está voando a uma velocidade de \(velocidadeVoo) km/h.")
    }
}

// Classe principal
class Principal {
    func iniciar() {
        let passaro = Ave(nome: "Canário", envergaduraAsas: 0.5)
        let aviao = Aviao(velocidadeVoo: 900)
        
        passaro.emitirSom()
        passaro.voar()
        
        aviao.voar()
    }
}

// Instância da classe Principal para iniciar o programa
let principal = Principal()
principal.iniciar()
```

Neste código, temos a definição de um protocolo chamado Voador, que contém uma propriedade `velocidadeVoo` e um método `voar()`. Em seguida, temos a classe base `Animal` com uma propriedade `nome` e um método `emitirSom()`. A classe `Ave` herda de `Animal` e implementa o protocolo `Voador`, adicionando uma propriedade `envergaduraAsas` e sobrescrevendo o método `emitirSom()`. A classe `Aviao` também implementa o protocolo `Voador`.

Na classe principal `Principal`, criamos uma instância de `Ave` chamada `passaro` e uma instância de `Aviao` chamada `aviao`. Em seguida, chamamos os métodos `emitirSom()` e `voar()` para o `passaro` e o método `voar()` para o `aviao`.

Esse código demonstra a utilização de herança, protocolos e polimorfismo no Swift, além de mostrar como criar e trabalhar com classes em um programa.