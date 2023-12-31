Certamente! Aqui está um código complexo em Swift que atende aos requisitos fornecidos:

```swift
import Foundation

enum Direcao {
    case norte, sul, leste, oeste
}

class Veiculo {
    var marca: String
    var modelo: String
    var ano: Int
    var velocidade: Double
    
    init(marca: String, modelo: String, ano: Int, velocidade: Double) {
        self.marca = marca
        self.modelo = modelo
        self.ano = ano
        self.velocidade = velocidade
    }
    
    func acelerar() {
        print("O veículo \(marca) \(modelo) está acelerando.")
    }
    
    func frear() {
        print("O veículo \(marca) \(modelo) está freando.")
    }
    
    func virarPara(direcao: Direcao) {
        print("O veículo \(marca) \(modelo) está virando para \(direcao).")
    }
}

class Carro: Veiculo {
    var quilometragem: Double
    
    init(marca: String, modelo: String, ano: Int, velocidade: Double, quilometragem: Double) {
        self.quilometragem = quilometragem
        super.init(marca: marca, modelo: modelo, ano: ano, velocidade: velocidade)
    }
    
    override func acelerar() {
        super.acelerar()
        print("O carro está agora a \(velocidade) km/h.")
    }
    
    func ligarFarol() {
        print("O carro \(marca) \(modelo) está ligando o farol.")
    }
}

class Moto: Veiculo {
    var cilindrada: Int
    
    init(marca: String, modelo: String, ano: Int, velocidade: Double, cilindrada: Int) {
        self.cilindrada = cilindrada
        super.init(marca: marca, modelo: modelo, ano: ano, velocidade: velocidade)
    }
    
    override func frear() {
        super.frear()
        print("A moto \(marca) \(modelo) está agora parada.")
    }
    
    func empinar() {
        print("A moto \(marca) \(modelo) está empinando!")
    }
}

let carro = Carro(marca: "Chevrolet", modelo: "Cruze", ano: 2021, velocidade: 0, quilometragem: 5000)
carro.acelerar()
carro.ligarFarol()

let moto = Moto(marca: "Honda", modelo: "CBR600RR", ano: 2020, velocidade: 0, cilindrada: 600)
moto.frear()
moto.empinar()
```

Neste código, criamos três classes: `Veiculo`, `Carro` e `Moto`. A classe `Veiculo` é a classe base, que contém propriedades e métodos compartilhados pelos carros e motos. As classes `Carro` e `Moto` herdam da classe `Veiculo` e adicionam propriedades e métodos específicos para cada tipo de veículo.

No código, também utilizamos um `enum` chamado `Direcao` para representar as direções possíveis (norte, sul, leste e oeste).

Ao criar um objeto do tipo `Carro` ou `Moto`, podemos chamar os métodos específicos de cada classe, como `acelerar`, `frear`, `virarPara`, `ligarFarol` e `empinar`. Esses métodos exibem mensagens no console para indicar a ação realizada pelo veículo.

No exemplo dado, criamos um objeto `carro` do tipo `Carro` e um objeto `moto` do tipo `Moto`, e chamamos alguns métodos para demonstrar o funcionamento do código.

Espero que este exemplo atenda às suas expectativas! Se você tiver mais alguma solicitação ou dúvida, é só me perguntar!