Claro! Aqui está um exemplo de um código complexo em Swift que envolve a manipulação de dados usando classes, estruturas e protocolos:

```
// Definindo um protocolo chamado "Imprimivel"
protocol Imprimivel {
    func imprimir()
}

// Definindo uma classe chamada "Pessoa" que implementa o protocolo "Imprimivel"
class Pessoa: Imprimivel {
    var nome: String
    var idade: Int
    
    init(nome: String, idade: Int) {
        self.nome = nome
        self.idade = idade
    }
    
    func imprimir() {
        print("Nome: \(nome), Idade: \(idade)")
    }
}

// Definindo uma estrutura chamada "Retangulo" que implementa o protocolo "Imprimivel"
struct Retangulo: Imprimivel {
    var largura: Double
    var altura: Double
    
    func imprimir() {
        print("Largura: \(largura), Altura: \(altura)")
    }
}

// Criando uma função genérica que recebe um objeto que implementa o protocolo "Imprimivel" e chama o método "imprimir"
func imprimirObjeto<T: Imprimivel>(objeto: T) {
    objeto.imprimir()
}

// Criando uma instância de Pessoa e uma instância de Retangulo
let pessoa = Pessoa(nome: "João", idade: 30)
let retangulo = Retangulo(largura: 10.0, altura: 5.0)

// Chamando a função "imprimirObjeto" com a instância de Pessoa e a instância de Retangulo
imprimirObjeto(objeto: pessoa)
imprimirObjeto(objeto: retangulo)
```

Neste código, temos a definição de um protocolo chamado "Imprimivel" que define um método chamado "imprimir". Em seguida, temos uma classe chamada "Pessoa" e uma estrutura chamada "Retangulo", ambas implementando o protocolo "Imprimivel" e provendo uma implementação para o método "imprimir".

Em seguida, temos uma função genérica chamada "imprimirObjeto" que recebe um objeto que implementa o protocolo "Imprimivel" e chama o método "imprimir" desse objeto.

Por fim, criamos uma instância de Pessoa e uma instância de Retangulo e chamamos a função "imprimirObjeto" passando essas instâncias como argumentos.

O resultado da execução desse código será a impressão das informações da Pessoa e do Retangulo no console.