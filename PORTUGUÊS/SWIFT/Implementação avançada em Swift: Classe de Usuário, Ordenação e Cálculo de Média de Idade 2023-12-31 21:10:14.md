Certamente! Aqui está um exemplo de código em Swift que usa conceitos avançados de programação e implementa uma funcionalidade complexa:

```swift
import Foundation

// Definindo uma classe para representar um objeto de usuário
class Usuario {
    var nome: String
    var idade: Int
    
    init(nome: String, idade: Int) {
        self.nome = nome
        self.idade = idade
    }
    
    func cumprimentar() {
        print("Olá, meu nome é \(self.nome)!")
    }
}

// Definindo uma função para ordenar uma lista de usuários por idade
func ordenarUsuariosPorIdade(usuarios: [Usuario]) -> [Usuario] {
    return usuarios.sorted { $0.idade < $1.idade }
}

// Definindo uma extensão para a classe Array que adiciona um método para retornar a média de idades dos usuários
extension Array where Element == Usuario {
    func calcularMediaDeIdade() -> Double {
        let totalIdades = self.reduce(0) { $0 + $1.idade }
        return Double(totalIdades) / Double(self.count)
    }
}

// Criando alguns usuários de exemplo
let usuario1 = Usuario(nome: "João", idade: 25)
let usuario2 = Usuario(nome: "Maria", idade: 30)
let usuario3 = Usuario(nome: "Pedro", idade: 20)

// Criando uma lista de usuários
let usuarios = [usuario1, usuario2, usuario3]

// Ordenando a lista de usuários por idade e imprimindo o resultado
let usuariosOrdenadosPorIdade = ordenarUsuariosPorIdade(usuarios: usuarios)
print("Usuários ordenados por idade:")
for usuario in usuariosOrdenadosPorIdade {
    usuario.cumprimentar()
}

// Calculando a média de idade dos usuários e imprimindo o resultado
let mediaDeIdade = usuarios.calcularMediaDeIdade()
print("Média de idade dos usuários: \(mediaDeIdade)")
```

Neste código, temos uma classe `Usuario` que representa um objeto de usuário com propriedades como nome e idade. A classe também possui um método `cumprimentar()` que imprime uma mensagem de cumprimento com o nome do usuário.

Temos também uma função `ordenarUsuariosPorIdade()` que recebe uma lista de usuários e retorna a mesma lista ordenada por idade.

Além disso, adicionamos uma extensão para a classe `Array` que adiciona um método `calcularMediaDeIdade()` para calcular a média de idades dos usuários em uma lista de usuários.

Por fim, criamos alguns usuários de exemplo, ordenamos a lista de usuários por idade e calculamos a média de idade dos usuários. Os resultados são impressos no console.

Espero que este código atenda às suas expectativas de complexidade e diferenciação!