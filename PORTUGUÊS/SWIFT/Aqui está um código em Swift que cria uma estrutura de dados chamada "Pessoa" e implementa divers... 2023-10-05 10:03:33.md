Claro! Aqui está um código em Swift que cria uma estrutura de dados chamada "Pessoa" e implementa várias funcionalidades relacionadas a ela:

```swift
struct Pessoa {
    var nome: String
    var idade: Int
    var endereco: String
    
    func apresentar() {
        print("Olá, meu nome é \(nome). Eu tenho \(idade) anos e moro em \(endereco).")
    }
    
    mutating func envelhecer() {
        idade += 1
        print("\(nome) agora tem \(idade) anos.")
    }
    
    static func compararIdades(pessoa1: Pessoa, pessoa2: Pessoa) -> String {
        if pessoa1.idade > pessoa2.idade {
            return "\(pessoa1.nome) é mais velho(a) do que \(pessoa2.nome)."
        } else if pessoa1.idade < pessoa2.idade {
            return "\(pessoa1.nome) é mais novo(a) do que \(pessoa2.nome)."
        } else {
            return "\(pessoa1.nome) e \(pessoa2.nome) têm a mesma idade."
        }
    }
}

var pessoa1 = Pessoa(nome: "João", idade: 25, endereco: "Rua A")
var pessoa2 = Pessoa(nome: "Maria", idade: 30, endereco: "Rua B")

pessoa1.apresentar() // Saída: Olá, meu nome é João. Eu tenho 25 anos e moro em Rua A.
pessoa2.apresentar() // Saída: Olá, meu nome é Maria. Eu tenho 30 anos e moro em Rua B.

pessoa1.envelhecer() // Saída: João agora tem 26 anos.

let comparacaoIdades = Pessoa.compararIdades(pessoa1: pessoa1, pessoa2: pessoa2)
print(comparacaoIdades) // Saída: João é mais novo(a) do que Maria.
```

Neste código, definimos uma estrutura de dados chamada "Pessoa" que possui três propriedades: nome, idade e endereço. A estrutura também possui três métodos. O método "apresentar()" exibe uma mensagem com o nome, idade e endereço da pessoa. O método "envelhecer()" incrementa a idade da pessoa em 1 e exibe uma mensagem informando a nova idade. O método está marcado como "mutating" porque ele modifica a propriedade "idade" da estrutura. O método estático "compararIdades(pessoa1:pessoa2:)" compara as idades de duas pessoas e retorna uma string indicando qual delas é mais nova, mais velha ou se têm a mesma idade.

No exemplo final, criamos duas instâncias da estrutura "Pessoa" (pessoa1 e pessoa2) e chamamos os métodos "apresentar()", "envelhecer()" e "compararIdades()".