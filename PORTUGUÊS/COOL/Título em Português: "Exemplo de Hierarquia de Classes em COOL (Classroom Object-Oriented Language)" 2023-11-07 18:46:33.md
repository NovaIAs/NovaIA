O COOL (Classroom Object-Oriented Language) é uma linguagem de programação orientada a objetos simples e projetada para ser fácil de aprender e usar. Embora eu possa criar um código complexo em COOL, é importante lembrar que a linguagem em si não é extensivamente usada na indústria de software e não é tão amplamente adotada quanto outras linguagens de programação.

Aqui está um exemplo de um programa COOL que cria uma hierarquia de classes para representar animais e suas características. O código será estruturado em várias classes e incluirá herança e polimorfismo:

```cool
class Animal {
    var nome: String <- "Desconhecido"
    var idade: Int <- 0
    
    method fazerSom() : String {
        out_string("O animal faz um som.")
    }
}

class Mamifero : Animal {
    var tipoDePelagem: String <- "Desconhecido"
    
    method fazerSom() : String {
        out_string("O mamífero faz um som.")
    }
}

class Ave : Animal {
    var envergaduraDasAsas: Int <- 0
    
    method fazerSom() : String {
        out_string("A ave faz um som.")
    }
}

class Leao : Mamifero {
    var tipoDePelagem <- "Pelo curto"
    
    method fazerSom() : String {
        out_string("O leão ruge.")
    }
}

class Pinguim : Ave {
    var envergaduraDasAsas <- 70
    
    method fazerSom() : String {
        out_string("O pinguim grasna.")
    }
}

let simba: Leao <- new Leao
let pingu: Pinguim <- new Pinguim

simba.nome <- "Simba"
simba.idade <- 5

pingu.nome <- "Pingu"
pingu.idade <- 2

out_string("Nome do leão: ")
out_string(simba.nome)
out_string("\n")

out_string("Idade do leão: ")
out_int(simba.idade)
out_string("\n")

out_string("Nome do pinguim: ")
out_string(pingu.nome)
out_string("\n")

out_string("Idade do pinguim: ")
out_int(pingu.idade)
out_string("\n")

simba.fazerSom()
out_string("\n")

pingu.fazerSom()
```

Neste código COOL, criamos uma hierarquia de classes para representar animais. Cada classe herda da classe anterior e substitui o método `fazerSom()` para fornecer uma implementação específica. Depois, criamos instâncias de `Leao` e `Pinguim`, atribuímos valores aos atributos e chamamos o método `fazerSom()` para cada instância.

Espero que este exemplo ajude a ilustrar o uso de COOL em um contexto de programação orientada a objetos. Por favor, note que COOL é uma linguagem acadêmica e não é amplamente usada na indústria.