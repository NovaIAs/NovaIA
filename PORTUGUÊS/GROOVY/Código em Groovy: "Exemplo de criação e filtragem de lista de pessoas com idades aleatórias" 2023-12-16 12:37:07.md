Claro! Vou criar um código complexo em Groovy para você. Lembre-se de que este código é apenas um exemplo e pode não ter uma aplicação prática real. Vamos lá:

```groovy
import java.util.Random

// Classe para representar uma pessoa
class Pessoa {
    String nome
    int idade
    
    Pessoa(String nome, int idade) {
        this.nome = nome
        this.idade = idade
    }
}

// Função para gerar um número aleatório
def gerarNumeroAleatorio(int min, int max) {
    Random random = new Random()
    return min + random.nextInt(max - min + 1)
}

// Função para criar uma lista de pessoas com idades aleatórias
def criarListaDePessoasAleatorias(int quantidade) {
    List<Pessoa> pessoas = []
    
    for (int i = 0; i < quantidade; i++) {
        def idadeAleatoria = gerarNumeroAleatorio(18, 60)
        def nomeAleatorio = "Pessoa " + (i + 1)
        pessoas.add(new Pessoa(nomeAleatorio, idadeAleatoria))
    }
    
    return pessoas
}

// Função para filtrar as pessoas com idade maior que um valor específico
def filtrarPessoasPorIdade(List<Pessoa> pessoas, int idadeMinima) {
    return pessoas.findAll { pessoa -> pessoa.idade > idadeMinima }
}

// Função para imprimir as pessoas de uma lista
def imprimirPessoas(List<Pessoa> pessoas) {
    pessoas.each { pessoa -> 
        println("Nome: ${pessoa.nome}, Idade: ${pessoa.idade}")
    }
}

// Execução do código
def listaDePessoas = criarListaDePessoasAleatorias(10)
println("Lista de pessoas geradas aleatoriamente:")
imprimirPessoas(listaDePessoas)

def idadeMinimaFiltro = 30
def pessoasFiltradas = filtrarPessoasPorIdade(listaDePessoas, idadeMinimaFiltro)
println("\nLista de pessoas com idade maior que ${idadeMinimaFiltro}:")
imprimirPessoas(pessoasFiltradas)
```

Neste exemplo, criamos uma classe `Pessoa` que possui um nome e uma idade. A função `gerarNumeroAleatorio` é utilizada para gerar um número aleatório dentro de um intervalo especificado. A função `criarListaDePessoasAleatorias` cria uma lista de pessoas com idades aleatórias.

A função `filtrarPessoasPorIdade` recebe uma lista de pessoas e um valor mínimo de idade e retorna uma nova lista contendo apenas as pessoas com idade maior que o valor especificado.

Por fim, a função `imprimirPessoas` é utilizada para imprimir os dados das pessoas, e o código principal cria uma lista de pessoas aleatórias, imprime essa lista, filtra as pessoas com idade maior que 30 e imprime essa nova lista filtrada.

Espero que este exemplo tenha atendido às suas expectativas! Lembre-se de que este é apenas um exemplo e pode ser adaptado de acordo com suas necessidades específicas.