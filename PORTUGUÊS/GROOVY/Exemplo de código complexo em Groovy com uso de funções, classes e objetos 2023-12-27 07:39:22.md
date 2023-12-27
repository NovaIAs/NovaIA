Claro! Abaixo está um exemplo de código complexo em Groovy, que utiliza diversas funcionalidades da linguagem. Vou explicar cada parte do código para facilitar o entendimento:

```groovy
import groovy.json.JsonSlurper

// Definindo uma função recursiva para calcular o fatorial de um número
def fatorial(num) {
    if (num <= 1) {
        return 1
    } else {
        return num * fatorial(num - 1)
    }
}

// Classe que representa uma pessoa
class Pessoa {
    String nome
    int idade
    
    // Construtor da classe
    Pessoa(String nome, int idade) {
        this.nome = nome
        this.idade = idade
    }
    
    // Método para exibir os dados da pessoa
    void exibirDados() {
        println("Nome: ${nome}")
        println("Idade: ${idade}")
    }
}

// Função para ler um arquivo JSON e retornar o conteúdo como um objeto
def lerJson(arquivo) {
    def slurper = new JsonSlurper()
    def conteudo = slurper.parseText(new File(arquivo).text)
    return conteudo
}

// Exemplo de utilização das funções e classes definidas acima
def numero = 5
println("O fatorial de ${numero} é ${fatorial(numero)}")

def pessoa1 = new Pessoa("João", 25)
pessoa1.exibirDados()

def pessoa2 = new Pessoa("Maria", 30)
pessoa2.exibirDados()

def arquivoJson = "dados.json"
def conteudoJson = lerJson(arquivoJson)
println(conteudoJson)
```

Explicação do código:

1. A primeira linha importa a classe `JsonSlurper` do pacote `groovy.json`, que será utilizada posteriormente para ler um arquivo JSON.
2. Em seguida, definimos uma função chamada `fatorial`, que recebe um número como parâmetro e calcula o fatorial desse número de forma recursiva. A função utiliza uma estrutura condicional (`if-else`) para verificar se o número é menor ou igual a 1, retornando 1 nesse caso, ou calculando o fatorial do número multiplicado pelo fatorial do número anterior.
3. Na sequência, é definida a classe `Pessoa`, que possui dois atributos: `nome` e `idade`. A classe também possui um construtor que recebe os valores desses atributos e um método chamado `exibirDados`, que exibe o nome e a idade da pessoa.
4. A função `lerJson` recebe o nome de um arquivo JSON como parâmetro, utiliza o `JsonSlurper` para ler o conteúdo desse arquivo e retorna o conteúdo convertido para um objeto.
5. Por fim, é feito um exemplo de utilização das funções e classes definidas anteriormente. É calculado o fatorial do número 5 e exibido na tela. Em seguida, são criados dois objetos da classe `Pessoa`, com diferentes nomes e idades, e os dados dessas pessoas são exibidos. Por fim, é lido o conteúdo de um arquivo JSON chamado "dados.json" e é exibido na tela.

Esse código tem como objetivo demonstrar o uso de funções, classes e objetos em Groovy, além de utilizar a biblioteca `groovy.json` para ler um arquivo JSON. Cada parte do código foi explicada individualmente para facilitar o entendimento.