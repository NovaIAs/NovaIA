Claro! Aqui está um exemplo de um código complexo escrito em Groovy:

```groovy
import java.util.Random

def listaPessoas = [
    [nome: 'João', idade: 25, cidade: 'São Paulo'],
    [nome: 'Maria', idade: 30, cidade: 'Rio de Janeiro'],
    [nome: 'Pedro', idade: 35, cidade: 'Belo Horizonte'],
    [nome: 'Ana', idade: 28, cidade: 'Curitiba'],
    [nome: 'Carlos', idade: 40, cidade: 'Porto Alegre'],
    [nome: 'Mariana', idade: 32, cidade: 'Salvador']
]

def random = new Random()
def pessoaSorteada = listaPessoas[random.nextInt(listaPessoas.size())]

println "A pessoa sorteada é: ${pessoaSorteada.nome}"
println "Idade: ${pessoaSorteada.idade}"
println "Cidade: ${pessoaSorteada.cidade}"

if (pessoaSorteada.idade >= 30) {
    println "${pessoaSorteada.nome} já passou dos 30 anos!"
} else {
    println "${pessoaSorteada.nome} ainda é jovem!"
}

def cidades = listaPessoas.collect { it.cidade }.unique()
println "As cidades das pessoas na lista são: ${cidades}"
```

Explicação do código:
1. Primeiro, importamos a classe `Random` para gerar números aleatórios.
2. Em seguida, criamos uma lista de pessoas com seus respectivos nome, idade e cidade.
3. Utilizamos a classe `Random` para gerar um número aleatório que será usado como índice da lista para selecionar uma pessoa aleatória.
4. Imprimimos na tela o nome, idade e cidade da pessoa sorteada.
5. Verificamos se a idade da pessoa sorteada é maior ou igual a 30 e imprimimos uma mensagem correspondente.
6. Utilizamos o método `collect` para criar uma lista com todas as cidades das pessoas na lista e removemos duplicatas utilizando o método `unique`.
7. Por fim, imprimimos na tela as cidades das pessoas na lista.

Esse código seleciona aleatoriamente uma pessoa da lista `listaPessoas` e exibe informações sobre ela. Além disso, verifica se a pessoa sorteada tem idade maior ou igual a 30 e exibe uma mensagem correspondente. Por fim, mostra todas as cidades presentes na lista.