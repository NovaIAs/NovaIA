Claro! Aqui está um exemplo de um código complexo em Groovy que realiza uma série de tarefas diferentes:

```groovy
import java.time.LocalDate

def listaPessoas = [
    [nome: 'João', idade: 25, sexo: 'Masculino'],
    [nome: 'Maria', idade: 30, sexo: 'Feminino'],
    [nome: 'Pedro', idade: 35, sexo: 'Masculino'],
    [nome: 'Ana', idade: 28, sexo: 'Feminino']
]

def filtroIdade = { pessoa -> pessoa.idade >= 30 }
def filtroSexo = { pessoa -> pessoa.sexo == 'Feminino' }

def pessoasFiltradas = listaPessoas.findAll(filtroIdade).findAll(filtroSexo)

def dataAtual = LocalDate.now()
def anosNascimento = pessoasFiltradas.collect { dataAtual.year - it.idade }

def cumprimentos = [
    'Olá',
    'Oi',
    'Oi, tudo bem?',
    'Bom dia',
    'Boa tarde'
]

while (anosNascimento.size() > 0) {
    def index = (int) (Math.random() * cumprimentos.size())
    def cumprimento = cumprimentos[index]
    def idade = anosNascimento.remove(0)

    println "${cumprimento}, você nasceu há ${idade} anos!"
}
```

Neste exemplo, temos uma lista de pessoas com seus respectivos nomes, idades e sexos. Primeiro, definimos dois filtros: um para pessoas com idade igual ou superior a 30 anos e outro para pessoas do sexo feminino.

Em seguida, utilizamos o método `findAll` para filtrar a lista de pessoas, aplicando ambos os filtros em sequência. As pessoas que atenderem aos critérios de ambos os filtros serão armazenadas na lista `pessoasFiltradas`.

Depois, obtemos a data atual utilizando a classe `LocalDate`, e calculamos os anos de nascimento das pessoas filtradas.

A seguir, temos uma lista de cumprimentos que serão utilizados para cumprimentar as pessoas de forma aleatória. Utilizamos um laço `while` para percorrer a lista de anos de nascimento, escolher um cumprimento aleatório e imprimir a mensagem correspondente, juntamente com a idade da pessoa.

Este código realiza diversas operações diferentes, desde filtrar uma lista de pessoas até realizar cálculos de datas e exibir mensagens personalizadas.