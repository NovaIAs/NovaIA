Claro! Aqui está um exemplo de um código complexo em JavaScript que utiliza conceitos avançados e é explicado em detalhes:

```javascript
// Definindo a classe "Pessoa"
class Pessoa {
  constructor(nome, idade) {
    this.nome = nome;
    this.idade = idade;
  }

  saudacao() {
    console.log(`Olá, meu nome é ${this.nome} e tenho ${this.idade} anos.`);
  }
}

// Herança: criando a classe "Aluno" que herda da classe "Pessoa"
class Aluno extends Pessoa {
  constructor(nome, idade, matricula) {
    super(nome, idade);
    this.matricula = matricula;
  }

  saudacao() {
    console.log(`Oi, sou o(a) aluno(a) ${this.nome}, tenho ${this.idade} anos e minha matrícula é ${this.matricula}.`);
  }
}

// Criando uma instância da classe "Aluno"
const aluno1 = new Aluno("João", 20, "ABC123");

// Função de callback para pesquisa de alunos
function callbackPesquisa(alunos) {
  console.log("Alunos encontrados:");
  alunos.forEach(aluno => {
    aluno.saudacao();
  });
}

// Simulando uma pesquisa assíncrona de alunos
function pesquisarAlunos(nome, callback) {
  setTimeout(() => {
    const alunosEncontrados = [
      new Aluno("João", 20, "ABC123"),
      new Aluno("Maria", 22, "DEF456")
    ];
    callback(alunosEncontrados);
  }, 2000);
}

// Chamando a função de pesquisa assíncrona
pesquisarAlunos("João", callbackPesquisa);
```

Este código demonstra alguns conceitos importantes de JavaScript, como a criação de classes, herança, uso de métodos, funções de callback e até mesmo uma simulação de pesquisa assíncrona.

Primeiramente, é definida a classe "Pessoa" com um construtor que recebe o nome e idade da pessoa e atribui esses valores às propriedades correspondentes. A classe também possui um método `saudacao()` que imprime no console uma saudação personalizada.

Em seguida, é criada a classe "Aluno" que herda da classe "Pessoa". Essa classe adiciona uma nova propriedade, "matricula", e sobrescreve o método `saudacao()` para incluir a exibição da matrícula do aluno.

Após a definição das classes, é criada uma instância da classe "Aluno" chamada `aluno1`, com os valores "João", 20 e "ABC123" para nome, idade e matrícula, respectivamente.

Em seguida, é definida uma função de callback chamada `callbackPesquisa`, que recebe um array de alunos e imprime suas saudações no console.

A função `pesquisarAlunos` simula uma pesquisa assíncrona de alunos, utilizando um `setTimeout` para esperar 2 segundos antes de retornar um array de alunos encontrados. Nesse exemplo, são retornados dois alunos: "João" e "Maria", com suas respectivas informações.

Por fim, é chamada a função `pesquisarAlunos` passando o nome "João" e a função de callback `callbackPesquisa`. Após 2 segundos, a pesquisa é concluída e a função de callback é executada, exibindo as saudações dos alunos encontrados no console.

Espero que esse código complexo em JavaScript tenha atendido às suas expectativas! Se você tiver mais alguma solicitação, é só me avisar!