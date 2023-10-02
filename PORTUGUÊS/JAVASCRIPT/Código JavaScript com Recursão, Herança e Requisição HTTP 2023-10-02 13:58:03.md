Claro! Aqui está um exemplo de um código complexo em JavaScript, que utiliza algumas funcionalidades avançadas da linguagem:

```javascript
// Função para calcular o fatorial de um número utilizando recursão
function fatorial(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return n * fatorial(n - 1);
  }
}

// Classe Pessoa
class Pessoa {
  constructor(nome, idade) {
    this.nome = nome;
    this.idade = idade;
  }

  saudacao() {
    console.log(`Olá, meu nome é ${this.nome} e tenho ${this.idade} anos.`);
  }
}

// Classe Aluno que estende a classe Pessoa
class Aluno extends Pessoa {
  constructor(nome, idade, curso) {
    super(nome, idade);
    this.curso = curso;
  }

  saudacao() {
    console.log(`Olá, meu nome é ${this.nome}, tenho ${this.idade} anos e estou cursando ${this.curso}.`);
  }
}

// Função assíncrona que realiza uma requisição HTTP
async function fazerRequisicao(url) {
  try {
    const resposta = await fetch(url);
    const dados = await resposta.json();
    console.log(dados);
  } catch (erro) {
    console.error('Erro na requisição:', erro);
  }
}

// Utilização das funções e classes definidas acima
const numero = 5;
console.log(`O fatorial de ${numero} é ${fatorial(numero)}.`);

const pessoa = new Pessoa('João', 30);
pessoa.saudacao();

const aluno = new Aluno('Maria', 25, 'Engenharia');
aluno.saudacao();

fazerRequisicao('https://api.example.com/dados');
```

Este código inclui uma função para calcular o fatorial de um número utilizando recursão, duas classes (`Pessoa` e `Aluno`) com herança, um método assíncrono para fazer uma requisição HTTP e exemplos de utilização de todas essas funcionalidades.