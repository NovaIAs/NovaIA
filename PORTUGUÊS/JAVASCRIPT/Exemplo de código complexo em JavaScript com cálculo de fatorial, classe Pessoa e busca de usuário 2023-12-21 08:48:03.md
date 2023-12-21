Claro! Aqui está um exemplo de código complexo em JavaScript:

```javascript
// Função para calcular o fatorial de um número
function calcularFatorial(numero) {
  if (numero === 0 || numero === 1) {
    return 1;
  } else {
    return numero * calcularFatorial(numero - 1);
  }
}

// Classe para representar uma pessoa
class Pessoa {
  constructor(nome, idade) {
    this.nome = nome;
    this.idade = idade;
  }

  saudacao() {
    console.log(`Olá, meu nome é ${this.nome} e eu tenho ${this.idade} anos.`);
  }
}

// Função assíncrona para buscar dados de um usuário
async function buscarUsuario(id) {
  try {
    const resposta = await fetch(`https://api.example.com/usuarios/${id}`);
    const usuario = await resposta.json();
    return usuario;
  } catch (error) {
    console.error(error);
    return null;
  }
}

// Utilizando as funções e classes definidas acima

const numero = 5;
const resultadoFatorial = calcularFatorial(numero);
console.log(`O fatorial de ${numero} é ${resultadoFatorial}`);

const pessoa = new Pessoa("João", 30);
pessoa.saudacao();

const idUsuario = 123;
buscarUsuario(idUsuario)
  .then((usuario) => {
    if (usuario) {
      console.log(`Usuário encontrado: ${usuario.nome}`);
    } else {
      console.log(`Usuário com ID ${idUsuario} não encontrado.`);
    }
  })
  .catch((error) => console.error(error));
```

Este código contém uma função para calcular o fatorial de um número, uma classe para representar uma pessoa, uma função assíncrona para buscar dados de um usuário em uma API e exemplos de utilização dessas funcionalidades.

A função `calcularFatorial` utiliza uma abordagem recursiva para calcular o fatorial de um número. Ela verifica se o número é igual a 0 ou 1, e retorna 1 nestes casos. Caso contrário, ela chama a si mesma passando o número decrementado em 1, multiplicado pelo próprio número.

A classe `Pessoa` possui um construtor que recebe o nome e a idade da pessoa e os armazena em propriedades. Ela também possui um método `saudacao` que exibe uma mensagem de saudação com o nome e a idade da pessoa.

A função assíncrona `buscarUsuario` utiliza a API `fetch` para fazer uma requisição HTTP para obter os dados de um usuário com base em um ID. Ela retorna uma `Promise` e, se a requisição for bem-sucedida, o resultado é convertido para JSON e retornado. Caso ocorra algum erro durante a requisição, o erro é capturado pelo bloco `catch` e tratado, exibindo uma mensagem de erro no console.

No exemplo de utilização, é calculado o fatorial do número 5 e o resultado é exibido no console. Em seguida, uma instância da classe `Pessoa` é criada com nome "João" e idade 30, e é chamado o método `saudacao` para exibir uma mensagem de saudação.

Por fim, é chamada a função `buscarUsuario` passando um ID de usuário. O resultado da chamada é tratado utilizando o método `then`, que recebe uma função de callback que é executada quando a `Promise` é resolvida. Se o usuário for encontrado, é exibida uma mensagem com o nome do usuário. Caso contrário, é exibida uma mensagem informando que o usuário não foi encontrado. Se ocorrer algum erro durante a execução da função, ele é capturado pelo método `catch` e uma mensagem de erro é exibida no console.