Claro! Aqui está um código complexo em TypeScript para você:

```
// Definindo uma interface para um objeto Pessoa
interface Pessoa {
  nome: string;
  idade: number;
  enderecos: string[];
  contatos: Contato[];
}

// Definindo uma interface para um objeto Contato
interface Contato {
  tipo: string;
  valor: string;
}

// Definindo uma classe Pessoa
class MinhaPessoa implements Pessoa {
  nome: string;
  idade: number;
  enderecos: string[];
  contatos: Contato[];

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
    this.enderecos = [];
    this.contatos = [];
  }

  // Método para adicionar um endereço ao objeto Pessoa
  adicionarEndereco(endereco: string): void {
    this.enderecos.push(endereco);
  }

  // Método para adicionar um contato ao objeto Pessoa
  adicionarContato(tipo: string, valor: string): void {
    const novoContato: Contato = {
      tipo: tipo,
      valor: valor,
    };
    this.contatos.push(novoContato);
  }
}

// Criando uma instância da classe Pessoa
const pessoa: Pessoa = new MinhaPessoa("João", 25);

// Adicionando endereços
pessoa.adicionarEndereco("Rua A, 123");
pessoa.adicionarEndereco("Rua B, 456");

// Adicionando contatos
pessoa.adicionarContato("Telefone", "123456789");
pessoa.adicionarContato("Email", "joao@email.com");

// Imprimindo os dados da pessoa
console.log(`Nome: ${pessoa.nome}`);
console.log(`Idade: ${pessoa.idade}`);
console.log(`Endereços: ${pessoa.enderecos.join(", ")}`);
console.log(`Contatos:`);
pessoa.contatos.forEach((contato) => {
  console.log(`- ${contato.tipo}: ${contato.valor}`);
});
```

Neste código, utilizamos o TypeScript para definir duas interfaces: `Pessoa` e `Contato`. A interface `Pessoa` possui propriedades como `nome`, `idade`, `enderecos` (um array de strings) e `contatos` (um array de objetos do tipo `Contato`).

Em seguida, criamos uma classe `MinhaPessoa` que implementa a interface `Pessoa`. Essa classe possui um construtor que recebe o nome e a idade da pessoa e inicializa as propriedades `nome`, `idade`, `enderecos` (inicializado como um array vazio) e `contatos` (inicializado como um array vazio).

A classe `MinhaPessoa` também possui dois métodos: `adicionarEndereco` e `adicionarContato`. O método `adicionarEndereco` recebe um endereço como parâmetro e o adiciona no array `enderecos` da pessoa. O método `adicionarContato` recebe um tipo e um valor de contato e cria um novo objeto `Contato` com esses valores, adicionando-o no array `contatos` da pessoa.

Por fim, criamos uma instância da classe `MinhaPessoa` chamada `pessoa` e utilizamos os métodos `adicionarEndereco` e `adicionarContato` para adicionar informações à pessoa. Em seguida, imprimimos os dados da pessoa no console.

Espero que este código atenda às suas expectativas! Se precisar de mais alguma coisa, é só pedir.