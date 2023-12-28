Claro! Aqui está um exemplo de um código complexo em TypeScript:

```typescript
/**
 * Classe que representa uma pessoa.
 */
class Pessoa {
  nome: string;
  idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  /**
   * Método que retorna a saudação da pessoa.
   */
  saudacao(): string {
    return `Olá, meu nome é ${this.nome} e tenho ${this.idade} anos.`;
  }
}

/**
 * Classe que representa uma lista de pessoas.
 */
class ListaPessoas {
  pessoas: Pessoa[];

  constructor() {
    this.pessoas = [];
  }

  /**
   * Método para adicionar uma pessoa à lista.
   * @param pessoa A pessoa a ser adicionada.
   */
  adicionarPessoa(pessoa: Pessoa): void {
    this.pessoas.push(pessoa);
  }

  /**
   * Método para remover uma pessoa da lista.
   * @param pessoa A pessoa a ser removida.
   */
  removerPessoa(pessoa: Pessoa): void {
    const index = this.pessoas.indexOf(pessoa);
    if (index !== -1) {
      this.pessoas.splice(index, 1);
    }
  }

  /**
   * Método que retorna a lista de pessoas.
   */
  obterPessoas(): Pessoa[] {
    return this.pessoas;
  }
}

// Criando algumas instâncias de Pessoa
const pessoa1 = new Pessoa("João", 25);
const pessoa2 = new Pessoa("Maria", 30);
const pessoa3 = new Pessoa("Carlos", 35);

// Criando uma instância de ListaPessoas
const listaPessoas = new ListaPessoas();

// Adicionando as pessoas à lista
listaPessoas.adicionarPessoa(pessoa1);
listaPessoas.adicionarPessoa(pessoa2);
listaPessoas.adicionarPessoa(pessoa3);

// Removendo a pessoa2 da lista
listaPessoas.removerPessoa(pessoa2);

// Obtendo a lista de pessoas
const pessoas = listaPessoas.obterPessoas();

// Exibindo a saudação de cada pessoa
for (const pessoa of pessoas) {
  console.log(pessoa.saudacao());
}
```

Neste exemplo, criamos duas classes em TypeScript: `Pessoa` e `ListaPessoas`. A classe `Pessoa` representa uma pessoa com propriedades de `nome` e `idade`, além de um método `saudacao()` que retorna uma saudação com o nome e a idade da pessoa.

A classe `ListaPessoas` representa uma lista de pessoas, onde podemos adicionar, remover e obter a lista de pessoas. Ela possui um array de objetos do tipo `Pessoa` e implementa os métodos `adicionarPessoa`, `removerPessoa` e `obterPessoas`.

Em seguida, criamos algumas instâncias da classe `Pessoa` e as adicionamos à lista de pessoas. Em seguida, removemos uma pessoa da lista e, por fim, exibimos a saudação de cada pessoa através de um loop `for...of`.

Esse é apenas um exemplo de um código complexo em TypeScript, e as possibilidades são infinitas. A linguagem TypeScript é poderosa e flexível, permitindo a criação de programas de qualquer tamanho e complexidade.