Claro! Aqui está um exemplo de um código complexo em TypeScript:

```typescript
// Classe abstrata para representar um animal
abstract class Animal {
  protected nome: string;
  protected idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  abstract fazerBarulho(): void;

  obterDetalhes(): string {
    return `Nome: ${this.nome}, Idade: ${this.idade}`;
  }
}

// Classe que representa um cachorro, que herda da classe Animal
class Cachorro extends Animal {
  private raca: string;

  constructor(nome: string, idade: number, raca: string) {
    super(nome, idade);
    this.raca = raca;
  }

  fazerBarulho(): void {
    console.log("Au au!");
  }

  obterDetalhes(): string {
    return `${super.obterDetalhes()}, Raça: ${this.raca}`;
  }
}

// Classe que representa um gato, que herda da classe Animal
class Gato extends Animal {
  private cor: string;

  constructor(nome: string, idade: number, cor: string) {
    super(nome, idade);
    this.cor = cor;
  }

  fazerBarulho(): void {
    console.log("Miau!");
  }

  obterDetalhes(): string {
    return `${super.obterDetalhes()}, Cor: ${this.cor}`;
  }
}

// Função para criar uma lista de animais
function criarListaAnimais(): Animal[] {
  const listaAnimais: Animal[] = [];

  const cachorro1 = new Cachorro("Rex", 3, "Labrador");
  const cachorro2 = new Cachorro("Max", 5, "Poodle");
  const gato1 = new Gato("Luna", 2, "Branco");
  const gato2 = new Gato("Simba", 4, "Amarelo");

  listaAnimais.push(cachorro1, cachorro2, gato1, gato2);

  return listaAnimais;
}

// Função para exibir os detalhes dos animais
function exibirDetalhesAnimais(listaAnimais: Animal[]): void {
  for (const animal of listaAnimais) {
    console.log(animal.obterDetalhes());
    animal.fazerBarulho();
    console.log("--------------------");
  }
}

// Chamada das funções
const listaAnimais = criarListaAnimais();
exibirDetalhesAnimais(listaAnimais);
```

Neste exemplo, temos uma hierarquia de classes representando animais, com uma classe abstrata `Animal` e duas classes concretas `Cachorro` e `Gato` que herdam dela. Cada animal tem um nome, idade e comportamentos específicos, representados pelos métodos `fazerBarulho()` e `obterDetalhes()`. A função `criarListaAnimais()` cria uma lista de animais e a função `exibirDetalhesAnimais()` exibe os detalhes de cada animal e faz com que eles emitam seus barulhos característicos.

Espero que este exemplo atenda às suas expectativas!