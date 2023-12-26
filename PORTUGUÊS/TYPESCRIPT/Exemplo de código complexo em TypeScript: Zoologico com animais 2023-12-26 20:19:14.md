Claro! Aqui está um exemplo de um código complexo em TypeScript com uma funcionalidade diferenciada:

```typescript
// Classe para representar um animal
class Animal {
  private nome: string;
  private idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  public comer(quantidade: number): void {
    console.log(`${this.nome} está comendo ${quantidade} unidades de comida.`);
  }

  public dormir(): void {
    console.log(`${this.nome} está dormindo.`);
  }

  public emitirSom(): void {
    console.log(`${this.nome} está emitindo um som.`);
  }
}

// Classe para representar um cachorro, que herda de Animal
class Cachorro extends Animal {
  private raca: string;

  constructor(nome: string, idade: number, raca: string) {
    super(nome, idade);
    this.raca = raca;
  }

  public latir(): void {
    console.log(`${this.nome} (${this.raca}) está latindo: Au Au!`);
  }
}

// Classe para representar um gato, que herda de Animal
class Gato extends Animal {
  private cor: string;

  constructor(nome: string, idade: number, cor: string) {
    super(nome, idade);
    this.cor = cor;
  }

  public miar(): void {
    console.log(`${this.nome} (${this.cor}) está miando: Miau Miau!`);
  }
}

// Classe para representar um zoológico
class Zoologico {
  private animais: Animal[];

  constructor() {
    this.animais = [];
  }

  public adicionarAnimal(animal: Animal): void {
    this.animais.push(animal);
    console.log(`${animal.constructor.name} '${animal.nome}' foi adicionado ao zoológico.`);
  }

  public alimentarAnimais(quantidade: number): void {
    console.log(`Alimentando todos os animais com ${quantidade} unidades de comida:`);
    this.animais.forEach((animal) => {
      animal.comer(quantidade);
    });
  }

  public fazerAnimaisEmitiremSom(): void {
    console.log("Fazendo os animais emitirem som:");
    this.animais.forEach((animal) => {
      animal.emitirSom();
    });
  }
}

// Utilização do código

// Criação do zoológico
const zoo = new Zoologico();

// Criação de alguns animais
const cachorro = new Cachorro("Rex", 4, "Labrador");
const gato = new Gato("Mimi", 2, "Branco");

// Adição dos animais ao zoológico
zoo.adicionarAnimal(cachorro);
zoo.adicionarAnimal(gato);

// Alimentação dos animais
zoo.alimentarAnimais(3);

// Fazendo os animais emitirem som
zoo.fazerAnimaisEmitiremSom();

// Cachorro latindo
cachorro.latir();

// Gato miando
gato.miar();
```

Neste exemplo, criamos uma estrutura de classes para representar animais em um zoológico. A classe `Animal` é a classe base, com propriedades como nome e idade, e métodos como comer, dormir e emitirSom.

As classes `Cachorro` e `Gato` herdam da classe `Animal` e adicionam comportamentos específicos para cada tipo de animal, como o método `latir` para o cachorro e `miar` para o gato.

A classe `Zoologico` representa o zoológico em si, com métodos para adicionar animais, alimentá-los e fazer com que emitam som.

No final do código, criamos instâncias das classes e utilizamos os métodos para adicionar animais ao zoológico, alimentá-los, fazer com que emitam som, e também utilizamos os métodos específicos de cada animal, como o `latir` e `miar`.

Espero que este exemplo atenda às suas expectativas!