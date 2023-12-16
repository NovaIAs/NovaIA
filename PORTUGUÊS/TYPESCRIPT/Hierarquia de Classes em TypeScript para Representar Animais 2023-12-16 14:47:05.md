Claro! Aqui está um exemplo de um código complexo em TypeScript:

```typescript
// Classe base para um animal
abstract class Animal {
  protected nome: string;
  protected idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  abstract fazerBarulho(): void;

  mostrarInformacoes(): void {
    console.log(`Nome: ${this.nome}`);
    console.log(`Idade: ${this.idade}`);
  }
}

// Classe para representar um cachorro
class Cachorro extends Animal {
  private raca: string;

  constructor(nome: string, idade: number, raca: string) {
    super(nome, idade);
    this.raca = raca;
  }

  fazerBarulho(): void {
    console.log("Au au!");
  }

  mostrarInformacoes(): void {
    super.mostrarInformacoes();
    console.log(`Raça: ${this.raca}`);
  }
}

// Classe para representar um gato
class Gato extends Animal {
  private cor: string;

  constructor(nome: string, idade: number, cor: string) {
    super(nome, idade);
    this.cor = cor;
  }

  fazerBarulho(): void {
    console.log("Miau!");
  }

  mostrarInformacoes(): void {
    super.mostrarInformacoes();
    console.log(`Cor: ${this.cor}`);
  }
}

// Função principal
function main(): void {
  const cachorro: Cachorro = new Cachorro("Rex", 5, "Labrador");
  const gato: Gato = new Gato("Bolinha", 3, "Branco");

  cachorro.fazerBarulho();
  cachorro.mostrarInformacoes();

  gato.fazerBarulho();
  gato.mostrarInformacoes();
}

// Executar a função principal
main();
```

Neste código, criamos uma hierarquia de classes que representam animais usando herança e polimorfismo. 

A classe `Animal` é uma classe abstrata que serve como base para as classes `Cachorro` e `Gato`. Ela possui as propriedades `nome` e `idade`, e um método abstrato `fazerBarulho()` que deve ser implementado nas classes derivadas.

A classe `Cachorro` e `Gato` herdam da classe `Animal` e implementam o método `fazerBarulho()` de forma diferente para representar os sons que cada animal faz. Além disso, cada classe possui propriedades específicas (`raca` para o cachorro e `cor` para o gato) e um método `mostrarInformacoes()` que exibe informações sobre o animal.

Na função principal `main()`, criamos instâncias de `Cachorro` e `Gato`, chamamos os métodos `fazerBarulho()` e `mostrarInformacoes()` para cada um, e assim podemos ver o resultado no console.

Este código é um exemplo simples de como utilizar herança e polimorfismo em TypeScript para modelar uma hierarquia de classes. Claro, este é apenas um exemplo e existem muitas outras possibilidades e aplicações para o TypeScript.