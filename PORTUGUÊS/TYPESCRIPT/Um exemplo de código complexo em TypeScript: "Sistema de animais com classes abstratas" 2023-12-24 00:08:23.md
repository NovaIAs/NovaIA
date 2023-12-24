Certamente! Aqui está um exemplo de um código complexo em TypeScript:

```typescript
// Classe abstrata para representar um animal
abstract class Animal {
  protected nome: string;
  protected idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  abstract emiteSom(): void;

  exibirInformacoes(): void {
    console.log(`Nome: ${this.nome}`);
    console.log(`Idade: ${this.idade}`);
  }
}

// Classe que representa um cachorro
class Cachorro extends Animal {
  private raca: string;

  constructor(nome: string, idade: number, raca: string) {
    super(nome, idade);
    this.raca = raca;
  }

  emiteSom(): void {
    console.log("Au au!");
  }

  exibirInformacoes(): void {
    console.log("Cachorro:");
    super.exibirInformacoes();
    console.log(`Raça: ${this.raca}`);
  }
}

// Classe que representa um gato
class Gato extends Animal {
  private pelagem: string;

  constructor(nome: string, idade: number, pelagem: string) {
    super(nome, idade);
    this.pelagem = pelagem;
  }

  emiteSom(): void {
    console.log("Miau!");
  }

  exibirInformacoes(): void {
    console.log("Gato:");
    super.exibirInformacoes();
    console.log(`Pelagem: ${this.pelagem}`);
  }
}

// Função principal
function main(): void {
  const cachorro: Cachorro = new Cachorro("Rex", 5, "Labrador");
  const gato: Gato = new Gato("Frajola", 3, "Malhado");

  cachorro.exibirInformacoes();
  cachorro.emiteSom();

  console.log();

  gato.exibirInformacoes();
  gato.emiteSom();
}

// Executa a função principal
main();
```

Neste exemplo, criei um programa TypeScript que simula um sistema de animais. Começo definindo uma classe abstrata `Animal`, que serve como base para as classes `Cachorro` e `Gato`. A classe `Animal` possui propriedades como `nome` e `idade`, bem como o método abstrato `emiteSom()`, que deve ser implementado nas subclasses.

A classe `Cachorro` herda da classe `Animal` e adiciona a propriedade `raca`. O método `emiteSom()` é implementado para exibir o som "Au au!". A classe também sobrescreve o método `exibirInformacoes()`, adicionando a informação da raça do cachorro.

A classe `Gato` também herda da classe `Animal` e adiciona a propriedade `pelagem`. O método `emiteSom()` é implementado para exibir o som "Miau!". A classe também sobrescreve o método `exibirInformacoes()`, adicionando a informação da pelagem do gato.

A função `main()` cria instâncias de `Cachorro` e `Gato`, exibe suas informações e emite o som característico de cada um.

Espero que esse exemplo atenda às suas expectativas!