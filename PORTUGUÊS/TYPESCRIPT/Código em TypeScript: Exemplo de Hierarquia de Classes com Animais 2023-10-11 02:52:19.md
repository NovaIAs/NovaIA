Claro! Aqui está um exemplo de um código complexo em TypeScript:

```typescript
// Classe abstrata de um Animal
abstract class Animal {
  protected nome: string;
  protected idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  abstract emitirSom(): void;
}

// Classe Cachorro que herda de Animal
class Cachorro extends Animal {
  private raca: string;

  constructor(nome: string, idade: number, raca: string) {
    super(nome, idade);
    this.raca = raca;
  }

  emitirSom(): void {
    console.log("Au au!");
  }

  latirParaOutroCachorro(outroCachorro: Cachorro): void {
    console.log(`${this.nome} está latindo para ${outroCachorro.nome}`);
  }
}

// Classe Gato que herda de Animal
class Gato extends Animal {
  private cor: string;

  constructor(nome: string, idade: number, cor: string) {
    super(nome, idade);
    this.cor = cor;
  }

  emitirSom(): void {
    console.log("Miau!");
  }

  arranharMobilia(): void {
    console.log(`${this.nome} está arranhando a mobília!`);
  }
}

// Criação de objetos e chamada de métodos
const rex = new Cachorro("Rex", 3, "Vira-lata");
rex.emitirSom(); // Saída: Au au!

const felix = new Gato("Felix", 5, "Preto");
felix.emitirSom(); // Saída: Miau!

rex.latirParaOutroCachorro(felix); // Saída: Rex está latindo para Felix

felix.arranharMobilia(); // Saída: Felix está arranhando a mobília!
```

Neste exemplo, temos a definição de três classes: `Animal`, `Cachorro` e `Gato`.

A classe `Animal` é uma classe abstrata que serve como base para as outras classes. Ela possui os atributos `nome` e `idade`, além do método abstrato `emitirSom()`.

A classe `Cachorro` herda de `Animal` e adiciona o atributo `raca`. Ela implementa o método `emitirSom()`, que imprime "Au au!" no console. Além disso, a classe possui um método `latirParaOutroCachorro()`, que recebe outro objeto do tipo `Cachorro` e imprime uma mensagem indicando que o cachorro atual está latindo para o outro cachorro.

A classe `Gato` também herda de `Animal` e adiciona o atributo `cor`. Ela implementa o método `emitirSom()`, que imprime "Miau!" no console. Além disso, a classe possui o método `arranharMobilia()`, que imprime uma mensagem indicando que o gato atual está arranhando a mobília.

No final do código, são criados objetos das classes `Cachorro` e `Gato`, e são chamados métodos desses objetos para demonstrar o funcionamento das classes.

Espero que este exemplo atenda às suas expectativas!