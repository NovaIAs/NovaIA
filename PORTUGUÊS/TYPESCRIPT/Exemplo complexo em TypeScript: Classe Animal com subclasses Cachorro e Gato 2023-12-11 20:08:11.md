Claro! Segue abaixo um exemplo de código complexo em TypeScript:

```typescript
// Classe base de animais
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

  latir(): void {
    console.log("O cachorro está latindo!");
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

  arranhar(): void {
    console.log("O gato está arranhando!");
  }
}

// Função para criar animais aleatórios
function criarAnimalAleatorio(): Animal {
  const random = Math.random();
  if (random < 0.5) {
    return new Cachorro("Rex", 3, "Labrador");
  } else {
    return new Gato("Felix", 2, "Preto");
  }
}

// Criação de animais
const animal1 = criarAnimalAleatorio();
const animal2 = criarAnimalAleatorio();

// Chamada do método emitirSom para cada animal
animal1.emitirSom();
animal2.emitirSom();

// Verificação de tipo e chamada de métodos específicos
if (animal1 instanceof Cachorro) {
  (animal1 as Cachorro).latir();
} else if (animal1 instanceof Gato) {
  (animal1 as Gato).arranhar();
}

if (animal2 instanceof Cachorro) {
  (animal2 as Cachorro).latir();
} else if (animal2 instanceof Gato) {
  (animal2 as Gato).arranhar();
}
```

Neste exemplo, temos um código em TypeScript que simula uma classe base de animais, com implementações específicas para cachorros e gatos. A classe base `Animal` é abstrata, o que significa que não pode ser instanciada diretamente. As classes `Cachorro` e `Gato` herdam da classe `Animal` e implementam o método abstrato `emitirSom()`.

Além disso, cada classe possui métodos específicos: `latir()` para cachorros e `arranhar()` para gatos. A função `criarAnimalAleatorio()` gera um animal aleatório, que pode ser um cachorro ou um gato, e retorna uma instância da classe correspondente.

No restante do código, criamos dois animais aleatórios e chamamos o método `emitirSom()` para cada um deles. Em seguida, verificamos o tipo de cada animal e chamamos os métodos específicos (`latir()` para cachorros e `arranhar()` para gatos) de acordo com o tipo.

Esse código é apenas um exemplo complexo e diferenciado, mas pode ser adaptado e expandido de acordo com as necessidades específicas de um projeto.