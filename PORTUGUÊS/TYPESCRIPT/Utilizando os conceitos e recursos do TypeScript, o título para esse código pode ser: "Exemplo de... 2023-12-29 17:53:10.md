Claro! Aqui está um código complexo em TypeScript que utiliza vários conceitos e recursos da linguagem:

```typescript
// Classe base para um animal
class Animal {
  protected nome: string;
  protected idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  // Método para obter a descrição do animal
  public obterDescricao(): string {
    return `Nome: ${this.nome}, Idade: ${this.idade}`;
  }

  // Método abstrato para emitir som
  public emitirSom(): void {
    throw new Error('Método não implementado');
  }
}

// Classe para representar um cachorro
class Cachorro extends Animal {
  private raca: string;

  constructor(nome: string, idade: number, raca: string) {
    super(nome, idade);
    this.raca = raca;
  }

  // Implementação do método para emitir som
  public emitirSom(): void {
    console.log('Au Au!');
  }

  // Método específico para cães
  public latir(): void {
    console.log('Cachorro latindo!');
  }
}

// Classe para representar um gato
class Gato extends Animal {
  private cor: string;

  constructor(nome: string, idade: number, cor: string) {
    super(nome, idade);
    this.cor = cor;
  }

  // Implementação do método para emitir som
  public emitirSom(): void {
    console.log('Miau!');
  }

  // Método específico para gatos
  public arranhar(): void {
    console.log('Gato arranhando!');
  }
}

// Função para processar animais
function processarAnimais(animais: Animal[]): void {
  for (const animal of animais) {
    console.log(animal.obterDescricao());
    animal.emitirSom();

    if (animal instanceof Cachorro) {
      const cachorro = animal as Cachorro;
      cachorro.latir();
    } else if (animal instanceof Gato) {
      const gato = animal as Gato;
      gato.arranhar();
    }

    console.log('-------------------');
  }
}

// Criação de instâncias de animais
const cachorro = new Cachorro('Rex', 5, 'Vira-lata');
const gato = new Gato('Mimi', 3, 'Branco');

// Processamento dos animais
processarAnimais([cachorro, gato]);
```

Este código exemplifica a utilização de classes, herança, polimorfismo, métodos abstratos e instanceof em TypeScript.

A classe `Animal` é uma classe base abstrata que possui propriedades comuns a todos os animais, como o nome e a idade. Ela possui um método `obterDescricao()` que retorna uma string com as informações do animal e um método abstrato `emitirSom()` que deve ser implementado pelas subclasses.

A classe `Cachorro` e `Gato` são subclasses de `Animal`, que herdam suas propriedades e métodos. Cada uma delas implementa o método `emitirSom()` de forma específica para cada animal e possui um método adicional específico para cada um (`latir()` para cachorro e `arranhar()` para gato).

A função `processarAnimais()` recebe um array de animais e itera sobre eles, exibindo suas descrições, emitindo seus sons e chamando os métodos adicionais específicos de cachorro ou gato, dependendo do tipo do animal.

No final do código, são criadas instâncias de um cachorro e um gato, e esses animais são passados como argumento para a função `processarAnimais()`.

Espero que este código complexo e diferenciado em TypeScript atenda às suas expectativas!