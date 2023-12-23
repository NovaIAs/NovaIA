Claro! Aqui está um código complexo em TypeScript:

```typescript
// Classe base para um animal
abstract class Animal {
  protected nome: string;
  protected idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  abstract emitirSom(): void;
}

// Classe para representar um cachorro
class Cachorro extends Animal {
  private raca: string;

  constructor(nome: string, idade: number, raca: string) {
    super(nome, idade);
    this.raca = raca;
  }

  emitirSom(): void {
    console.log("Au Au!");
  }

  latirParaOutroCachorro(outroCachorro: Cachorro): void {
    console.log(`${this.nome} latiu para ${outroCachorro.nome}`);
  }
}

// Classe para representar um gato
class Gato extends Animal {
  private cor: string;

  constructor(nome: string, idade: number, cor: string) {
    super(nome, idade);
    this.cor = cor;
  }

  emitirSom(): void {
    console.log("Miau!");
  }

  miarParaOutroGato(outroGato: Gato): void {
    console.log(`${this.nome} miou para ${outroGato.nome}`);
  }
}

// Função para criar animais aleatórios
function criarAnimaisAleatorios(): Animal[] {
  const animais: Animal[] = [];

  for (let i = 0; i < 5; i++) {
    const idade = Math.floor(Math.random() * 10) + 1;
    const racaOuCor = Math.random() > 0.5 ? "Vira-lata" : "Amarelo";
    const animal = Math.random() > 0.5
      ? new Cachorro(`Cachorro${i}`, idade, racaOuCor)
      : new Gato(`Gato${i}`, idade, racaOuCor);

    animais.push(animal);
  }

  return animais;
}

// Função para fazer os animais interagirem
function fazerAnimaisInteragirem(animais: Animal[]): void {
  for (let i = 0; i < animais.length; i++) {
    const animal = animais[i];

    if (animal instanceof Cachorro) {
      for (let j = i + 1; j < animais.length; j++) {
        if (animais[j] instanceof Cachorro) {
          animal.latirParaOutroCachorro(animais[j] as Cachorro);
        }
      }
    } else if (animal instanceof Gato) {
      for (let j = i + 1; j < animais.length; j++) {
        if (animais[j] instanceof Gato) {
          animal.miarParaOutroGato(animais[j] as Gato);
        }
      }
    }
  }
}

// Execução do código
const animais = criarAnimaisAleatorios();
fazerAnimaisInteragirem(animais);
```

Neste código, temos o conceito de herança e polimorfismo sendo aplicados. Primeiro, temos uma classe abstrata chamada `Animal`, que serve como base para as classes `Cachorro` e `Gato`. A classe `Animal` possui propriedades comuns a todos os animais, como nome e idade, e um método abstrato `emitirSom()`, que deve ser implementado pelas subclasses.

As classes `Cachorro` e `Gato` estendem a classe `Animal` e implementam o método `emitirSom()`. Além disso, cada uma possui métodos específicos para interagir com outros animais da mesma espécie.

A função `criarAnimaisAleatorios()` cria um array de animais com dados aleatórios, sendo 50% de chance de criar um cachorro e 50% de chance de criar um gato. Os animais são adicionados no array `animais`.

A função `fazerAnimaisInteragirem()` percorre o array de animais e faz com que os animais interajam entre si. Se o animal for um cachorro, ele irá latir para outros cachorros no array. Se for um gato, ele irá miar para outros gatos no array.

Por fim, na execução do código, criamos os animais aleatórios e fazemos com que eles interajam entre si. Os latidos e miados são exibidos no console.

Espero que este código atenda às suas expectativas!