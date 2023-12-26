Claro! Aqui está um exemplo de código complexo em TypeScript:

```typescript
// Classe base
class Animal {
  constructor(private nome: string) {}

  mover(distancia: number = 0) {
    console.log(`${this.nome} se moveu ${distancia} metros.`);
  }
}

// Classe derivada
class Cachorro extends Animal {
  constructor(nome: string) {
    super(nome);
  }

  latir() {
    console.log(`${this.nome} latiu: Au Au!`);
  }
}

// Classe derivada
class Gato extends Animal {
  constructor(nome: string) {
    super(nome);
  }

  miar() {
    console.log(`${this.nome} miou: Miau!`);
  }
}

// Função para criar animais aleatórios
function criarAnimalAleatorio(): Animal {
  const random = Math.random();
  if (random < 0.5) {
    return new Cachorro("Rex");
  } else {
    return new Gato("Mimi");
  }
}

// Criando uma lista de animais
const animais: Animal[] = [];
for (let i = 0; i < 5; i++) {
  const animal = criarAnimalAleatorio();
  animais.push(animal);
}

// Movendo os animais e chamando métodos específicos
animais.forEach((animal) => {
  animal.mover(10);
  if (animal instanceof Cachorro) {
    animal.latir();
  } else if (animal instanceof Gato) {
    animal.miar();
  }
});
```

Neste exemplo, temos uma hierarquia de classes onde a classe `Animal` é a classe base, e as classes `Cachorro` e `Gato` são classes derivadas.

A classe `Animal` possui um construtor que recebe o nome do animal e um método `mover` que exibe no console a distância percorrida pelo animal.

As classes `Cachorro` e `Gato` herdam da classe `Animal` e possuem métodos específicos para cada tipo de animal, como `latir` e `miar`, respectivamente.

A função `criarAnimalAleatorio` é responsável por criar um animal aleatório, sendo que há uma chance de 50% de criar um cachorro ou um gato.

Em seguida, criamos uma lista de animais e preenchemos com animais aleatórios utilizando a função `criarAnimalAleatorio`. Por fim, percorremos a lista de animais e chamamos o método `mover` para cada um deles, exibindo também o método específico de cada tipo de animal, se aplicável.

Este código demonstra o uso de classes, herança, polimorfismo e a criação de objetos aleatórios. É um exemplo complexo que envolve vários conceitos do TypeScript.