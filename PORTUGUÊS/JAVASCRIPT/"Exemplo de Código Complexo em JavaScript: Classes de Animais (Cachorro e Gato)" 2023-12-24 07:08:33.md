Claro! Aqui está um exemplo de um código complexo em JavaScript, utilizando alguns conceitos avançados e padrões de design:

```javascript
// Classe base para criação de animais
class Animal {
  constructor(nome, idade) {
    this.nome = nome;
    this.idade = idade;
  }

  comer() {
    console.log(`${this.nome} está comendo.`);
  }

  dormir() {
    console.log(`${this.nome} está dormindo.`);
  }
}

// Classe específica para criação de cães
class Cachorro extends Animal {
  constructor(nome, idade, raca) {
    super(nome, idade);
    this.raca = raca;
  }

  latir() {
    console.log(`${this.nome} está latindo.`);
  }
}

// Classe específica para criação de gatos
class Gato extends Animal {
  constructor(nome, idade, cor) {
    super(nome, idade);
    this.cor = cor;
  }

  miar() {
    console.log(`${this.nome} está miando.`);
  }
}

// Função de utilidade para criar instâncias de animais
function criarAnimal(tipo, nome, idade, propriedades) {
  switch (tipo) {
    case "Cachorro":
      return new Cachorro(nome, idade, propriedades.raca);
    case "Gato":
      return new Gato(nome, idade, propriedades.cor);
    default:
      throw new Error(`Tipo de animal desconhecido: ${tipo}`);
  }
}

// Exemplo de uso
const cachorro = criarAnimal("Cachorro", "Rex", 5, { raca: "Labrador" });
const gato = criarAnimal("Gato", "Miau", 3, { cor: "Cinza" });

cachorro.comer(); // Output: Rex está comendo.
cachorro.dormir(); // Output: Rex está dormindo.
cachorro.latir(); // Output: Rex está latindo.

gato.comer(); // Output: Miau está comendo.
gato.dormir(); // Output: Miau está dormindo.
gato.miar(); // Output: Miau está miando.
```

Neste exemplo, temos a definição de três classes: `Animal`, `Cachorro` e `Gato`. A classe `Animal` é a classe base, que possui propriedades comuns a todos os animais, como nome e idade, além de métodos como `comer` e `dormir`.

As classes `Cachorro` e `Gato` são subclasses da classe `Animal`, que herdam suas propriedades e métodos. Além disso, elas possuem métodos específicos, como `latir` para o `Cachorro` e `miar` para o `Gato`.

A função `criarAnimal` é uma função de utilidade que recebe o tipo de animal desejado, juntamente com suas propriedades específicas (raca para o `Cachorro` e cor para o `Gato`), e retorna uma nova instância do animal correspondente.

No exemplo de uso, criamos um cachorro chamado "Rex" e um gato chamado "Miau", e utilizamos os métodos de cada um dos animais.

Espero que este código complexo em JavaScript tenha atendido às suas expectativas!