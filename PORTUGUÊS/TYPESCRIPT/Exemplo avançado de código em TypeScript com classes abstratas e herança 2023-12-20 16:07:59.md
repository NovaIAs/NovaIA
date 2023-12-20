Claro! Aqui está um exemplo de um código complexo em TypeScript, que utiliza conceitos avançados e várias funcionalidades da linguagem:

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

  mostrarInformacoes(): void {
    console.log(`Nome: ${this.nome}`);
    console.log(`Idade: ${this.idade}`);
  }
}

// Classe para representar um cachorro, que herda da classe Animal
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
    console.log("Informações do cachorro:");
    super.mostrarInformacoes();
    console.log(`Raça: ${this.raca}`);
  }
}

// Classe para representar um gato, que herda da classe Animal
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
    console.log("Informações do gato:");
    super.mostrarInformacoes();
    console.log(`Cor: ${this.cor}`);
  }
}

// Função para criar instâncias de animais aleatórios
function criarAnimaisAleatorios(): Animal[] {
  const animais: Animal[] = [];
  const nomesCachorro = ["Max", "Bella", "Charlie", "Lucy", "Cooper"];
  const nomesGato = ["Oliver", "Luna", "Leo", "Milo", "Simba"];
  const racasCachorro = ["Labrador", "Poodle", "Bulldog", "Golden Retriever", "Beagle"];
  const coresGato = ["Preto", "Branco", "Cinza", "Amarelo", "Marrom"];

  for (let i = 0; i < 5; i++) {
    const idade = Math.floor(Math.random() * 10) + 1;
    const nomeCachorro = nomesCachorro[Math.floor(Math.random() * nomesCachorro.length)];
    const nomeGato = nomesGato[Math.floor(Math.random() * nomesGato.length)];
    const raca = racasCachorro[Math.floor(Math.random() * racasCachorro.length)];
    const cor = coresGato[Math.floor(Math.random() * coresGato.length)];

    if (Math.random() < 0.5) {
      animais.push(new Cachorro(nomeCachorro, idade, raca));
    } else {
      animais.push(new Gato(nomeGato, idade, cor));
    }
  }

  return animais;
}

// Função para exibir as informações de todos os animais
function exibirInformacoesAnimais(animais: Animal[]): void {
  animais.forEach((animal) => {
    animal.mostrarInformacoes();
    animal.fazerBarulho();
    console.log("--------------------");
  });
}

// Teste do código
const animais = criarAnimaisAleatorios();
exibirInformacoesAnimais(animais);
```

Neste código, temos a definição de três classes: `Animal` (classe abstrata), `Cachorro` (subclasse de `Animal`) e `Gato` (subclasse de `Animal`). A classe `Animal` possui propriedades comuns a todos os animais, como `nome` e `idade`, além de métodos abstratos que devem ser implementados pelas subclasses, como `fazerBarulho()`. As subclasses `Cachorro` e `Gato` implementam o método `fazerBarulho()` de maneiras diferentes.

Além disso, o código possui a função `criarAnimaisAleatorios()`, que cria uma lista de animais com dados aleatórios, e a função `exibirInformacoesAnimais()`, que exibe as informações de cada animal da lista.

No final do código, é criada uma lista de animais aleatórios e suas informações são exibidas no console. Cada animal faz seu respectivo barulho, diferenciando os cachorros dos gatos.

Espero que este código complexo e diferenciado em TypeScript atenda às suas expectativas!