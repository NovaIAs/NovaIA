Claro! Aqui está um exemplo de código complexo em TypeScript:

```typescript
// Classe para representar um animal
class Animal {
  private nome: string;
  private idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  public emitirSom(): void {
    console.log(`${this.nome} está emitindo um som.`);
  }

  public envelhecer(anos: number): void {
    this.idade += anos;
    console.log(`${this.nome} envelheceu ${anos} anos.`);
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
    console.log(`${this.nome} está latindo.`);
  }
}

// Função para criar uma lista de cachorros
function criarListaCachorros(): Cachorro[] {
  const listaCachorros: Cachorro[] = [];

  // Adiciona cachorros à lista
  listaCachorros.push(new Cachorro("Rex", 2, "Labrador"));
  listaCachorros.push(new Cachorro("Bob", 4, "Poodle"));
  listaCachorros.push(new Cachorro("Max", 3, "Golden Retriever"));

  return listaCachorros;
}

// Função para envelhecer a lista de cachorros
function envelhecerCachorros(listaCachorros: Cachorro[], anos: number): void {
  console.log(`Envelhecendo cachorros em ${anos} anos:`);

  for (const cachorro of listaCachorros) {
    cachorro.envelhecer(anos);
  }
}

// Função para emitir som de todos os animais da lista
function emitirSomAnimais(listaAnimais: Animal[]): void {
  console.log("Emitindo som dos animais:");

  for (const animal of listaAnimais) {
    animal.emitirSom();
  }
}

// Cria a lista de cachorros
const cachorros: Cachorro[] = criarListaCachorros();

// Envelhece os cachorros em 1 ano
envelhecerCachorros(cachorros, 1);

// Emite som de todos os animais (incluindo os cachorros)
emitirSomAnimais(cachorros);
```

Neste exemplo, temos uma classe `Animal` que representa um animal genérico, com propriedades `nome` e `idade`, e os métodos `emitirSom()` e `envelhecer()`. A classe `Cachorro` herda de `Animal` e adiciona a propriedade `raca` e o método `latir()`.

Em seguida, temos a função `criarListaCachorros()` que cria uma lista de cachorros. Essa lista é passada para a função `envelhecerCachorros()`, que envelhece todos os cachorros em uma determinada quantidade de anos.

Por fim, a função `emitirSomAnimais()` emite o som de todos os animais da lista, incluindo os cachorros.

O código é executado, criando a lista de cachorros, envelhecendo-os em 1 ano e emitindo o som de todos os animais. Os resultados são exibidos no console.