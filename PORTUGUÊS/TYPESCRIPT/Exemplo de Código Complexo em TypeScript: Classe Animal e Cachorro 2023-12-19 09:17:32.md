Claro! Aqui está um exemplo de código complexo em TypeScript:

```typescript
// Classe para representar um Animal
class Animal {
  private nome: string;
  private idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  // Método para retornar uma saudação do animal
  saudacao(): string {
    return `Olá, meu nome é ${this.nome} e eu tenho ${this.idade} anos.`;
  }
}

// Classe para representar um Cachorro, que herda de Animal
class Cachorro extends Animal {
  private raca: string;

  constructor(nome: string, idade: number, raca: string) {
    super(nome, idade);
    this.raca = raca;
  }

  // Método para latir
  latir(): string {
    return "Au au!";
  }

  // Sobrescrevendo o método saudacao
  saudacao(): string {
    return `Olá, meu nome é ${this.nome}, eu tenho ${this.idade} anos e minha raça é ${this.raca}.`;
  }
}

// Função para criar e interagir com um Cachorro
function interagirComCachorro(): void {
  const nome: string = prompt("Digite o nome do cachorro:");
  const idade: number = parseInt(prompt("Digite a idade do cachorro:"));
  const raca: string = prompt("Digite a raça do cachorro:");

  const cachorro: Cachorro = new Cachorro(nome, idade, raca);

  console.log(cachorro.saudacao());
  console.log(cachorro.latir());
}

interagirComCachorro();
```

Neste exemplo, temos duas classes: `Animal` e `Cachorro`. A classe `Cachorro` herda da classe `Animal`. A classe `Animal` possui um construtor que recebe o nome e a idade do animal, bem como um método `saudacao` que retorna uma saudação do animal.

A classe `Cachorro` adiciona um atributo `raca` ao construtor, e possui um método `latir` que retorna a string "Au au!". Além disso, sobrescreve o método `saudacao` para incluir também a raça.

A função `interagirComCachorro` é responsável por criar um novo objeto `Cachorro` com base nas informações fornecidas pelo usuário e interagir com ele, exibindo a saudação e o latido.

Espero que este código atenda às suas expectativas!