Claro, aqui está um exemplo de um código complexo em TypeScript:

```typescript
// Definindo a classe Pessoa
class Pessoa {
  private nome: string;
  private idade: number;
  
  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }
  
  // Método para obter o nome da pessoa
  public getNome(): string {
    return this.nome;
  }
  
  // Método para obter a idade da pessoa
  public getIdade(): number {
    return this.idade;
  }
  
  // Método para verificar se a pessoa é maior de idade
  public isMaiorDeIdade(): boolean {
    return this.idade >= 18;
  }
}

// Criando uma instância da classe Pessoa
const pessoa1 = new Pessoa("João", 25);
const pessoa2 = new Pessoa("Maria", 17);

// Imprimindo os dados da pessoa 1
console.log("Nome: " + pessoa1.getNome());
console.log("Idade: " + pessoa1.getIdade());
console.log("Maior de idade? " + (pessoa1.isMaiorDeIdade() ? "Sim" : "Não"));

// Imprimindo os dados da pessoa 2
console.log("Nome: " + pessoa2.getNome());
console.log("Idade: " + pessoa2.getIdade());
console.log("Maior de idade? " + (pessoa2.isMaiorDeIdade() ? "Sim" : "Não"));
```

Neste exemplo, foi criada uma classe chamada "Pessoa" com os atributos "nome" e "idade". A classe possui um construtor que recebe esses valores e métodos para obter o nome e a idade da pessoa, além de um método para verificar se a pessoa é maior de idade.

Em seguida, são criadas duas instâncias da classe Pessoa, uma representando uma pessoa de 25 anos chamada "João" e outra representando uma pessoa de 17 anos chamada "Maria".

Por fim, os dados das duas pessoas são impressos no console, incluindo o nome, idade e se são maiores de idade ou não.

Espero que esse código atenda às suas expectativas! Se tiver alguma dúvida ou precisar de algo mais, fique à vontade para perguntar.