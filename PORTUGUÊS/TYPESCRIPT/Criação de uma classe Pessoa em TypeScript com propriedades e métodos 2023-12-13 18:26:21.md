Claro! Aqui está um código em TypeScript que cria uma classe chamada "Pessoa" com propriedades e métodos:

```typescript
class Pessoa {
  private nome: string;
  private idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }

  public saudacao(): void {
    console.log(`Olá, meu nome é ${this.nome} e tenho ${this.idade} anos.`);
  }

  public aniversario(): void {
    this.idade++;
    console.log(`Feliz aniversário! Agora tenho ${this.idade} anos.`);
  }
}

const pessoa1 = new Pessoa("João", 25);
pessoa1.saudacao(); // Saída: Olá, meu nome é João e tenho 25 anos.
pessoa1.aniversario(); // Saída: Feliz aniversário! Agora tenho 26 anos.
```

Este código cria uma classe chamada "Pessoa" com duas propriedades privadas: "nome" e "idade". O construtor é responsável por receber o nome e a idade da pessoa e atribuí-los às propriedades correspondentes.

A classe também possui dois métodos públicos: "saudacao" e "aniversario". O método "saudacao" imprime uma mensagem de saudação com o nome e a idade da pessoa. O método "aniversario" incrementa a idade em 1 e imprime uma mensagem de felicitações.

No exemplo, é criado um objeto da classe "Pessoa" chamado "pessoa1" com o nome "João" e a idade "25". Em seguida, são chamados os métodos "saudacao" e "aniversario", que exibem as mensagens correspondentes no console.

Espero que este código atenda às suas expectativas! Se você precisar de mais ajuda, é só me pedir.