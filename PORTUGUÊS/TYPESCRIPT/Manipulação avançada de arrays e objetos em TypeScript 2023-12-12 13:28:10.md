Claro! Aqui está um exemplo de um código complexo em TypeScript que envolve a manipulação de arrays e objetos:

```typescript
// Definir a classe "Pessoa" com propriedades nome e idade
class Pessoa {
  nome: string;
  idade: number;

  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }
}

// Criar um array de pessoas
const pessoas: Pessoa[] = [
  new Pessoa("João", 25),
  new Pessoa("Maria", 30),
  new Pessoa("Pedro", 20),
  new Pessoa("Ana", 35),
  new Pessoa("Carlos", 40)
];

// Filtrar pessoas com idade maior ou igual a 30
const pessoasComIdadeMaiorOuIgualA30 = pessoas.filter(pessoa => pessoa.idade >= 30);

// Mapear nomes das pessoas filtradas
const nomesPessoasComIdadeMaiorOuIgualA30 = pessoasComIdadeMaiorOuIgualA30.map(pessoa => pessoa.nome);

// Criar um objeto com as pessoas filtradas e mapeadas
const objetoFinal = {
  totalPessoas: pessoas.length,
  pessoasFiltradas: pessoasComIdadeMaiorOuIgualA30.length,
  nomesPessoasFiltradas: nomesPessoasComIdadeMaiorOuIgualA30
};

// Imprimir o objeto final no console
console.log(objetoFinal);
```

Neste código, temos uma classe "Pessoa" que possui propriedades de nome e idade. Em seguida, criamos um array de pessoas com diferentes nomes e idades.

Utilizamos os métodos `filter` e `map` para filtrar e mapear as pessoas com idade maior ou igual a 30. Em seguida, criamos um objeto final que contém informações sobre o total de pessoas, o número de pessoas filtradas e uma lista com os nomes das pessoas filtradas.

Por fim, imprimimos o objeto final no console. Este código exemplifica a utilização de alguns conceitos avançados do TypeScript, como classes, arrays, métodos de array e manipulação de objetos.