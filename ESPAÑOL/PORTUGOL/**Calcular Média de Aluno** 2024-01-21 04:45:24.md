```portuguol
program CalcularMédiaAluno
variável nome: cadeia;
variável nota1: real;
variável nota2: real;
variável nota3: real;
variável média: real;

início
  escreva("Informe o nome do aluno: ");
  leia(nome);
  escreva("Informe a primeira nota: ");
  leia(nota1);
  escreva("Informe a segunda nota: ");
  leia(nota2);
  escreva("Informe a terceira nota: ");
  leia(nota3);

  média := (nota1 + nota2 + nota3) / 3;

  se média >= 7 então
    escreva("Aluno ", nome, " aprovado com média ", média, ".");
  senão
    escreva("Aluno ", nome, " reprovado com média ", média, ".");
  fim_se;

fim_programa
```

Explicação:

* Este código calcula a média de um aluno a partir de três notas informadas pelo usuário.
* O programa solicita o nome do aluno, as três notas e, em seguida, calcula a média usando a fórmula `(nota1 + nota2 + nota3) / 3`.
* O programa então verifica se a média é maior ou igual a 7, e imprime uma mensagem informando se o aluno foi aprovado ou reprovado.