```portugol
programa CalculaMediaNotas /* Calcula a média das notas de uma determinada disciplina. */
var
  nomeAluno: cadeia;
  nota1, nota2, nota3: real;
  media: real;
inicio
  escreva("Digite o nome do aluno: ");
  leia(nomeAluno);
  escreva("Digite a primeira nota: ");
  leia(nota1);
  escreva("Digite a segunda nota: ");
  leia(nota2);
  escreva("Digite a terceira nota: ");
  leia(nota3);
  media := (nota1 + nota2 + nota3) / 3;
  se media >= 7 então
    escreva("O aluno ", nomeAluno, " foi aprovado com média ", media);
  senão
    escreva("O aluno ", nomeAluno, " foi reprovado com média ", media);
  fim_se;
fim.
```

Este código calcula a média das notas de uma determinada disciplina e informa se o aluno foi aprovado ou reprovado.

O código começa declarando as variáveis que serão usadas no programa:

* **nomeAluno**: uma variável do tipo cadeia que armazenará o nome do aluno.
* **nota1**, **nota2** e **nota3**: variáveis do tipo real que armazenarão as notas do aluno.
* **media**: uma variável do tipo real que armazenará a média das notas do aluno.

Em seguida, o programa solicita ao usuário que digite o nome do aluno e suas notas.

Depois de receber as notas do aluno, o programa calcula a média das notas usando a seguinte fórmula:

```
media = (nota1 + nota2 + nota3) / 3
```

Em seguida, o programa verifica se a média do aluno é maior ou igual a 7. Se a média for maior ou igual a 7, o aluno é considerado aprovado. Caso contrário, o aluno é considerado reprovado.

Por fim, o programa exibe uma mensagem informando se o aluno foi aprovado ou reprovado.