```portugol

/* PROGRAMA: Calcular a área de um retângulo */

VAR
  comprimento, largura, area: REAL;

INICIO
  ESCREVA("Digite o comprimento do retângulo: ");
  LEIA(comprimento);
  ESCREVA("Digite a largura do retângulo: ");
  LEIA(largura);
  area := comprimento * largura;
  ESCREVA("A área do retângulo é: ", area);
FIM.

/* PROGRAMA: Calcular a média de três notas */

VAR
  nota1, nota2, nota3, media: REAL;

INICIO
  ESCREVA("Digite a primeira nota: ");
  LEIA(nota1);
  ESCREVA("Digite a segunda nota: ");
  LEIA(nota2);
  ESCREVA("Digite a terceira nota: ");
  LEIA(nota3);
  media := (nota1 + nota2 + nota3) / 3;
  ESCREVA("A média das notas é: ", media);
FIM.

/* PROGRAMA: Calcular o fatorial de um número */

VAR
  numero, fatorial: INTEIRO;

INICIO
  ESCREVA("Digite um número inteiro positivo: ");
  LEIA(numero);
  fatorial := 1;
  PARA i DE 1 ATE numero FAÇ
    fatorial := fatorial * i;
  FIM PARA;
  ESCREVA("O fatorial de ", numero, " é: ", fatorial);
FIM.

/* PROGRAMA: Verificar se um número é primo */

VAR
  numero, contador: INTEIRO;

INICIO
  ESCREVA("Digite um número inteiro positivo: ");
  LEIA(numero);
  contador := 0;
  PARA i DE 1 ATE numero FAÇ
    SE numero MOD i = 0 ENTAO
      contador := contador + 1;
    FIM SE;
  FIM PARA;
  SE contador = 2 ENTAO
    ESCREVA(numero, " é um número primo.");
  SENAO
    ESCREVA(numero, " não é um número primo.");
  FIM SE;
FIM.

/* PROGRAMA: Imprimir a tabuada de um número */

VAR
  numero, i: INTEIRO;

INICIO
  ESCREVA("Digite um número inteiro positivo: ");
  LEIA(numero);
  PARA i DE 1 ATE 10 FAÇ
    ESCREVA(numero, " x ", i, " = ", numero * i);
  FIM PARA;
FIM.

```