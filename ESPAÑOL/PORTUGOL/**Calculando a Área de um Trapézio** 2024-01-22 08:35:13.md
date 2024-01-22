```portugol
PROGRAMA CalculaAreaTrapezio;

DECLARAÇÃO
  baseMaior: REAL;
  baseMenor: REAL;
  altura: REAL;
  area: REAL;

INÍCIO
  ESCREVA("Informe a base maior do trapézio: ");
  LEIA(baseMaior);
  ESCREVA("Informe a base menor do trapézio: ");
  LEIA(baseMenor);
  ESCREVA("Informe a altura do trapézio: ");
  LEIA(altura);

  area := (baseMaior + baseMenor) * altura / 2;

  ESCREVA("A área do trapézio é: ", area, " unidades quadradas.");
FIM.
```

EXPLICAÇÃO DO CÓDIGO EM PORTUGOL:

- O programa começa com a declaração das variáveis:
  - `baseMaior` e `baseMenor` são variáveis do tipo real que armazenam os valores das bases do trapézio.
  - `altura` é uma variável do tipo real que armazena o valor da altura do trapézio.
  - `area` é uma variável do tipo real que armazena o valor da área do trapézio.

- Em seguida, o programa solicita ao usuário que informe os valores da base maior, da base menor e da altura do trapézio.

- O programa usa a fórmula da área do trapézio, que é `(baseMaior + baseMenor) * altura / 2`, para calcular o valor da área.

- Por fim, o programa exibe o valor da área do trapézio na tela.