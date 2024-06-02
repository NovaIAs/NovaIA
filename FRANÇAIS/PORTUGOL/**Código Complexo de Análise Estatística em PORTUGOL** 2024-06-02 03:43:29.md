**Code PORTUGOL Complexe**

```portugol
/* Programa de Análise de Dados Estatísticos */

#definir real
#definir inteiro
#definir boolean

/* Declaração de variáveis */
real media, desvio_padrao, variancia;
inteiro numero_dados;
boolean existem_dados;

/* Função para calcular a média */
funcao real CalcularMedia(inteiro numero_dados, real valores[])
comeco
  real soma = 0;
  para i = 1 ate numero_dados faca
    soma = soma + valores[i];
  fim_para
  retornar soma / numero_dados;
fim_funcao

/* Função para calcular o desvio padrão */
funcao real CalcularDesvioPadrao(inteiro numero_dados, real valores[])
comeco
  real media = CalcularMedia(numero_dados, valores);
  real soma_desvios = 0;
  para i = 1 ate numero_dados faca
    soma_desvios = soma_desvios + (valores[i] - media) * (valores[i] - media);
  fim_para
  retornar sqrt(soma_desvios / (numero_dados - 1));
fim_funcao

/* Função para calcular a variância */
funcao real CalcularVariancia(inteiro numero_dados, real valores[])
comeco
  real desvio_padrao = CalcularDesvioPadrao(numero_dados, valores);
  retornar desvio_padrao * desvio_padrao;
fim_funcao

/* Função principal */
inicio
  /* Leitura do número de dados */
  escreva("Informe o número de dados: ");
  leia(numero_dados);

  /* Verificação se existem dados */
  se numero_dados <= 0 entao
    existem_dados = falso;
  senao
    existem_dados = verdadeiro;
  fim_se

  /* Leitura dos dados */
  se existem_dados entao
    real valores[numero_dados];
    para i = 1 ate numero_dados faca
      escreva("Informe o ", i, "° valor: ");
      leia(valores[i]);
    fim_para

    /* Cálculo da média */
    media = CalcularMedia(numero_dados, valores);

    /* Cálculo do desvio padrão */
    desvio_padrao = CalcularDesvioPadrao(numero_dados, valores);

    /* Cálculo da variância */
    variancia = CalcularVariancia(numero_dados, valores);

    /* Exibição dos resultados */
    escreva("Média: ", media);
    escreva("Desvio Padrão: ", desvio_padrao);
    escreva("Variância: ", variancia);
  senao
    escreva("Não existem dados para análise.");
  fim_se
fim_inicio
```

**Explicação do Código**

Este código é um programa complexo em PORTUGOL que realiza análise estatística em um conjunto de dados. Ele calcula a média, o desvio padrão e a variância dos dados fornecidos pelo usuário.

O código usa funções para encapsular os cálculos estatísticos, tornando o código mais legível e modular. As funções são:

* **CalcularMedia:** Calcula a média de um conjunto de dados.
* **CalcularDesvioPadrao:** Calcula o desvio padrão de um conjunto de dados.
* **CalcularVariancia:** Calcula a variância de um conjunto de dados.

A função principal do programa solicita ao usuário o número de dados e, em seguida, lê os valores dos dados. Ele verifica se há dados disponíveis e, se houver, calcula a média, o desvio padrão e a variância usando as funções definidas. Por fim, exibe os resultados na tela.