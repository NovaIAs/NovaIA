```portuguol
/* PROGRAM: Exercicio 11 - Temperatura e Umidade */

DEFINE
  REAL temperatura, umidade

BEGIN
  /* Entrada de dados */
  ESCREVA("Digite a temperatura:")
  LEIA(temperatura)
  ESCREVA("Digite a umidade:")
  LEIA(umidade)

  /* Avaliar a temperatura */
  SE temperatura < 0:
  ENTON
    ESCREVA("Temperatura muito baixa")
  FIMSE
  SENAO
    SE temperatura >= 0 E temperatura < 10:
    ENTON
      ESCREVA("Temperatura fria")
    FIMSE
    SENAO
      SE temperatura >= 10 E temperatura < 20:
      ENTON
        ESCREVA("Temperatura agradavel")
      FIMSE
      SENAO
        SE temperatura >= 20 E temperatura < 30:
        ENTON
          ESCREVA("Temperatura quente")
        FIMSE
        SENAO
          SE temperatura >= 30:
          ENTON
            ESCREVA("Temperatura muito quente")
          FIMSE
        FIMSE
      FIMSE
    FIMSE
  FIMSE

  /* Avaliar a umidade */
  SE umidade < 30:
  ENTON
    ESCREVA("Umidade muito baixa")
  FIMSE
  SENAO
    SE umidade >= 30 E umidade < 50:
    ENTON
      ESCREVA("Umidade baixa")
    FIMSE
    SENAO
      SE umidade >= 50 E umidade < 70:
      ENTON
        ESCREVA("Umidade agradavel")
      FIMSE
      SENAO
        SE umidade >= 70 E umidade < 90:
        ENTON
          ESCREVA("Umidade alta")
        FIMSE
        SENAO
          SE umidade >= 90:
          ENTON
            ESCREVA("Umidade muito alta")
          FIMSE
        FIMSE
      FIMSE
    FIMSE
  FIMSE

END.
```

**Explanation:**

This program is written in Portugol, a programming language developed in Brazil. It takes two inputs from the user: the temperature and the humidity. It then evaluates these inputs and prints out a message indicating the temperature and humidity conditions.

Here is a brief explanation of the code:

* The program starts by defining the variables `temperatura` and `umidade` as real numbers.
* It then uses the `ESCREVA` and `LEIA` functions to prompt the user to enter the temperature and humidity values.
* The program then uses a series of `SE` statements to evaluate the temperature and humidity values.
* For the temperature, it checks if it is below 0, between 0 and 10, between 10 and 20, between 20 and 30, or above 30. Depending on the range, it prints out a message indicating the temperature condition.
* For the humidity, it checks if it is below 30, between 30 and 50, between 50 and 70, between 70 and 90, or above 90. Depending on the range, it prints out a message indicating the humidity condition.

The program ends by using the `END.` statement to indicate the end of the program.