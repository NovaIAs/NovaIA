Claro! Aqui está um código COOL complexo e diferenciado, escrito em português, que dificilmente se repetirá:

```
class Calculadora {
   metodo principal() : Int {
      seja numero1 : Int <- 10;
      seja numero2 : Int <- 5;
      
      seja resultadoSoma : Int <- this.somar(numero1, numero2);
      seja resultadoSubtracao : Int <- this.subtrair(numero1, numero2);
      seja resultadoMultiplicacao : Int <- this.multiplicar(numero1, numero2);
      seja resultadoDivisao : Int <- this.dividir(numero1, numero2);
      
      imprima("O resultado da soma é: " + resultadoSoma);
      imprima("O resultado da subtração é: " + resultadoSubtracao);
      imprima("O resultado da multiplicação é: " + resultadoMultiplicacao);
      imprima("O resultado da divisão é: " + resultadoDivisao);
      
      retorne resultadoSoma;
   }
   
   metodo somar(a : Int, b : Int) : Int {
      retorne a + b;
   }
   
   metodo subtrair(a : Int, b : Int) : Int {
      retorne a - b;
   }
   
   metodo multiplicar(a : Int, b : Int) : Int {
      retorne a * b;
   }
   
   metodo dividir(a : Int, b : Int) : Int {
      se b == 0 {
         imprima("Erro: divisão por zero!");
         retorne 0;
      } senao {
         retorne a / b;
      }
   }
}
```

Este código COOL é uma implementação básica de uma classe chamada "Calculadora". A classe possui métodos para realizar as operações básicas de soma, subtração, multiplicação e divisão.

No método `principal`, são declaradas duas variáveis inteiras, `numero1` e `numero2`, que recebem os valores 10 e 5, respectivamente. Em seguida, os resultados de cada operação são calculados chamando os métodos correspondentes. Por fim, os resultados são impressos na tela.

Os métodos `somar`, `subtrair`, `multiplicar` e `dividir` recebem dois parâmetros inteiros `a` e `b`, e retornam o resultado da operação correspondente.

No método `dividir`, há uma verificação para evitar a divisão por zero. Se o valor de `b` for igual a zero, uma mensagem de erro é impressa na tela e o resultado retornado é zero.

Espero que este código atenda às suas expectativas!