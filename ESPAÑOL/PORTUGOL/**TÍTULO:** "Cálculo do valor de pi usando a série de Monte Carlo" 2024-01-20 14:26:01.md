```portuguol
programmatas Complexa
( ********************************************************** *)
( *** Programa que calcula o valor de pi usando a série de Monte Carlo *** )
( ********************************************************** *)
inteiro nr, i, pontosDentro
real pi
pontosDentro := 0
nr := 1000000
para i := 1 até nr fazer
    real x, y
    x := SorteiaReal(0.0, 1.0)
    y := SorteiaReal(0.0, 1.0)
    se (x * x + y * y) <= 1.0 então
        pontosDentro := pontosDentro + 1
    fim
fimpara
pi := 4.0 * Real(pontosDentro) / Real(nr)
imprime(pi)
fimprogramatas
```

**Explicação:**

* O programa calcula o valor de pi usando a série de Monte Carlo.
* A série de Monte Carlo é um método para estimar o valor de uma integral calculando a média de uma série de valores aleatórios.
* No caso do cálculo do valor de pi, a integral a ser calculada é:

```
pi = 4 * integral(sqrt(1 - x^2), x, 0, 1)
```

* O programa gera um grande número de pontos aleatórios dentro do quadrado unitário e conta quantos desses pontos estão dentro do círculo unitário.
* A razão entre o número de pontos dentro do círculo e o número total de pontos gerados é uma estimativa do valor da integral.
* Multiplicando essa estimativa por 4, obtém-se o valor estimado de pi.
* O programa usa a função `SorteiaReal` para gerar números reais aleatórios.
* A função `SorteiaReal` recebe dois números reais como parâmetros e retorna um número real aleatório entre esses dois números.
* A função `imprime` é usada para imprimir o valor de pi na tela.