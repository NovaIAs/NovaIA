**Programme de calcul d'équations différentielles partielles elliptiques avec conditions aux limites de Dirichlet**

```portugol
inicio
    # Definindo as dimensões da grade
    inteiro nx, ny, ordem

    # Lendo as dimensões da grade e a ordem da aproximação
    leia(nx, ny, ordem)

    # Alocando memória para a matriz de coeficientes e o vetor de soluções
    real[,] a(nx + 1, ny + 1)
    real[,] b(nx + 1, ny + 1)
    real[,] c(nx + 1, ny + 1)
    real[,] d(nx + 1, ny + 1)
    real[,] f(nx + 1, ny + 1)
    real[,] u(nx + 1, ny + 1)

    # Lendo os coeficientes da equação e o termo fonte
    para i de 1 até nx + 1 faça
        para j de 1 até ny + 1 faça
            leia(a(i, j), b(i, j), c(i, j), d(i, j), f(i, j))
        fim para
    fim para

    # Impondo as condições de contorno de Dirichlet
    para i de 1 até nx + 1 faça
        u(i, 1) = 0.0  # Condição de contorno superior
        u(i, ny + 1) = 0.0  # Condição de contorno inferior
    fim para
    para j de 1 até ny + 1 faça
        u(1, j) = 0.0  # Condição de contorno esquerda
        u(nx + 1, j) = 0.0  # Condição de contorno direita
    fim para

    # Resolvendo o sistema linear usando eliminação gaussiana
    para k de 2 até nx faça
        para i de 2 até ny faça
            # Calculando os coeficientes triangulares
            l_i_k = b(i, k) / a(i, k - 1)
            u_i_k = c(i, k) / a(i, k - 1)

            # Atualizando os coeficientes da equação linear
            para j de k até nx + 1 faça
                a(i, j) = a(i, j) - l_i_k * a(i, k - 1)
                b(i, j) = b(i, j) - l_i_k * b(i, k - 1)
                c(i, j) = c(i, j) - l_i_k * c(i, k - 1)
                d(i, j) = d(i, j) - l_i_k * d(i, k - 1)
            fim para

            # Substituindo u_i_k-1 nas equações
            para j de k até nx + 1 faça
                d(i, j) = d(i, j) - u_i_k * d(i - 1, j)
            fim para
        fim para
    fim para

    # Resolvendo o sistema triangular superior
    para i de ny até 1 passo -1 faça
        para k de nx até 1 passo -1 faça
            u(i, k) = (d(i, k) - b(i, k) * u(i, k + 1) - c(i, k) * u(i, k - 1) - d(i, k) * u(i + 1, k)) / a(i, k)
        fim para
    fim para

    # Imprimindo a solução
    para i de 1 até nx + 1 faça
        para j de 1 até ny + 1 faça
            escreva(u(i, j))
        fim para
    fim para
fim
```

**Explicação do Código:**

Este código resolve equações diferenciais parciais elípticas bidimensionais da forma:

```
L[u] = -∂²u/∂x² - ∂²u/∂y² = f
```

com condições de contorno de Dirichlet.

* **Dimensões da grade:** `nx` e `ny` representam o número de pontos da grade nas direções x e y, respectivamente.

* **Termos da equação:** Os coeficientes `a`, `b`, `c`, `d` e o termo fonte `f` são lidos do arquivo de entrada.

* **Condições de contorno de Dirichlet:** As soluções nos limites superior, inferior, esquerdo e direito são definidas como 0.

* **Eliminação Gaussiana:** O código usa eliminação gaussiana para transformar o sistema linear resultante em uma forma triangular superior.

* **Resolução do sistema triangular superior:** O código resolve o sistema triangular superior para obter a solução `u`.

* **Impressão da solução:** A solução `u` é impressa como uma grade de valores.