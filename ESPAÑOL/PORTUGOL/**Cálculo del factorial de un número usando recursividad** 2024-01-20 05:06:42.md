```portuguol
programa código_complexo

função recursiva fatorial(n)
  se n <= 1
    então retorne 1
  senão retorne n * fatorial(n-1);

função main
  número n
  escreva "Digite um número inteiro não negativo:"
  leia n
  se n < 0
    então escreva "Erro: o número digitado deve ser não negativo"
    retorne
  senão escreva "O fatorial de ", n, " é ", fatorial(n);

fim_função

fim_programa
```

Explicação do código:

* A função `fatorial` é uma função recursiva, ou seja, ela se chama a si mesma para resolver o problema. Ela calcula o fatorial de um número inteiro não negativo, que é o produto de todos os números inteiros positivos menores ou iguais a esse número.
* A função `main` é a função principal do programa, que é chamada quando o programa é executado. Ela solicita ao usuário que digite um número inteiro não negativo e, em seguida, chama a função `fatorial` para calcular o fatorial desse número.
* O programa utiliza a estrutura de seleção `se` para verificar se o número digitado pelo usuário é válido (não negativo) e, em caso afirmativo, utiliza a estrutura de saída `escreva` para imprimir o fatorial do número.