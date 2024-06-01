**Code complexe en PORTUGOL**

**Objectif:** Calculer et afficher la valeur de la somme de tous les nombres premiers inférieurs à un nombre entier positif donné.

**Code:**

```portuguol
programa SomatorioPrimos
   funcao ehPrimo(num)
      se num <= 1
         retorna falso
      senao
         div := 2
         enquanto div * div <= num
            se num % div = 0
               retorna falso
            senao
               div := div + 1
         fim enquanto
         retorna verdadeiro
      fim se
   fim funcao

   var
      num: inteiro
      soma: inteiro

   inicio
      escreva("Digite um número inteiro positivo: ")
      leia(num)

      soma := 0

      para i := 2 ate num
         se ehPrimo(i)
            soma := soma + i
         fim se
      fim para

      escreva("A soma dos números primos menores que ", num, " é: ", soma)
fim programa
```

**Explicação do código:**

* A função **ehPrimo** verifica se um número é primo. Um número é considerado primo se for maior que 1 e não for divisível por nenhum número inteiro positivo diferente de 1 e de si mesmo.

* A variável **num** armazena o número inteiro positivo fornecido pelo usuário.

* A variável **soma** é inicializada com 0 e armazena a soma dos números primos encontrados.

* O loop for itera sobre os números de 2 a **num** e chama a função **ehPrimo** para cada número. Se o número for primo, seu valor é adicionado à variável **soma**.

* Finalmente, o código exibe a soma dos números primos menores que **num**.