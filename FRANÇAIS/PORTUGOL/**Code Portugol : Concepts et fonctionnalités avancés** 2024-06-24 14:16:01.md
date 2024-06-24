**Code en Portugol**

```portugol
início
  # Declaração de variáveis
  inteiro a = 10, b = 20, soma;

  # Cálculo da soma
  soma = a + b;

  # Verificação da condição
  se soma >= 30 então
    escreva ("A soma é maior ou igual a 30")
  senão
    escreva ("A soma é menor que 30")
  fim_se

  # Repetição (loop) usando para
  para i = 1 até 10 faça
    escreva (i)
  fim_para

  # Repetição (loop) usando enquanto
  enquanto soma > 0 faça
    soma = soma - 1
    escreva (soma)
  fim_enquanto

  # Repetição (loop) usando repita
  repita
    escreva ("Este é um loop")
  até soma = 0

  # Decisão múltipla (switch)
  semelha(soma)
  de 0 faça
    escreva ("A soma é 0")
  de 10 faça
    escreva ("A soma é 10")
  de 15 faça
    escreva ("A soma é 15")
  de outro_caso
    escreva ("A soma é outro valor")
  fim_semelha

  # Função para calcular o fatorial de um número
  função fatorial (número inteiro n)
    se n <= 1 então
      retorna 1
    senão
      retorna n * fatorial (n - 1)
    fim_se
  fim_função

  # Chamada da função para calcular o fatorial de 5
  escreva ("O fatorial de 5 é ", fatorial (5))

  # Array (vetor) de números inteiros
  vetor numeros [10];

  # Atribuição de valores ao array
  para i = 0 até 9 faça
    numeros[i] = i + 1
  fim_para

  # Exibição dos valores do array
  para i = 0 até 9 faça
    escreva (numeros[i])
  fim_para

  # Estrutura de dados (registro) representando um aluno
  reg Aluno
    cadeia nome
    real media
  fim_reg

  # Declaração do array de alunos
  vetor alunos [10];

  # Atribuição de valores ao array de alunos
  para i = 0 até 9 faça
    alunos[i].nome = "Aluno " + (i + 1)
    alunos[i].media = 7.0 + (i * 0.5)
  fim_para

  # Exibição dos dados dos alunos
  para i = 0 até 9 faça
    escreva ("Aluno: ", alunos[i].nome)
    escreva (" - Média: ", alunos[i].media)
  fim_para

fim
```

**Explicação do Código**

Ce code Portugol est conçu pour être diversifié et complexe, couvrant un large éventail de concepts et de fonctionnalités du langage. Voici une explication détaillée de chaque partie du code :

**Déclaration de variables** : Ce code déclare trois variables entières : `a`, `b` et `soma`.

**Calcul arithmétique** : Le code calcule la somme des variables `a` et `b` et l'enregistre dans la variable `soma`.

**Conditionnelle** : Le code vérifie si la variable `soma` est supérieure ou égale à 30. Si c'est le cas, il affiche un message indiquant que la somme est supérieure ou égale à 30. Sinon, il affiche un message indiquant que la somme est inférieure à 30.

**Boucles** : Ce code utilise trois types de boucles :

* **Boucle `para`** : Elle exécute une instruction de manière répétée pour une plage de valeurs spécifiée.
* **Boucle `enquanto`** : Elle exécute une instruction de manière répétée tant qu'une condition est vraie.
* **Boucle `repita`** : Elle exécute d'abord une instruction, puis vérifie une condition. Si la condition est vraie, elle exécute à nouveau l'instruction.

**Décision multiple (switch)** : Ce code utilise une instruction `semelha` (équivalent à `switch`) pour exécuter différentes actions en fonction de la valeur d'une variable.

**Fonction** : Ce code définit une fonction `fatorial` qui calcule le factoriel d'un nombre.

**Array (vecteur)** : Ce code crée un array (vecteur) de 10 nombres entiers et l'initialise avec les valeurs de 1 à 10.

**Structure de données** : Ce code définit une structure de données (enregistrement) `Aluno` pour représenter un élève avec des champs pour le nom et la moyenne.

**Manipulation de structure de données** : Ce code crée un array d'élèves et l'initialise avec des noms et des moyennes. Il parcourt ensuite l'array et affiche les informations sur chaque élève.

Ce code démontre la puissance et la flexibilité du langage Portugol, couvrant divers concepts et fonctionnalités qui peuvent être utilisés pour résoudre des problèmes complexes.