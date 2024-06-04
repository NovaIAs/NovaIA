**Code Complexe en PORTUGOL**

```portuqol
SE (A > 10) | (B < 5)
ENTÃO
  SE (C = 1) OU (D = 2)
  ENTÃO
    X := X + 1;
  SENAO
    SE (E <= 10)
    ENTÃO
      Y := Y + 2;
    FIM SE;
  FIM SE;
FIM SE;

ENQUANTO (I <= 100) FAÇA
  SE (J MOD 2 = 0)
  ENTÃO
    K := K + 1;
  FIM SE;
  I := I + 1;
FIM ENQUANTO;

PARA I DE 1 ATÉ 5 FAÇA
  SE (I = 2)
  ENTÃO
    CONTINUE;
  FIM SE;
  L := L + I;
FIM PARA;

CASO (VARIAVEL) DE
  1: EXIBIR "Opção 1 Selecionada";
  2: EXIBIR "Opção 2 Selecionada";
  3: EXIBIR "Opção 3 Selecionada";
  SENAO: EXIBIR "Opção Inválida";
FIM CASO;

FUNCAO Soma (A, B)
  RETORNE A + B;
FIM FUNCAO;

INICIO
  X := 10;
  Y := 5;
  Z := Soma(X, Y);
  EXIBIR Z;
FIM.
```

**Explication du Code**

Ce code PORTUGOL complexe comprend les structures de contrôle, les boucles, les fonctions et les instructions avancées suivantes :

* **Instructions conditionnelles (SE)** : Elles évaluent des conditions et exécutent des instructions en conséquence.
* **Boucle ENQUANTO** : Elle exécute des instructions tant qu'une condition est vraie.
* **Boucle PARA** : Elle exécute des instructions un nombre spécifié de fois.
* **Instruction CASOR** : Elle sélectionne un ensemble d'instructions à exécuter en fonction d'une valeur.
* **Fonction** : Elle encapsule un ensemble d'instructions et peut retourner une valeur.
* **Continue** : Elle saute l'exécution du code restant du bloc actuel et passe à l'itération suivante.