**Programme complexe en PORTUGOL**

**Objectif :** Résoudre un système d'équations linéaires de grande dimension à l'aide de la méthode d'élimination de Gauss-Jordan.

**Code :**

```portgol
INICIO
	DEFINIR VARIAVEIS
		MATRIZ A(N,N), B(N)
		NULO FLAG
		INICIO I, J, PIVOT

	ESCREVER "Informe a dimensão do sistema (N) : "
	LER N

	ESCREVER "Informe a matriz A : "
	PARA I = 1 ATE N
		PARA J = 1 ATE N
			LER A(I,J)
		FIM
	FIM

	ESCREVER "Informe o vetor B : "
	PARA I = 1 ATE N
		LER B(I)
	FIM

	PARA PIVOT = 1 ATE N
		FLAG = FALSO
		PARA I = PIVOT ATE N
			SE A(I,PIVOT) <> 0
				FLAG = VERDADEIRO
				TROCAR LINHAS I E PIVOT EM A E B
				SAIR PARA
			FIM
		FIM
		SE FLAG = FALSO
			ESCREVER "O sistema não tem solução única."
			TERMINAR
		FIM

		PARA I = 1 ATE N
			SE I <> PIVOT
				FATOR = A(I,PIVOT) / A(PIVOT,PIVOT)
				PARA J = PIVOT ATE N
					A(I,J) = A(I,J) - FATOR * A(PIVOT,J)
				FIM
				B(I) = B(I) - FATOR * B(PIVOT)
			FIM
		FIM
	FIM

	PARA I = 1 ATE N
		A(I,I) = 1
	FIM

	ESCREVER "Solução do sistema : "
	PARA I = 1 ATE N
		ESCREVER "X" I " = " B(I)
	FIM

FIM
```

**Explication :**

**Variables :**

* `A` : Matrice des coefficients du système.
* `B` : Vecteur des termes constants du système.
* `FLAG` : Indicateur pour vérifier si la matrice est singulière.
* `I`, `J`, `PIVOT` : Compteurs de boucle et pivot.

**Algorithme :**

1. **Entrée des données :** Demander à l'utilisateur d'entrer la dimension du système, les coefficients de la matrice et les termes constants.
2. **Élimination de Gauss :**
    * Pour chaque colonne pivot, trouver la première ligne non nulle en dessous.
    * S'il n'y en a pas, le système est singulier et il y a plusieurs solutions.
    * Sinon, échanger les lignes avec la ligne pivot.
    * Pour chaque ligne en dessous de la ligne pivot, soustraire un multiple de la ligne pivot pour annuler les éléments de la colonne pivot.
3. **Réduction de Jordan :**
    * Rendre les éléments diagonaux égaux à 1 en divisant chaque ligne par son élément diagonal.
4. **Affichage de la solution :** Afficher les valeurs des variables inconnues à partir du vecteur `B`.