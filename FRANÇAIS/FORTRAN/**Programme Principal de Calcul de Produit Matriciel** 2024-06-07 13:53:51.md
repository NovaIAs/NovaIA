**Programme principal**

```fortran
PROGRAMME Principal
  IMPLICIT NONE

  INTEGER :: n, i, j, k

  ! Déclaration des tableaux
  REAL, DIMENSION(1000,1000) :: A, B, C

  ! Initialisation des tableaux
  DO i = 1, 1000
    DO j = 1, 1000
      A(i,j) = REAL(i + j)
      B(i,j) = REAL(i - j)
    END DO
  END DO

  ! Calcul de la matrice produit
  DO i = 1, 1000
    DO j = 1, 1000
      DO k = 1, 1000
        C(i,j) = C(i,j) + A(i,k) * B(k,j)
      END DO
    END DO
  END DO

  ! Affichage de la matrice produit
  DO i = 1, 1000
    DO j = 1, 1000
      WRITE (*,*) C(i,j)
    END DO
  END DO

END PROGRAMME Principal
```

**Explication du code**

Ce programme en FORTRAN effectue un calcul de produit matriciel entre deux matrices A et B de dimensions 1000x1000. Le résultat est stocké dans la matrice C.

**Déclaration des variables**

* `n` : Taille des matrices A, B et C (définie à 1000 dans cet exemple).
* `i`, `j`, `k` : Indices de boucle.
* `A`, `B`, `C` : Tableaux représentant les matrices A, B et C.

**Initialisation des matrices**

Les matrices A et B sont initialisées avec des valeurs basées sur leurs indices de ligne et de colonne.

**Calcul du produit matriciel**

Le calcul du produit matriciel est effectué à l'aide de boucles imbriquées qui parcourent chaque élément des matrices A et B. Pour chaque élément de C, le produit scalaire des lignes correspondantes de A et des colonnes correspondantes de B est calculé.

**Affichage de la matrice produit**

La matrice produit C est affichée à l'écran à des fins de diagnostic ou de vérification.

Ce code est complexe en raison de sa taille et de sa structure en boucles imbriquées. Il illustre la puissance des boucles et des tableaux en FORTRAN pour les calculs numériques intensifs.