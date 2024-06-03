```java
// Code Java complexe et différencié

import java.util.*;
import java.io.*;
import java.math.*;
import java.util.regex.*;
import java.util.stream.*;

public class CodeComplexe {

    public static void main(String[] args) {

        // Lire les entrées
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        int m = scanner.nextInt();
        int[][] mat = new int[n][m];
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                mat[i][j] = scanner.nextInt();
            }
        }

        // Résoudre le problème
        int[] minRows = new int[n];
        int[] minCols = new int[m];
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                minRows[i] = Math.min(minRows[i], mat[i][j]);
                minCols[j] = Math.min(minCols[j], mat[i][j]);
            }
        }
        int minVal = Arrays.stream(minRows).min().getAsInt();
        List<Integer> minRowIndices = new ArrayList<>();
        List<Integer> minColIndices = new ArrayList<>();
        for (int i = 0; i < n; ++i) {
            if (minRows[i] == minVal) {
                minRowIndices.add(i);
            }
        }
        for (int j = 0; j < m; ++j) {
            if (minCols[j] == minVal) {
                minColIndices.add(j);
            }
        }

        // Écrire les sorties
        System.out.println(minVal);
        System.out.println(minRowIndices.size());
        System.out.println(IntStream.of(minRowIndices.stream().mapToInt(Integer::intValue).toArray()).sorted().toArray());
        System.out.println(minColIndices.size());
        System.out.println(IntStream.of(minColIndices.stream().mapToInt(Integer::intValue).toArray()).sorted().toArray());
    }
}
```

**Explication du code :**

Ce code Java complexe effectue les tâches suivantes :

1. **Lire les entrées:** Lit les entiers `n` et `m` et crée une matrice `mat` de dimensions `n x m`.

2. **Résoudre le problème:**

   - Calcule les valeurs minimales dans chaque ligne (`minRows`) et chaque colonne (`minCols`) de la matrice.
   - Trouve la valeur minimale globale (`minVal`).
   - Identifie les indices des lignes et des colonnes ayant la valeur minimale.

3. **Écrire les sorties:**

   - Imprime la valeur minimale (`minVal`).
   - Imprime le nombre de lignes ayant la valeur minimale.
   - Imprime les indices de ces lignes triés dans l'ordre croissant.
   - Imprime le nombre de colonnes ayant la valeur minimale.
   - Imprime les indices de ces colonnes triés dans l'ordre croissant.