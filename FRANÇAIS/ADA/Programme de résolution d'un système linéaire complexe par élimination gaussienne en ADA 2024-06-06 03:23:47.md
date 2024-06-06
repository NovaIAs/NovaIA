**Programme de résolution d'un système linéaire complexe par élimination gaussienne**

```ada
with Ada.Text_IO;
procedure Elimination_Gaussienne is

    type Tableau_Ligne is array (Positive range <>) of Float;
    type Tableau is array (Positive range <>) of Tableau_Ligne;

    MATRICE : constant Tableau := ((1.0, 2.0, 3.0), (4.0, 5.0, 6.0), (7.0, 8.0, 9.0));
    RESULTAT : Tableau;
    PERM : array (Positive range <>) of Positive;

    function Multiplier (L1 : Tableau_Ligne; L2 : Tableau_Ligne; c : Float) return Tableau_Ligne is
    begin
        for I in L1'Range loop
            L1 (I) := L1 (I) * c;
        end loop;
        return L1 + L2;
    end Multiplier;

    procedure Pivoter (I : Positive; J : Positive) is
        AXE_TEMP : Tableau_Ligne;
        TEMP : Positive;
    begin
        AXE_TEMP := RESULTAT (I);
        RESULTAT (I) := RESULTAT (J);
        RESULTAT (J) := AXE_TEMP;
        TEMP := PERM (I);
        PERM (I) := PERM (J);
        PERM (J) := TEMP;
    end Pivoter;

    procedure Eliminer (I : Positive; J : Positive) is
        for K in J + 1 .. RESULTAT'Last loop
            RESULTAT (K) := Multiplier (RESULTAT (K), RESULTAT (I), -MATRICE (K, J) / MATRICE (I, J));
        end loop;
    end Eliminer;

    procedure Resoudre is
        for I in 1 .. RESULTAT'First + RESULTAT'Last - 1 loop
            for J in I + 1 .. RESULTAT'Last loop
                if abs (MATRICE (I, J)) > abs (MATRICE (I, I)) then
                    Pivoter (I, J);
                end if;
                Eliminer (I, J);
            end loop;
        end loop;
    end Resoudre;

    function Afficher_Matrice (M : Tableau) is
        for L in M'Range loop
            for C in L'Range loop
                Ada.Text_IO.Put_Number (M (L, C), Fore => 5, After => 2);
                Ada.Text_IO.Put_Line (" ");
            end loop;
            Ada.Text_IO.New_Line;
        end loop;
    end Afficher_Matrice;

begin
    RESULTAT := MATRICE;
    Perm := (others => 1);
    Resoudre;
    Afficher_Matrice (RESULTAT);
end Elimination_Gaussienne;
```

**Explication du code :**

Ce programme résout un système linéaire complexe en utilisant l'élimination gaussienne. Il effectue les opérations suivantes :

* **Type de tableau :** Définit un type de tableau pour stocker les lignes et les matrices.
* **Matrice constante :** Contient la matrice du système linéaire.
* **Variables :** RESULTAT stocke la matrice augmentée, PERM stocke la permutation des lignes pour le pivotement.
* **Fonction Multiplier :** Multiplie deux lignes et ajoute un coefficient à la première ligne.
* **Procédure Pivoter :** Permute deux lignes et leurs indices de permutation correspondants.
* **Procédure Éliminer :** Élimine les éléments sous un élément pivot à l'aide de la multiplication de lignes.
* **Procédure Résoudre :** Effectue l'élimination gaussienne, en pivotaient et éliminant les éléments.
* **Fonction Afficher_Matrice :** Affiche une matrice de manière lisible.
* **Programme principal :** Initialise RESULTAT, PERM et appelle Résoudre pour résoudre le système.