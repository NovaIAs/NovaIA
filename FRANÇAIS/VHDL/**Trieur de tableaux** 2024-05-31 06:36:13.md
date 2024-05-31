**Module de tri de tableau**

```vhdl
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity TrieurTableau is
    generic (
        E : Positive;
        L : Positive := 1
    );
    port (
        Tableau : inout array(L - 1 downto 0) of E;
        Trié : out Boolean
    );
end entity;

architecture Synthèse of TrieurTableau is
    type Etat is (
        INIT, SORT, FIN
    );

    signal EtatCourant : Etat := INIT;

    procedure Echanger(in out Tableau : array(L - 1 downto 0) of E; in i, j : natural) is
    begin
        Tableau(i) := Tableau(i) xor Tableau(j);
        Tableau(j) := Tableau(i) xor Tableau(j);
        Tableau(i) := Tableau(i) xor Tableau(j);
    end procedure;

begin
    Trié <= '0';

    process
    begin
        loop
            case EtatCourant is
                when INIT =>
                    if Tableau(L - 1) > Tableau(L - 2) then
                        Echanger(Tableau, L - 1, L - 2);
                    end if;
                    EtatCourant <= SORT;
                when SORT =>
                    for i in L - 2 downto 0 loop
                        if Tableau(i) > Tableau(i + 1) then
                            Echanger(Tableau, i, i + 1);
                        end if;
                    end loop;
                    EtatCourant <= FIN;
                when FIN =>
                    Trié <= '1';
                    EtatCourant <= INIT;
            end case;
        end loop;
    end process;
end architecture;
```

**Explication**

Ce code VHDL implémente un module qui trie un tableau d'entiers en utilisant l'algorithme du tri à bulles. Voici une explication du code :

* **Génériques** :

    * **E** : Type des éléments du tableau.
    * **L** : Longueur du tableau (par défaut 1).

* **Ports** :

    * **Tableau** : Tableau d'entrée/sortie contenant les éléments à trier.
    * **Trié** : Signal de sortie qui indique si le tableau est trié.

* **Architecture Synthèse** :

    * **Type Etat** : Type énuméré pour suivre l'état du processus de tri.
    * **ÉtatCourant** : Signal qui stocke l'état actuel.
    * **Echanger** : Procédure pour échanger deux éléments du tableau.
    * **Process** : Processus principal qui effectue le tri :
        * Il commence par l'état **INIT** où il échange éventuellement les deux derniers éléments du tableau.
        * Il passe ensuite à l'état **SORT** où il effectue plusieurs passes sur le tableau, échangeant les éléments adjacents s'ils sont dans l'ordre incorrect.
        * Une fois le tableau trié, il passe à l'état **FIN** et signale que le tableau est trié via le signal **Trié**.
        * Enfin, il retourne à l'état **INIT** pour gérer de nouveaux tableaux à trier.