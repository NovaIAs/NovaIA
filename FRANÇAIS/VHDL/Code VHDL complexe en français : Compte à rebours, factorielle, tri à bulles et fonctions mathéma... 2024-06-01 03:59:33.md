```vhdl
-- Code VHDL complexe en français

-- Définition d'un processus de comptage à rebours
-- Le processus décrémente la variable "compteur" d'une unité toutes les secondes

process (horloge)
begin
    if rising_edge(horloge) then
        if compteur > 0 then
            compteur := compteur - 1;
        end if;
    end if;
end process;

-- Définition d'une fonction de calcul de factorielle
-- La fonction calcule la factorielle du nombre entier "n"

function factorielle(n : integer) return integer is
begin
    if n = 0 then
        return 1;
    else
        return n * factorielle(n - 1);
    end if;
end function factorielle;

-- Définition d'une architecture pour un composant de tri d'un tableau d'entiers
-- L'architecture utilise l'algorithme de tri à bulles

architecture tri for composant tri_tableau is
begin

    process (horloge)
    begin
        if rising_edge(horloge) then
            for i in tableau'range loop
                for j in i+1 to tableau'high loop
                    if tableau(i) > tableau(j) then
                        echanger(tableau(i), tableau(j));
                    end if;
                end loop;
            end loop;
        end if;
    end process;

    function echanger(inout a : integer; inout b : integer) is
    begin
        variable tmp : integer;
        tmp := a;
        a := b;
        b := tmp;
    end function echanger;

end architecture tri;

-- Définition d'un package contenant des fonctions mathématiques
-- Le package fournit des fonctions pour le calcul de sinus, cosinus et tangente

package fonctions_mathematiques is

    function sinus(x : real) return real;
    function cosinus(x : real) return real;
    function tangente(x : real) return real;

end package fonctions_mathematiques;

-- Corps du package contenant les implémentations des fonctions
-- Les fonctions utilisent des approximations polynomiales pour calculer les valeurs trigonométriques

package body fonctions_mathematiques is

    function sinus(x : real) return real is
    begin
        return x - x**3 / 3! + x**5 / 5! - x**7 / 7! + x**9 / 9!;
    end function sinus;

    function cosinus(x : real) return real is
    begin
        return 1 - x**2 / 2! + x**4 / 4! - x**6 / 6! + x**8 / 8!;
    end function cosinus;

    function tangente(x : real) return real is
    begin
        return sinus(x) / cosinus(x);
    end function tangente;

end package body fonctions_mathematiques;
```

**Explications du code :**

* Le premier processus implémente un compte à rebours en décrémentant la variable "compteur" de 1 toutes les secondes.
* La fonction "factorielle" calcule la factorielle d'un nombre entier "n" en utilisant la récursivité.
* L'architecture "tri" implémente l'algorithme de tri à bulles pour trier un tableau d'entiers.
* Le package "fonctions_mathematiques" fournit des fonctions pour calculer le sinus, le cosinus et la tangente à l'aide d'approximations polynomiales.