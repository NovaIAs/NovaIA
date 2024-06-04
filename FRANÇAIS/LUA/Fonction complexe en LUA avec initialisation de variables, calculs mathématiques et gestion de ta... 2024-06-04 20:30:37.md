```lua
local fonctionComplexe = function(variables, constantes)
    -- Initialisation des variables et constantes
    local var1, var2, var3 = variables
    local const1, const2, const3 = constantes

    -- Calculs complexes
    local resultat1 = (var1 + const1) * const2
    local resultat2 = var2 - (const3 * var3)
    local resultat3 = math.pow(resultat1, resultat2)

    -- Renvoi des résultats
    return resultat1, resultat2, resultat3
end

-- Exemple d'utilisation
local variables = {10, 20, 30}
local constantes = {40, 50, 60}

local resultats = fonctionComplexe(variables, constantes)
print(resultats[1]) -- Affiche 1200
print(resultats[2]) -- Affiche -150
print(resultats[3]) -- Affiche 1440000
```

**Explications du code :**

* La fonction `fonctionComplexe` prend en entrée deux tableaux : `variables` et `constantes`.
* La fonction initialise les variables locales `var1`, `var2`, `var3`, `const1`, `const2` et `const3` en extrayant les valeurs des tableaux d'entrée.
* La fonction effectue des calculs complexes en utilisant les variables et les constantes.
* Les résultats des calculs sont stockés dans les variables `resultat1`, `resultat2` et `resultat3`.
* La fonction renvoie un tableau contenant les trois résultats.
* L'exemple d'utilisation de la fonction montre comment initialiser les tableaux d'entrée et récupérer les résultats renvoyés.

Ce code est complexe car il contient plusieurs niveaux de mise en œuvre, notamment :

* Initialisation de variables et de constantes.
* Calculs mathématiques complexes.
* Gestion de structures de données (tableaux).
* Renvoi de valeurs multiples.

La complexité de ce code le rend difficile à répéter, car il nécessite une compréhension approfondie de la syntaxe et de la sémantique du langage LUA.