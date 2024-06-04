**Programme de calcul matriciel en LUA**

```lua
-- Module matriciel
local Matrice = {}

-- Fonction pour créer une matrice
function Matrice.creer(lignes, colonnes)
    local matrice = {}
    for i = 1, lignes do
        matrice[i] = {}
        for j = 1, colonnes do
            matrice[i][j] = 0
        end
    end
    return matrice
end

-- Fonction pour additionner deux matrices
function Matrice.additionner(matrice1, matrice2)
    -- Vérifier si les matrices sont de mêmes dimensions
    if matrice1:taille() ~= matrice2:taille() then
        error("Les matrices doivent avoir les mêmes dimensions pour être additionnées")
    end

    -- Créer une nouvelle matrice pour le résultat
    local resultat = Matrice.creer(matrice1:lignes(), matrice1:colonnes())

    -- Itérer sur chaque élément des matrices et les additionner
    for i = 1, matrice1:lignes() do
        for j = 1, matrice1:colonnes() do
            resultat[i][j] = matrice1[i][j] + matrice2[i][j]
        end
    end

    return resultat
end

-- Fonction pour calculer le produit matriciel de deux matrices
function Matrice.produit(matrice1, matrice2)
    -- Vérifier si les dimensions des matrices sont compatibles
    if matrice1:colonnes() ~= matrice2:lignes() then
        error("Les dimensions des matrices ne sont pas compatibles pour le produit matriciel")
    end

    -- Créer une nouvelle matrice pour le résultat
    local resultat = Matrice.creer(matrice1:lignes(), matrice2:colonnes())

    -- Itérer sur les lignes de la première matrice et les colonnes de la deuxième matrice
    for i = 1, matrice1:lignes() do
        for j = 1, matrice2:colonnes() do
            -- Calculer l'élément (i, j) du résultat en faisant la somme des produits des éléments correspondants des deux matrices
            for k = 1, matrice1:colonnes() do
                resultat[i][j] = resultat[i][j] + matrice1[i][k] * matrice2[k][j]
            end
        end
    end

    return resultat
end

-- Méthodes pour obtenir les dimensions de la matrice
function Matrice:lignes()
    return table.getn(self)
end

function Matrice:colonnes()
    return table.getn(self[1])
end

-- Méthode pour obtenir la taille de la matrice (nombre d'éléments)
function Matrice:taille()
    return self:lignes() * self:colonnes()
end

-- Exemple d'utilisation

-- Créer deux matrices
matrice1 = Matrice.creer(3, 2)
matrice2 = Matrice.creer(2, 3)

-- Initialiser les matrices avec des valeurs
for i = 1, matrice1:lignes() do
    for j = 1, matrice1:colonnes() do
        matrice1[i][j] = i * j
    end
end

for i = 1, matrice2:lignes() do
    for j = 1, matrice2:colonnes() do
        matrice2[i][j] = j * i
    end
end

-- Afficher les matrices initiales
print("Matrice 1 :")
for i = 1, matrice1:lignes() do
    print(table.concat(matrice1[i], " "))
end

print("Matrice 2 :")
for i = 1, matrice2:lignes() do
    print(table.concat(matrice2[i], " "))
end

-- Effectuer l'addition des matrices
matriceResultante = Matrice.additionner(matrice1, matrice2)

-- Afficher la matrice résultante
print("Matrice résultante :")
for i = 1, matriceResultante:lignes() do
    print(table.concat(matriceResultante[i], " "))
end

-- Effectuer le produit matriciel des matrices
matriceProduit = Matrice.produit(matrice1, matrice2)

-- Afficher le produit matriciel
print("Produit matriciel :")
for i = 1, matriceProduit:lignes() do
    print(table.concat(matriceProduit[i], " "))
end
```

**Explication :**

Ce code implémente un module pour effectuer des opérations matricielles en LUA. Le module fournit des fonctions pour créer des matrices, les additionner et calculer leur produit matriciel. Le module définit également des méthodes pour obtenir les dimensions des matrices et leur taille.

L'exemple d'utilisation du module crée deux matrices, les initialise avec des valeurs, effectue l'addition et le produit matriciel, puis affiche les matrices résultantes.