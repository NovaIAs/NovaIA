```lua
-- Déclaration des variables de table
local tab = {}

-- Affectation de valeurs à des clés spécifiques
tab["clé 1"] = "valeur 1"
tab["clé 2"] = {
    ["sous-clé 1"] = "valeur 2.1",
    ["sous-clé 2"] = "valeur 2.2",
}

-- Itération sur les clés de la table
for key, value in pairs(tab) do
    print("Clé : " .. key .. ", Valeur : " .. value)
end

-- Déclaration de la fonction
function maFonction(param1, param2, ...)
    -- Code de la fonction utilisant les paramètres et les arguments variables
    print("param1 : " .. param1)
    print("param2 : " .. param2)

    -- Itération sur les arguments variables
    for i, arg in ipairs({...}) do
        print("Argument " .. i .. " : " .. arg)
    end
end

-- Appel de la fonction avec des arguments
maFonction("argument 1", "argument 2", "argument 3", "argument 4")

-- Définition d'une méta-table pour la table tab
setmetatable(tab, {
    __index = function(self, key)
        -- Retourne une valeur par défaut pour les clés manquantes
        return "Clé introuvable : " .. key
    end,
    __newindex = function(self, key, value)
        -- Intercepte les affectations à des clés inexistantes
        error("Impossible de créer une nouvelle clé : " .. key)
    end,
})

-- Essaie d'accéder à une clé inexistante
print(tab["clé inexistante"])

-- Essaie d'ajouter une nouvelle clé
tab["nouvelle clé"] = "nouvelle valeur"
```

**Explication :**

* Ce code illustre des fonctionnalités avancées de Lua, notamment :
    * Tables avec des clés et des valeurs complexes
    * Itération sur les tables et les arguments variables
    * Fonctions avec des arguments de longueur variable
    * Méta-tables pour personnaliser le comportement des tables
* Le code utilise une méta-table pour intercepter les accès aux clés inexistantes et les tentatives d'ajout de nouvelles clés.
* La fonction `maFonction` utilise des arguments de longueur variable pour accepter un nombre indéfini d'arguments.
* La table `tab` est personnalisée pour retourner une valeur par défaut pour les clés manquantes et pour empêcher la création de nouvelles clés.