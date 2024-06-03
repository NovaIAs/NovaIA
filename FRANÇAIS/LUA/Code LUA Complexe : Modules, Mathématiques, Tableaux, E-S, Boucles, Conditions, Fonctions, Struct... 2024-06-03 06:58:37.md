**Code LUA Complexe**

```lua
-- Déclaration de modules
local table = {}
local math = {}
local io = {}

-- Fonctions mathématiques
math.addition = function(a, b)
  return a + b
end
math.soustraction = function(a, b)
  return a - b
end
math.multiplication = function(a, b)
  return a * b
end
math.division = function(a, b)
  return a / b
end

-- Tableaux
table.creer = function()
  return {}
end
table.ajouter = function(table, element)
  table[#table + 1] = element
end
table.obtenir = function(table, index)
  return table[index]
end

-- E/S
io.ecrire = function(texte)
  print(texte)
end
io.lire = function()
  return io.read()
end

-- Boucles
local i = 1
while i <= 10 do
  io.ecrire(i)
  i = i + 1
end

for nom, prenom in pairs({nom = "Dupont", prenom = "Jean"}) do
  io.ecrire(nom .. " " .. prenom)
end

-- Conditions
if i == 10 then
  io.ecrire("i vaut 10")
elseif i > 10 then
  io.ecrire("i est supérieur à 10")
else
  io.ecrire("i est inférieur à 10")
end

-- Fonctions
function saluer(nom)
  io.ecrire("Bonjour " .. nom)
end
saluer("Dupont")

-- Structures de données avancées
local tree = {}
tree.racine = {}
tree.inserer = function(noeud, valeur)
  if valeur < noeud.valeur then
    if noeud.gauche == nil then
      noeud.gauche = {valeur = valeur}
    else
      tree.inserer(noeud.gauche, valeur)
    end
  else
    if noeud.droite == nil then
      noeud.droite = {valeur = valeur}
    else
      tree.inserer(noeud.droite, valeur)
    end
  end
end

tree.afficher = function(noeud)
  if noeud == nil then
    return
  end
  tree.afficher(noeud.gauche)
  io.ecrire(noeud.valeur)
  tree.afficher(noeud.droite)
end

-- Exemple d'utilisation de la structure de données arborescente
local arbre = tree.creer()
arbre.inserer(arbre.racine, 10)
arbre.inserer(arbre.racine, 15)
arbre.inserer(arbre.racine, 5)
arbre.inserer(arbre.racine, 3)
arbre.inserer(arbre.racine, 7)
arbre.afficher(arbre.racine)
```

**Explication du code**

Ce code LUA complexe couvre une large gamme de sujets, notamment :

* **Modules** : Les modules sont utilisés pour organiser le code et encapsuler les fonctionnalités.
* **Fonctions mathématiques** : Fonctions de base pour effectuer des opérations mathématiques courantes.
* **Tableaux** : Types de données de tableau pour stocker et manipuler des collections de données.
* **E/S** : Fonctions pour lire et écrire des informations depuis et vers la console.
* **Boucles** : Boucles `while` et `for` pour itérer sur des séquences.
* **Conditions** : Instructions conditionnelles `if-else` pour exécuter du code en fonction de conditions.
* **Fonctions** : Définition et appel de fonctions personnalisées.
* **Structures de données avancées** : Arbres binaires pour stocker des données de manière hiérarchique.

Ce code complexe démontre la puissance et la flexibilité du langage LUA pour créer des programmes avancés.