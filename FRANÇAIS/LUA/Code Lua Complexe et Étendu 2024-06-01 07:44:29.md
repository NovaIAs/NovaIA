**Voici un code Lua complexe et étendu qui serait difficilement répété :**

```lua
-- Créer une table avec des données imbriquées
donnees = {
  {"Nom", "Prénom", "Âge"},
  {"Jean", "Dupont", 25},
  {"Marie", "Martin", 30},
}

-- Créer une fonction pour afficher les données
function afficherDonnees(donnees)
  for ligne, donneesLigne in pairs(donnees) do
    for colonne, donneeColonne in pairs(donneesLigne) do
      print(donneeColonne)
    end
    print() -- Saut de ligne
  end
end

-- Créer une fonction pour filtrer les données
function filtrerDonnees(donnees, critere, valeur)
  resultats = {}
  for ligne, donneesLigne in pairs(donnees) do
    if donneesLigne[critere] == valeur then
      resultats[line] = donneesLigne
    end
  end
  return resultats
end

-- Afficher les données initiales
print("Données initiales :")
afficherDonnees(donnees)

-- Filtrer les données par âge
donneesFiltrees = filtrerDonnees(donnees, "Âge", 25)

-- Afficher les données filtrées
print("Données filtrées par âge :")
afficherDonnees(donneesFiltrees)
```

**Explications du code :**

* **Création de données imbriquées (table) :** La table `donnees` contient plusieurs sous-tables, chacune représentant une ligne de données.
* **Fonction d'affichage des données :** La fonction `afficherDonnees` parcourt la table `donnees` et affiche chaque colonne de chaque ligne.
* **Fonction de filtrage des données :** La fonction `filtrerDonnees` renvoie une nouvelle table contenant uniquement les lignes correspondant au critère de filtrage spécifié.
* **Filtrage et affichage des données par âge :** Nous filtrons les `donnees` par âge égal à 25 et affichons les résultats filtrés.

Ce code démontre des fonctionnalités avancées de Lua telles que les tables imbriquées, les fonctions et le filtrage de données.