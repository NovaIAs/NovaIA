**Système de gestion de tournoi multi-joueurs**

```lua
-- Paramètres du tournoi
NB_JOUEURS = 16
NB_TOURS = 4

-- Création de la liste des joueurs
joueurs = {}
for i = 1, NB_JOUEURS do
    joueurs[i] = {}
    joueurs[i].nom = "Joueur " .. i
    joueurs[i].score = 0
end

-- Fonction de comparaison pour le tri des joueurs par score
function comparer_score(a, b)
    return a.score > b.score
end

-- Fonction pour générer des rencontres aléatoires
function generer_rencontres(joueurs)
    rencontres = {}
    local nb_rencontres = #joueurs / 2
    for i = 1, nb_rencontres do
        local joueur1 = joueurs[math.random(#joueurs)]
        local joueur2 = joueurs[math.random(#joueurs)]
        while joueur1 == joueur2 do
            joueur2 = joueurs[math.random(#joueurs)]
        end
        rencontres[i] = {joueur1, joueur2}
    end
    return rencontres
end

-- Fonction pour simuler une rencontre
function simuler_rencontre(joueur1, joueur2)
    if math.random() > 0.5 then
        return joueur1
    else
        return joueur2
    end
end

-- Fonction pour dérouler un tour du tournoi
function jouer_tour(joueurs, rencontres)
    for i = 1, #rencontres do
        local vainqueur = simuler_rencontre(rencontres[i][1], rencontres[i][2])
        vainqueur.score = vainqueur.score + 1
    end
end

-- Fonction pour afficher les résultats d'un tour
function afficher_resultats(joueurs)
    table.sort(joueurs, comparer_score)
    print("Résultats du tour :")
    for i = 1, #joueurs do
        print(i .. ". " .. joueurs[i].nom .. " (" .. joueurs[i].score .. " points)")
    end
end

-- Déroulement du tournoi
for tour = 1, NB_TOURS do
    rencontres = generer_rencontres(joueurs)
    jouer_tour(joueurs, rencontres)
    afficher_resultats(joueurs)
end

-- Affichage du classement final
print("Classement final :")
table.sort(joueurs, comparer_score)
for i = 1, #joueurs do
    print(i .. ". " .. joueurs[i].nom .. " (" .. joueurs[i].score .. " points)")
end
```

**Explication du code :**

* Le tableau `joueurs` contient les informations des joueurs (nom et score).
* La fonction `comparer_score` compare les joueurs par leur score.
* La fonction `generer_rencontres` génère des rencontres aléatoires entre les joueurs.
* La fonction `simuler_rencontre` simule une rencontre entre deux joueurs en générant un résultat aléatoire.
* La fonction `jouer_tour` simule un tour du tournoi en jouant toutes les rencontres et en mettant à jour les scores des joueurs.
* La fonction `afficher_resultats` affiche les résultats d'un tour, triés par score.
* Le code principal génère les rencontres, joue les tours et affiche les résultats.
* Le classement final est affiché une fois que tous les tours sont joués.