```lua
-- Module de gestion des arbres binaires de recherche (ABR)

-- Définition d'un nœud d'ABR
Noeud = {
    valeur = nil,  -- Valeur associée au nœud
    cle = nil,    -- Clé associée au nœud
    gauche = nil, -- Pointeur vers le sous-arbre gauche
    droite = nil  -- Pointeur vers le sous-arbre droit
}

-- Fonction de création d'un ABR vide
function creerABR()
    return nil
end

-- Fonction d'insertion d'un nœud dans un ABR
function insererABR(arbre, valeur, cle)
    if arbre == nil then
        return {valeur = valeur, cle = cle}
    elseif valeur < arbre.valeur then
        -- Insertion dans le sous-arbre gauche
        arbre.gauche = insererABR(arbre.gauche, valeur, cle)
    else
        -- Insertion dans le sous-arbre droit
        arbre.droite = insererABR(arbre.droite, valeur, cle)
    end
    return arbre
end

-- Fonction de recherche d'un nœud dans un ABR
function rechercherABR(arbre, cle)
    if arbre == nil then
        return nil
    elseif arbre.cle == cle then
        return arbre
    elseif cle < arbre.cle then
        -- Recherche dans le sous-arbre gauche
        return rechercherABR(arbre.gauche, cle)
    else
        -- Recherche dans le sous-arbre droit
        return rechercherABR(arbre.droite, cle)
    end
end

-- Fonction de suppression d'un nœud dans un ABR
function supprimerABR(arbre, cle)
    if arbre == nil then
        return nil
    elseif arbre.cle == cle then
        -- Suppression du nœud racine
        if arbre.gauche == nil and arbre.droite == nil then
            return nil
        elseif arbre.gauche == nil then
            return arbre.droite
        elseif arbre.droite == nil then
            return arbre.gauche
        else
            -- Remplacement du nœud supprimé par son successeur
            successeur = arbre.droite
            while successeur.gauche ~= nil do
                successeur = successeur.gauche
            end
            arbre.valeur = successeur.valeur
            arbre.cle = successeur.cle
            arbre.droite = supprimerABR(arbre.droite, successeur.cle)
            return arbre
        end
    elseif cle < arbre.cle then
        -- Suppression dans le sous-arbre gauche
        arbre.gauche = supprimerABR(arbre.gauche, cle)
    else
        -- Suppression dans le sous-arbre droit
        arbre.droite = supprimerABR(arbre.droite, cle)
    end
    return arbre
end

-- Fonction de parcours infixe (gauche, racine, droite) d'un ABR
function parcourirInfixeABR(arbre)
    if arbre == nil then
        return ""
    else
        return parcourirInfixeABR(arbre.gauche) .. " " .. arbre.valeur .. " " .. parcourirInfixeABR(arbre.droite)
    end
end

-- Fonction de parcours préfixe (racine, gauche, droite) d'un ABR
function parcourirPrefixeABR(arbre)
    if arbre == nil then
        return ""
    else
        return arbre.valeur .. " " .. parcourirPrefixeABR(arbre.gauche) .. " " .. parcourirPrefixeABR(arbre.droite)
    end
end

-- Fonction de parcours postfixe (gauche, droite, racine) d'un ABR
function parcourirPostfixeABR(arbre)
    if arbre == nil then
        return ""
    else
        return parcourirPostfixeABR(arbre.gauche) .. " " .. parcourirPostfixeABR(arbre.droite) .. " " .. arbre.valeur
    end
end
```

**Explication du code :**

Ce code fournit un module complet pour la gestion des arbres binaires de recherche (ABR) en langage Lua. Il comprend des fonctions pour créer, insérer, rechercher, supprimer et parcourir des ABR. Voici l'explication détaillée de chaque fonction :

* **creerABR()** : Crée un ABR vide.
* **insererABR(arbre, valeur, cle)** : Insère un nœud avec la valeur et la clé spécifiées dans l'ABR en maintenant la propriété d'ordre des ABR.
* **rechercherABR(arbre, cle)** : Recherche un nœud avec la clé spécifiée dans l'ABR et renvoie le nœud s'il est trouvé, ou nil sinon.
* **supprimerABR(arbre, cle)** : Supprime un nœud avec la clé spécifiée de l'ABR tout en maintenant les propriétés d'ordre des ABR.
* **parcourirInfixeABR(arbre)** : Effectue un parcours infixe (gauche, racine, droite) de l'ABR et renvoie une chaîne représentant les valeurs des nœuds dans l'ordre infixe.
* **parcourirPrefixeABR(arbre)** : Effectue un parcours préfixe (racine, gauche, droite) de l'ABR et renvoie une chaîne représentant les valeurs des nœuds dans l'ordre préfixe.
* **parcourirPostfixeABR(arbre)** : Effectue un parcours postfixe (gauche, droite, racine) de l'ABR et renvoie une chaîne représentant les valeurs des nœuds dans l'ordre postfixe.