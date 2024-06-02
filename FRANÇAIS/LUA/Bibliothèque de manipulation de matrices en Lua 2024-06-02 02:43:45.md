```lua
-- ### Bibliothèque pour gérer les matrices ############################

-- ### Créer une matrice ##############################################
matrice = {}
matrice[1] = {1, 2, 3}
matrice[2] = {4, 5, 6}
matrice[3] = {7, 8, 9}

-- ### Imprimer une matrice ###########################################
for i, ligne in pairs(matrice) do
    for j, element in pairs(ligne) do
        io.write(element, "\t")
    end
    io.write("\n")
end

-- ### Ajouter deux matrices ##########################################
function addition_matrices(matrice1, matrice2)
    local resultat = {}

    -- Vérifier que les matrices ont les mêmes dimensions
    if #matrice1 ~= #matrice2 then
        error("Les matrices n'ont pas les mêmes dimensions")
    end

    -- Parcourir les lignes et les colonnes pour effectuer l'addition
    for i = 1, #matrice1 do
        resultat[i] = {}
        for j = 1, #matrice1[i] do
            resultat[i][j] = matrice1[i][j] + matrice2[i][j]
        end
    end

    return resultat
end

-- ### Multiplier une matrice par un scalaire ##########################
function multiplication_scalaire(matrice, scalaire)
    local resultat = {}

    -- Parcourir les lignes et les colonnes pour multiplier chaque élément
    for i = 1, #matrice do
        resultat[i] = {}
        for j = 1, #matrice[i] do
            resultat[i][j] = matrice[i][j] * scalaire
        end
    end

    return resultat
end

-- ### Multiplier deux matrices ########################################
function multiplication_matrices(matrice1, matrice2)
    local resultat = {}

    -- Vérifier que les dimensions sont compatibles
    if #matrice2[1] ~= #matrice1 then
        error("Les dimensions des matrices ne sont pas compatibles pour la multiplication")
    end

    -- Parcourir les lignes de la première matrice et les colonnes de la deuxième
    for i = 1, #matrice1 do
        resultat[i] = {}
        for j = 1, #matrice2[1] do
            resultat[i][j] = 0 -- Initialiser l'élément à 0
            for k = 1, #matrice1[i] do
                resultat[i][j] = resultat[i][j] + matrice1[i][k] * matrice2[k][j]
            end
        end
    end

    return resultat
end

-- ### Déterminant d'une matrice #####################################
function determinant(matrice)
    local n = #matrice
    if n == 1 then
        return matrice[1][1]
    elseif n == 2 then
        return matrice[1][1]*matrice[2][2] - matrice[1][2]*matrice[2][1]
    else
        local det = 0
        for i = 1, n do
            det = det + (-1)^(i+1) * matrice[1][i] * determinant(sous_matrice(matrice, i, 1))
        end
        return det
    end
end

-- ### Sous-matrice (matrice plus petite obtenue en retirant une ligne et une colonne) ################
function sous_matrice(matrice, i, j)
    local sous_matrice = {}
    for k = 1, #matrice do
        if k ~= i then
            sous_matrice[k] = {}
            for l = 1, #matrice[k] do
                if l ~= j then
                    sous_matrice[k][l] = matrice[k][l]
                end
            end
        end
    end
    return sous_matrice
end

-- ### Inverse d'une matrice #########################################
function inverse(matrice)
    local det = determinant(matrice)
    if det == 0 then
        error("La matrice n'est pas inversible")
    end
    local adj = adjointe(matrice)
    for i = 1, #matrice do
        for j = 1, #matrice do
            matrice[i][j] = adj[i][j] / det
        end
    end
    return matrice
end

-- ### Adjointe d'une matrice ########################################
function adjointe(matrice)
    local n = #matrice
    local adj = {}
    for i = 1, n do
        adj[i] = {}
        for j = 1, n do
            adj[i][j] = (-1)^(i+j) * determinant(sous_matrice(matrice, i, j))
        end
    end
    return adj
end

-- ### Résolution d'un système linéaire par la méthode de Gauss #########
function systeme_lineaire(matrice_augmentee)
    local n = #matrice_augmentee

    -- Élimination de Gauss vers l'avant
    for i = 1, n do
        -- Trouver le pivot dans la colonne courante
        local pivot_ligne = i
        for j = i+1, n do
            if math.abs(matrice_augmentee[j][i]) > math.abs(matrice_augmentee[pivot_ligne][i]) then
                pivot_ligne = j
            end
        end

        -- Échanger la ligne pivot avec la ligne courante
        if pivot_ligne ~= i then
            for j = 1, n+1 do
                matrice_augmentee[pivot_ligne][j], matrice_augmentee[i][j] = matrice_augmentee[i][j], matrice_augmentee[pivot_ligne][j]
            end
        end

        -- Soustraire des multiples de la ligne pivot des autres lignes afin de créer des zéros
        for j = 1, n do
            if j ~= i then
                local facteur = matrice_augmentee[j][i] / matrice_augmentee[i][i]
                for k = 1, n+1 do
                    matrice_augmentee[j][k] = matrice_augmentee[j][k] - facteur * matrice_augmentee[i][k]
                end
            end
        end
    end

    -- Élimination de Gauss vers l'arrière
    for i = n, 1, -1 do
        -- Diviser la ligne courante par le coefficient diagonal
        local diviseur = matrice_augmentee[i][i]
        for j = 1, n+1 do
            matrice_augmentee[i][j] = matrice_augmentee[i][j] / diviseur
        end

        -- Soustraire des multiples de la ligne courante des autres lignes afin de créer des zéros
        for j = 1, i-1 do
            local facteur = matrice_augmentee[j][i]
            for k = 1, n+1 do
                matrice_augmentee[j][k] = matrice_augmentee[j][k] - facteur * matrice_augmentee[i][k]
            end
        end
    end

    -- Extraction des solutions
    local solutions = {}
    for i = 1, n do
        solutions[i] = matrice_augmentee[i][n+1]
    end

    return solutions
end

-- ### Exemple d'utilisation ########################################
-- Création d'une matrice
matrice = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}

-- Addition de deux matrices
matrice_additionnee = addition_matrices(matrice, {{10, 11, 12}, {13, 14, 15}, {16, 17, 18}})

-- Multiplication d'une matrice par un scalaire
matrice_multiplicee_scalaire = multiplication_scalaire(matrice, 2)

-- Multiplication de deux matrices
matrice_multiplicee = multiplication_matrices(matrice, {{10, 11, 12}, {13, 14, 15}})

-- Calcul du déterminant
det = determinant(matrice)

-- Calcul de l'inverse
inverse_matrice = inverse(matrice)

-- Calcul de l'adjointe
adjointe_matrice = adjointe(matrice)

-- Résolution d'un système linéaire
systeme_augmentee = {{1, 2, 3, 5}, {4, 5, 6, 7}, {7, 8, 9, 10}}
solutions = systeme_lineaire(systeme