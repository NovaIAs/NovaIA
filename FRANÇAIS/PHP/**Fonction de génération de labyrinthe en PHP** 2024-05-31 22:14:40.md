**Fonction de génération de labyrinthe**

```php
<?php
function generer_labyrinthe($largeur, $hauteur) {
    // Initialiser le tableau de cellules
    $labyrinthe = array();
    for ($i = 0; $i < $largeur; $i++) {
        for ($j = 0; $j < $hauteur; $j++) {
            $labyrinthe[$i][$j] = array(
                'etat' => 'mur',
                'visitee' => false
            );
        }
    }

    // Choisir une cellule de départ aléatoire
    $x = rand(0, $largeur - 1);
    $y = rand(0, $hauteur - 1);
    creuser_cellule($labyrinthe, $x, $y);

    // Créer un tas pour stocker les cellules à visiter
    $tas = array();
    tas_push($tas, array($x, $y));

    // Tant qu'il y a des cellules à visiter
    while (!tas_est_vide($tas)) {
        // Récupérer la cellule courante
        list($x, $y) = tas_pop($tas);

        // Marquer la cellule comme visitée
        $labyrinthe[$x][$y]['visitee'] = true;

        // Obtenir les cellules voisines non visitées
        $voisins = obtenir_voisins($labyrinthe, $x, $y);

        // Choisir une cellule voisine aléatoire
        $voisin_choisi = $voisins[rand(0, count($voisins) - 1)];
        $x_voisin = $voisin_choisi[0];
        $y_voisin = $voisin_choisi[1];

        // Creuser un passage entre la cellule courante et la cellule voisine
        creuser_passage($labyrinthe, $x, $y, $x_voisin, $y_voisin);

        // Ajouter la cellule voisine au tas
        tas_push($tas, array($x_voisin, $y_voisin));
    }

    // Renvoyer le labyrinthe
    return $labyrinthe;
}
```

**Fonctions de manipulation du tas**

```php
function tas_est_vide($tas) {
    return count($tas) == 0;
}

function tas_push($tas, $element) {
    $tas[] = $element;
    tas_trier($tas);
}

function tas_pop($tas) {
    $element = $tas[0];
    unset($tas[0]);
    $tas = array_values($tas);
    tas_trier($tas);
    return $element;
}

function tas_trier($tas) {
    for ($i = floor(count($tas) / 2) - 1; $i >= 0; $i--) {
        tas_ajuster($tas, $i);
    }
}

function tas_ajuster($tas, $index) {
    $gauche = 2 * $index + 1;
    $droite = 2 * $index + 2;
    $min = $index;

    if ($gauche < count($tas) && $tas[$gauche] < $tas[$min]) {
        $min = $gauche;
    }
    if ($droite < count($tas) && $tas[$droite] < $tas[$min]) {
        $min = $droite;
    }

    if ($min != $index) {
        $temp = $tas[$index];
        $tas[$index] = $tas[$min];
        $tas[$min] = $temp;
        tas_ajuster($tas, $min);
    }
}
```

**Fonctions de manipulation du labyrinthe**

```php
function creuser_cellule($labyrinthe, $x, $y) {
    $labyrinthe[$x][$y]['etat'] = 'chemin';
}

function creuser_passage($labyrinthe, $x1, $y1, $x2, $y2) {
    $labyrinthe[($x1 + $x2) / 2][($y1 + $y2) / 2]['etat'] = 'chemin';
}

function obtenir_voisins($labyrinthe, $x, $y) {
    $voisins = array();

    if ($x > 0 && $labyrinthe[$x - 1][$y]['etat'] == 'mur' && !$labyrinthe[$x - 1][$y]['visitee']) {
        $voisins[] = array($x - 1, $y);
    }
    if ($x < count($labyrinthe) - 1 && $labyrinthe[$x + 1][$y]['etat'] == 'mur' && !$labyrinthe[$x + 1][$y]['visitee']) {
        $voisins[] = array($x + 1, $y);
    }
    if ($y > 0 && $labyrinthe[$x][$y - 1]['etat'] == 'mur' && !$labyrinthe[$x][$y - 1]['visitee']) {
        $voisins[] = array($x, $y - 1);
    }
    if ($y < count($labyrinthe[0]) - 1 && $labyrinthe[$x][$y + 1]['etat'] == 'mur' && !$labyrinthe[$x][$y + 1]['visitee']) {
        $voisins[] = array($x, $y + 1);
    }

    return $voisins;
}
```

**Utilisation**

```php
$labyrinthe = generer_labyrinthe(50, 50);

// Afficher le labyrinthe
for ($i = 0; $i < count($labyrinthe); $i++) {
    for ($j = 0; $j < count($labyrinthe[0]); $j++) {
        echo $labyrinthe[$i][$j]['etat'] . ' ';
    }
    echo '<br>';
}
```