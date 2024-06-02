```rust
pub fn rendu_image(image: &mut Image, camera: &Camera, scene: &Scene) {
    let largeur_image = image.largeur();
    let hauteur_image = image.hauteur();

    for y in 0..hauteur_image {
        for x in 0..largeur_image {
            let rayon = camera.generer_rayon(x, y);
            let couleur = rendu_rayon(&rayon, scene, 0);
            image.set_pixel(x, y, couleur);
        }
    }
}

pub fn rendu_rayon(rayon: &Rayon, scene: &Scene, profondeur: u32) -> Couleur {
    if profondeur >= scene.profondeur_récursive_max {
        return Couleur::new(0.0, 0.0, 0.0);
    }

    let intersection = scene.intersection_proche(rayon);

    match intersection {
        Some(intersection) => {
            let matériau = &intersection.objet.matériau;
            let position = rayon.at(intersection.t);
            let normale = intersection.objet.normale_à(position);
            let rayon_réfléchi = rayon.réfléchi(&normale, position);
            let rayon_réfracté = rayon.réfracté(&normale, position, matériau.indice_réfraction);

            let couleur_diffuse = matériau.couleur_diffuse * intersection.uv.x * intersection.uv.y;
            let couleur_réfléchie = rendu_rayon(&rayon_réfléchi, scene, profondeur + 1);
            let couleur_réfractée = rendu_rayon(&rayon_réfracté, scene, profondeur + 1);

            couleur_diffuse * matériau.albedo[0] + couleur_réfléchie * matériau.albedo[1] + couleur_réfractée * matériau.albedo[2]
        }
        None => Couleur::new(0.0, 0.0, 0.0),
    }
}
```

**Explication du code :**

Ce code implémente un algorithme de rendu de trajet de rayon pour générer une image à partir d'une scène. Le code est divisé en deux fonctions principales :

* `rendu_image()` : Cette fonction construit l'image en parcourant chaque pixel de l'image et en appelant la fonction `rendu_rayon()` pour générer la couleur de chaque pixel.
* `rendu_rayon()` : Cette fonction récursive trace un rayon à travers la scène et calcule la couleur du pixel correspondant à ce rayon.

**Fonction `rendu_image()` :**

* Elle parcourt chaque pixel de l'image (boucles `for` sur `x` et `y`).
* Pour chaque pixel, elle génère un rayon à partir de la caméra (appel à `camera.generer_rayon()`).
* Elle appelle ensuite `rendu_rayon()` pour obtenir la couleur du pixel correspondant à ce rayon.
* Enfin, elle définit la couleur du pixel dans l'image.

**Fonction `rendu_rayon()` :**

* Elle vérifie si la profondeur de récursion maximale a été atteinte (pour éviter les boucles infinies).
* Elle recherche l'intersection la plus proche entre le rayon et les objets de la scène (appel à `scene.intersection_proche()`).
* Si une intersection est trouvée, elle calcule la couleur du pixel en fonction des propriétés de l'objet intersectif (couleur diffuse, réflexion, réfraction).
* Si aucune intersection n'est trouvée, elle renvoie une couleur noire.

En parcourant chaque pixel et en traçant des rayons, l'algorithme construit progressivement l'image en calculant la couleur de chaque pixel en fonction des interactions des rayons avec la scène.