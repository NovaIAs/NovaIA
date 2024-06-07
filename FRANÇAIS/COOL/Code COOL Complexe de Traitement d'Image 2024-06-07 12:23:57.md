**Code COOL complexe**

```cool
type AccumulateurImage {
  attribut width : entier;
  attribut height : entier;
  attribut pixels : tableau[width, height] de entier;
  méthode add(image : AccumulateurImage) : Aucun {
    self := self + image;
  }
  méthode +(self : AccumulateurImage, image : AccumulateurImage) : AccumulateurImage {
    nouveau résultat : AccumulateurImage;
    résultat.width := self.width;
    résultat.height := self.height;
    pour (x de 0 jusqu'à self.width, y de 0 jusqu'à self.height) {
      résultat.pixels[x, y] := self.pixels[x, y] + image.pixels[x, y];
    }
    retourner résultat;
  }
}

type TraitementImage {
  attribut image : AccumulateurImage;
  méthode charger(nomFichier : string) : Aucun {
    image := chargerImageDepuisFichier(nomFichier);
  }
  méthode appliquerFiltre(filtre : FiltreImage) : Aucun {
    pour (x de 0 jusqu'à image.width, y de 0 jusqu'à image.height) {
      image.pixels[x, y] := filtre.appliquer(image.pixels[x, y]);
    }
  }
  méthode enregistrer(nomFichier : string) : Aucun {
    enregistrerImageDansFichier(image, nomFichier);
  }
}

type FiltreImage {
  méthode appliquer(pixel : entier) : entier;
}

type FiltreMoyenneur : FiltreImage {
  méthode appliquer(pixel : entier) : entier {
    total := 0;
    compteur := 0;
    pour (x de -1 jusqu'à 1, y de -1 jusqu'à 1) {
      if (0 <= self.x + x < self.width) et (0 <= self.y + y < self.height) {
        total := total + image.pixels[self.x + x, self.y + y];
        compteur := compteur + 1;
      }
    }
    retourner total / compteur;
  }
}

type FiltrePasseHaut : FiltreImage {
  méthode appliquer(pixel : entier) : entier {
    gradient := (pixel - image.pixels[self.x + 1, self.y]) -
                (pixel - image.pixels[self.x - 1, self.y]) +
                (pixel - image.pixels[self.x, self.y + 1]) -
                (pixel - image.pixels[self.x, self.y - 1]);
    retourner pixel + gradient;
  }
}

classe Main {
  méthode main() : Aucun {
    traitement := nouveau TraitementImage;
    traitement.charger("image.png");
    traitement.appliquerFiltre(nouveau FiltreMoyenneur);
    traitement.appliquerFiltre(nouveau FiltrePasseHaut);
    traitement.enregistrer("image_traitee.png");
  }
}
```

**Explications du code**

Ce code est complexe car il utilise plusieurs concepts avancés de COOL, notamment les classes, les méthodes, les types abstraits et les itérateurs.

Voici un résumé de ce que fait le code :

1. Il définit trois classes : `AccumulateurImage`, `TraitementImage` et `FiltreImage`.
2. La classe `AccumulateurImage` représente une accumulation d'images, qui peut être utilisée pour effectuer des opérations telles que l'ajout d'images ou le calcul des moyennes.
3. La classe `TraitementImage` représente un traitement d'image, qui peut être utilisé pour charger une image, lui appliquer des filtres et l'enregistrer.
4. La classe `FiltreImage` représente un filtre d'image, qui peut être appliqué à une image pour modifier ses pixels.
5. La classe `Main` définit une fonction `main`, qui crée une instance de la classe `TraitementImage`, charge une image, lui applique deux filtres (un filtre de moyenne et un filtre passe-haut) et l'enregistre dans un nouveau fichier.