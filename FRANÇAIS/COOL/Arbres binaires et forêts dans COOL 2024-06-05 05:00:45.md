**Code COOL**

```cool
-- Définition de la classe **Arbre**
class Arbre {
  attributs:
    valeur: Int;
    gauche: Arbre;
    droite: Arbre;

  méthodes:
    -- Constructeur par défaut
    init(): Arbre {
      valeur <- 0;
      gauche <- null;
      droite <- null;
    }

    -- Insère une valeur dans l'arbre
    insérer(v: Int): void {
      if (v <= valeur) {
        if (gauche == null) {
          gauche <- new Arbre;
          gauche.insérer(v);
        } else {
          gauche.insérer(v);
        }
      } else {
        if (droite == null) {
          droite <- new Arbre;
          droite.insérer(v);
        } else {
          droite.insérer(v);
        }
      }
    }

    -- Recherche une valeur dans l'arbre
    rechercher(v: Int): Boolean {
      if (valeur == v) {
        return true;
      } else if (v <= valeur) {
        if (gauche != null) {
          return gauche.rechercher(v);
        }
      } else {
        if (droite != null) {
          return droite.rechercher(v);
        }
      }

      return false;
    }

    -- Affiche l'arbre
    afficher(): void {
      println(valeur);
      if (gauche != null) {
        gauche.afficher();
      }
      if (droite != null) {
        droite.afficher();
      }
    }
}

-- Définition de la classe **Forêt**
class Forêt {
  attributs:
    arbres: List<Arbre>;

  méthodes:
    -- Constructeur par défaut
    init(): Forêt {
      arbres <- new List<Arbre>;
    }

    -- Ajoute un arbre à la forêt
    ajouter(arbre: Arbre): void {
      arbres.add(arbre);
    }

    -- Recherche une valeur dans la forêt
    rechercher(v: Int): Boolean {
      for (i in arbres) {
        if (i.rechercher(v)) {
          return true;
        }
      }

      return false;
    }

    -- Affiche la forêt
    afficher(): void {
      print("Forêt : ");
      for (i in arbres) {
        print(i.valeur);
      }
      println();
    }
}

-- Création d'un arbre
arbre1 <- new Arbre;
arbre1.insérer(10);
arbre1.insérer(5);
arbre1.insérer(15);
arbre1.insérer(2);
arbre1.insérer(7);
arbre1.insérer(12);
arbre1.insérer(20);

-- Affichage de l'arbre
println("Arbre : ");
arbre1.afficher();
println();

-- Création d'une forêt
forêt1 <- new Forêt;
forêt1.ajouter(arbre1);
forêt1.ajouter(new Arbre);
forêt1.ajouter(new Arbre);

-- Affichage de la forêt
println("Forêt : ");
forêt1.afficher();
```

**Explications**

Ce code définit deux classes : **Arbre** et **Forêt**. La classe **Arbre** représente un arbre binaire de recherche. Elle contient des attributs pour stocker la valeur du nœud, ainsi que des références vers les sous-arbres gauche et droit. Elle possède également des méthodes pour insérer, rechercher et afficher l'arbre.

La classe **Forêt** représente une collection d'arbres. Elle contient un attribut pour stocker une liste d'arbres. Elle possède également des méthodes pour ajouter, rechercher et afficher des arbres dans la forêt.

Le code crée ensuite un arbre binaire de recherche et l'insère dans une forêt. Enfin, il affiche l'arbre et la forêt.