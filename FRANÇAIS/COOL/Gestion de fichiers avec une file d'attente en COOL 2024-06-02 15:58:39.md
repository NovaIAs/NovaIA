```cool
classe Fichier {
    entier i;
    entier longueur;
    File t;
    method initialiser(entier i, entier longueur) {
        this.i = i;
        this.longueur = longueur;
        this.t = File.new(longueur + 1);
    }
    method ecrire(entier e) {
        t.ajouterEnQueue(e);
    }
    method lire() {
        si t.vide() alors {
            erreur("Fichier vide");
        } sinon {
            return t.retirerDeQueue();
        }
    }
    method vider() {
        t.vider();
    }
    method rechercher(entier e) {
        entier j;
        j = this.i;
        tant que j <= longueur et t.valeur(j) != e faites {
            j = j + 1;
        }
        si j > longueur alors {
            return -1;
        } sinon {
            return j;
        }
    }
}

classe File {
    entier longueur;
    entier premier;
    entier dernier;
    File t;
    method initialiser(entier longueur) {
        this.longueur = longueur;
        this.premier = 1;
        this.dernier = 0;
        this.t = tableau[longueur + 1] d'entier;
    }
    method vide() {
        return premier > dernier;
    }
    method ajouterEnQueue(entier e) {
        si dernier < longueur alors {
            dernier = dernier + 1;
            t[dernier] = e;
        } sinon {
            erreur("File pleine");
        }
    }
    method retirerDeQueue() {
        entier e;
        si vide() alors {
            erreur("File vide");
        } sinon {
            e = t[premier];
            premier = premier + 1;
            return e;
        }
    }
    method valeur(entier i) {
        si i < 1 ou i > longueur alors {
            erreur("Index hors bornes");
        } sinon {
            return t[i];
        }
    }
    method vider() {
        premier = 1;
        dernier = 0;
    }
}

main {
    Fichier f;
    entier i, j;

    f = Fichier.new(100);
    f.initialiser(1, 100);
    pour i de 1 à 100 faites {
        f.ecrire(i);
    }
    i = 1;
    j = f.rechercher(42);
    tant que j != -1 faites {
        f.vider();
        pour i de 1 à 100 faites {
            f.ecrire(i);
        }
        j = f.rechercher(42);
    }
}
```

**Explications**

Ce code complexe en COOL implémente une classe `Fichier` qui représente un fichier et une classe `File` qui représente une file d'attente.

**Classe Fichier**

La classe `Fichier` représente un fichier contenant une séquence d'entiers. Elle possède les attributs suivants :

* `i` : l'indice du premier élément dans le fichier
* `longueur` : la longueur du fichier
* `t` : la file d'attente d'entiers stockés dans le fichier

La classe fournit les méthodes suivantes :

* `initialiser(i, longueur)` : initialise le fichier avec un indice de départ `i` et une longueur `longueur`
* `ecrire(e)` : écrit l'entier `e` dans le fichier
* `lire()` : lit le prochain entier dans le fichier
* `vider()` : vide le fichier
* `rechercher(e)` : recherche l'entier `e` dans le fichier et renvoie son indice, ou -1 s'il n'est pas trouvé

**Classe File**

La classe `File` implémente une file d'attente d'entiers. Elle possède les attributs suivants :

* `longueur` : la longueur maximale de la file d'attente
* `premier` : l'indice du premier élément dans la file d'attente
* `dernier` : l'indice du dernier élément dans la file d'attente
* `t` : le tableau d'entiers représentant la file d'attente

La classe fournit les méthodes suivantes :

* `initialiser(longueur)` : initialise la file d'attente avec une longueur maximale `longueur`
* `vide()` : renvoie `true` si la file d'attente est vide, `false` sinon
* `ajouterEnQueue(e)` : ajoute l'entier `e` à la fin de la file d'attente
* `retirerDeQueue()` : retire et renvoie le premier entier de la file d'attente
* `valeur(i)` : renvoie l'entier à l'indice `i` dans la file d'attente
* `vider()` : vide la file d'attente

**Fonction Main**

La fonction `main` crée un fichier `f` et y écrit les entiers de 1 à 100. Elle recherche ensuite l'entier 42 dans le fichier et le supprime jusqu'à ce qu'il ne soit plus trouvé.