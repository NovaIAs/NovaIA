**Classe de base**

```cool
classe Base {
    caractéristique num: entier;
    méthode get_num {
        renvoi num;
    }
    méthode set_num (n: entier) {
        num := n;
    }
}
```

**Classe dérivée**

```cool
classe Dérivée hérite de Base {
    caractéristique nom: chaîne;
    méthode get_nom {
        renvoi nom;
    }
    méthode set_nom (n: chaîne) {
        nom := n;
    }
}
```

**Interface**

```cool
interface Iface {
    méthode operation ();
}
```

**Classe implémentant l'interface**

```cool
classe Implémentation implements Iface {
    méthode operation {
        renvoi "Opération effectuée";
    }
}
```

**Classe générique**

```cool
classe Liste [T] {
    caractéristique éléments: [T];
    méthode ajouter (élément: T) {
        éléments := éléments + [élément];
    }
    méthode obtenir (index: entier) {
        renvoi éléments [index];
    }
}
```

**Fonctions anonymes**

```cool
fonction somme (x: entier, y: entier) {
    renvoi x + y;
}

variable f = fonction (x: entier) {
    renvoi x * x;
};
```

**Fermetures**

```cool
classe Fermeture {
    caractéristique compteur: entier;
    méthode fermé (incrément: entier) {
        renvoi fonction () {
            compteur := compteur + incrément;
            renvoi compteur;
        };
    }
}
```

**Classe imbriquée**

```cool
classe ClasseExtérieure {
    caractéristique nom: chaîne;
    classe ClasseIntérieure {
        méthode interne () {
            renvoi nom;
        }
    }
}
```

**Exception**

```cool
exception Erreur {
    méthode erreur () {
        renvoi "Une erreur est survenue";
    }
}

essayer {
    opération_risquée();
} attraper Erreur e {
    afficher (e.erreur());
}
```

**Multithreading**

```cool
classe Verrou {
    caractéristique verrouillé: bool;
    méthode prendre () {
        verrouillé := vrai;
    }
    méthode relâcher () {
        verrouillé := faux;
    }
}

classe Tâche implements Runnable {
    méthode run () {
        verrou.prendre();
        // Exécuter le code partagé
        verrou.relâcher();
    }
}

verrou = nouveau Verrou;
tâche1 = nouveau Tâche(verrou);
tâche2 = nouveau Tâche(verrou);
tâche1.start();
tâche2.start();
```