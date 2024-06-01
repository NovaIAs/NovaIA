```d
import std.stdio, std.traits, std.algorithm, std.array, std.traits, std.functional, std.string, std.json, std.math, std.utf, std.range, std.container, std.array, std.bigfloat, std.bigint, std.format;
import std.parallelism;

// Définition de types et de fonctions de haut niveau
abstract immutable interface IMembre {
    string nom;
    real age;
    bool estMarié;
}

enum ETypeMembre {
    ETypeMembre.Actif = 0,
    ETypeMembre.Passif = 1,
    ETypeMembre.Honoraire = 2,
}

class Membre implements IMembre {
    string nom;
    real age;
    bool estMarié;
    ETypeMembre typeMembre;
}

auto comparerMembres(const IMembre& a, const IMembre& b) pure body {
    return a.nom > b.nom;
}

// Définition d'une structure de données complexe
struct MembreCollection {
    immutable array[IMembre] membres;

    // Constructeur qui prend un tableau d'IMembre et l'assigne à "membres"
    this(in array[IMembre] m) pure @safe body {
        membres = m;
    }

    // Filtre les membres en fonction d'un prédicat
    immutable array[IMembre] filtrer(Function!(IMembre) @pure bool) pure nothrow body {
        return @safe membres.filter!Predicate(it);
    }

    // Trie les membres en fonction d'un comparateur
    immutable MembreCollection trier(Comparator!(IMembre) @pure) pure nothrow body {
        return @safe MembreCollection(membres.sorted!Comparator(it));
    }

    // Renvoie le nombre de membres
    immutable uint length() const pure nothrow body {
        return @safe membres.length;
    }

    // Surcharge d'opérateur pour l'itération
    immutable foreach(void delegate(IMembre)) const @safe pure nothrow body {
        for (m in membres)
            it.call(m);
    }

    // Surcharge d'opérateur pour l'accès aux éléments
    immutable IMembre opIndex(in uint i) const pure nothrow body {
        return @safe membres[i];
    }

    // Surcharge d'opérateur pour l'assignation d'éléments
    void opAssign(in uint i, in IMembre v) pure nothrow body {
        immutable m = @safe membres[i];
        membres[i] = if (is(m, Membre)) m.cast(Membre) { v : m; };
    }
}

// Exemple d'utilisation du code
auto main() pure nothrow body {
    // Création d'une collection de membres
    auto membres = MembreCollection([
        Membre("Jean", 25, true, ETypeMembre.Actif),
        Membre("Marie", 30, false, ETypeMembre.Passif),
        Membre("Paul", 40, true, ETypeMembre.Honoraire),
    ]);

    // Filtrage des membres actifs
    immutable membresActifs = membres.filtrer(it.typeMembre == ETypeMembre.Actif);

    // Tri des membres par ordre alphabétique
    immutable membresTries = membres.trier(comparerMembres);

    // Affichage du nombre de membres
    writefln("Nombre de membres : %d", membres.length);

    // Itération sur les membres
    writefln("Membres :");
    foreach (m; membres)
        writefln("\t%s (%d ans, %s)", m.nom, m.age, m.estMarié ? "marié" : "célibataire");

    // Accès à un élément de la collection
    immutable marie = membres[1];
    writefln("Nom du deuxième membre : %s", marie.nom);

    // Modification d'un élément de la collection
    membres[1] = Membre("Marie Curie", 45, true, ETypeMembre.Honoraire);
    writefln("Nom du deuxième membre modifié : %s", membres[1].nom);
}
```

**Explication du code :**

Ce code définit des types, des fonctions et une structure de données complexes dans la langage D. Voici une explication de chaque partie :

**Types et fonctions de haut niveau**

* `IMembre` : Interface représentant un membre avec des propriétés comme `nom`, `age`, `estMarié`.
* `ETypeMembre` : Énumération représentant les types de membres.
* `Membre` : Classe implémentant l'interface `IMembre`.
* `comparerMembres` : Fonction pure qui compare deux membres en fonction de leur nom.

**Structure de données complexe : `MembreCollection`**

* `MembreCollection` : Structure contenant un tableau d'objets `IMembre`.
* Constructeur : Crée une instance de `MembreCollection` à partir d'un tableau d'objets `IMembre`.
* `filtrer` : Filtre les membres en fonction d'un prédicat.
* `trier` : Trie les membres en fonction d'un comparateur.
* `length` : Renvoie le nombre de membres.
* Surcharges d'opérateurs :
    * Itération (`foreach`)
    * Accès aux éléments (`opIndex`)
    * Assignation d'éléments (`opAssign`)

**Exemple d'utilisation**

* Création d'une collection de membres.
* Filtrage des membres actifs.
* Tri des membres par ordre alphabétique.
* Affichage du nombre de membres.
* Itération sur les membres.
* Accès et modification d'un membre de la collection.

Ce code illustre l'utilisation de types complexes, de fonctions de haut niveau et d'une structure de données personnalisée pour manipuler des données dans le langage D.