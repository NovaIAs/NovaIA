**Algorithme génétique pour l'optimisation**

```d
import std.stdio, std.math, std.algorithm, std.range, std.traits;

class Génétique {

    T(immutable) Génome;                    // Type générique pour le génome
    int tailleGénétique;                    // Taille du génome

    float tauxMutation;                     // Taux de mutation
    float tauxCroisement;                   // Taux de croisement
    int taillePopulation;                   // Taille de la population

    Génome[] population;                    // Population courante
    Génome meilleur;                        // Meilleur génome trouvé

    this() pure nothrow {
        // Valeurs par défaut
        tauxMutation       = 0.1;
        tauxCroisement     = 0.8;
        taillePopulation   = 100;
    }

    this(tauxMutation, tauxCroisement, taillePopulation) pure nothrow {
        this.tauxMutation   = tauxMutation;
        this.tauxCroisement = tauxCroisement;
        this.taillePopulation = taillePopulation;
    }

    void initialiser(T(Génome) Génome initial) {
        tailleGénétique = initial.taille;
        population = new Génome[taillePopulation];
        for (Génome i; i < taillePopulation; i++)
            population[i] = initial.clone();
        meilleur = population[0].clone();
    }

    Génome[] sélection() {
        // Sélection par tournoi
        Génome[] sélectionnés = new Génome[taillePopulation];
        for (Génome i; i < taillePopulation; i++) {
            Génome participant1 = population[rand(taillePopulation)];
            Génome participant2 = population[rand(taillePopulation)];
            sélectionnés[i] = participant1.obtenirAptitude() > participant2.obtenirAptitude() ? participant1 : participant2;
        }
        return sélectionnés;
    }

    Génome[] croisement(Génome[] sélectionnés) {
        Génome[] croisés = new Génome[taillePopulation];
        for (Génome i; i < taillePopulation; i += 2) {
            if (rand.real < tauxCroisement) {
                Génome parent1 = sélectionnés[rand(taillePopulation)];
                Génome parent2 = sélectionnés[rand(taillePopulation)];
                croisés[i]   = parent1.clone();
                croisés[i+1] = parent2.clone();
                croisés[i].croiser(croisés[i+1]);
            } else {
                croisés[i]   = sélectionnés[rand(taillePopulation)].clone();
                croisés[i+1] = sélectionnés[rand(taillePopulation)].clone();
            }
        }
        return croisés;
    }

    Génome[] mutation(Génome[] croisés) {
        for (Génome i; i < taillePopulation; i++) {
            if (rand.real < tauxMutation) {
                croisés[i]._mutation();
            }
        }
        return croisés;
    }

    void miseÀJour(Génome[] nouvellePopulation) {
        population = nouvellePopulation;
        meilleur = max(meilleur, population[]);
    }

    Génome obtenirMeilleur() {
        return meilleur;
    }

    void exécuter(Génome initial, int nombreGénérations) {
        initialiser(initial);
        for (int i; i < nombreGénérations; i++) {
            Génome[] sélectionnés = sélection();
            Génome[] croisés     = croisement(sélectionnés);
            Génome[] mutés       = mutation(croisés);
            miseÀJour(mutés);
            writeln(meilleur);
        }
    }
}

```

**Classe Génome**

```d
import std.stdio, std.math, std.algorithm, std.range, std.traits;

interface Génome(immutable) {
    int taille;                          // Taille du génome

    T obtenirAptitude() pure nothrow;        // Fonction d'aptitude

    void _mutation() pure virtual;            // Méthode de mutation
    void croiser(T other) pure virtual;      // Méthode de croisement
}

class RéelGénome(Génome) {

    float[] gènes;                           // Gènes du génome

    this() pure nothrow {
        taille = 0;
    }

    this(float[] gènes) pure nothrow {
        this.gènes = gènes;
        taille = gènes.length;
    }

    override T obtenirAptitude() pure nothrow {
        // Fonction d'aptitude par défaut
        return real(sum(gènes**2));
    }

    override void _mutation() pure virtual {
        // Mutation par défaut
        gènes[rand(taille)] += rand.real * 2.0 - 1.0;
    }

    override void croiser(T other) pure virtual {
        // Croisement par défaut
        float[] gènesCroisés = new float[taille];
        for (int i; i < taille; i++)
            gènesCroisés[i] = rand.real < 0.5 ? gènes[i] : other.gènes[i];
        other.gènes = gènesCroisés;
        gènes = gènesCroisés;
    }
}

```

**Exemple d'utilisation**

```d
void main() {
    auto génétique = Génétique(0.1, 0.8, 100);
    auto génomeInitial = RéelGénome([1.0, 2.0, 3.0]);
    génétique.exécuter(généomeInitial, 100);
    writeln("Meilleur génome trouvé :", génétique.obtenirMeilleur());
}
```