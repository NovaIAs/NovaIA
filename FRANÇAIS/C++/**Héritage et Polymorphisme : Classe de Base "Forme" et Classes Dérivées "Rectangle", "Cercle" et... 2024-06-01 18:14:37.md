**Classe de base : Forme**

```cpp
class Forme {
protected:
    double aire;
    double perimetre;
    Point centre;

public:
    Forme(double x, double y) : centre(x, y) {}
    virtual double getAire() const { return aire; }
    virtual double getPerimetre() const { return perimetre; }
    virtual void afficher() const { std::cout << "Centre : " << centre; }
};
```

**Classes dérivées : Rectangle, Cercle, Triangle**

```cpp
class Rectangle : public Forme {
public:
    Rectangle(double largeur, double hauteur) :
        Forme(0, 0), largeur(largeur), hauteur(hauteur) {}
    double getAire() const override { return largeur * hauteur; }
    double getPerimetre() const override { return 2 * largeur + 2 * hauteur; }
    void afficher() const override {
        Forme::afficher();
        std::cout << ", Largeur : " << largeur << ", Hauteur : " << hauteur;
    }

private:
    double largeur;
    double hauteur;
};

class Cercle : public Forme {
public:
    Cercle(double rayon) : Forme(0, 0), rayon(rayon) {}
    double getAire() const override { return M_PI * rayon * rayon; }
    double getPerimetre() const override { return 2 * M_PI * rayon; }
    void afficher() const override {
        Forme::afficher();
        std::cout << ", Rayon : " << rayon;
    }

private:
    double rayon;
};

class Triangle : public Forme {
public:
    Triangle(double base, double hauteur) :
        Forme(0, 0), base(base), hauteur(hauteur) {}
    double getAire() const override { return 0.5 * base * hauteur; }
    double getPerimetre() const override { return base + hauteur + std::sqrt(base * base + hauteur * hauteur); }
    void afficher() const override {
        Forme::afficher();
        std::cout << ", Base : " << base << ", Hauteur : " << hauteur;
    }

private:
    double base;
    double hauteur;
};
```

**Fonction principale**

```cpp
int main() {
    Forme* formes[] = {
        new Rectangle(5, 10),
        new Cercle(3),
        new Triangle(4, 6)
    };

    for (Forme* f : formes) {
        f->afficher();
        std::cout << "\nAire : " << f->getAire() << std::endl;
        std::cout << "Périmètre : " << f->getPerimetre() << std::endl << std::endl;
    }

    for (Forme* f : formes) {
        delete f;
    }

    return 0;
}
```

**Explications**

* La classe **Forme** définit les attributs et méthodes communs à toutes les formes géométriques.
* Les classes dérivées **Rectangle**, **Cercle** et **Triangle** implémentent les méthodes spécifiques à chaque type de forme.
* La fonction principale crée un tableau de pointeurs vers des objets de type **Forme**, initialisant chacun d'eux avec des dimensions différentes.
* Elle parcourt ensuite le tableau, affiche les informations, l'aire et le périmètre de chaque forme.
* Enfin, elle libère la mémoire allouée aux objets **Forme**.

Ce code démontre :

* Le polymorphisme par l'implémentation de méthodes virtuelles.
* L'utilisation héritage hiérarchique.
* Le traitement dynamique des objets via des pointeurs.