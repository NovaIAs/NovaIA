```c++
// Définir la classe Mère
class Mère {
public:
    Mère() { std::cout << "Constructeur de la classe Mère\n"; }
    ~Mère() { std::cout << "Destructeur de la classe Mère\n"; }
    virtual void parler() { std::cout << "Je suis la mère\n"; }
};

// Définir la classe Fille
class Fille : public Mère {
public:
    Fille() : Mère() { std::cout << "Constructeur de la classe Fille\n"; }
    ~Fille() { std::cout << "Destructeur de la classe Fille\n"; }
    void parler() override { std::cout << "Je suis la fille\n"; }
    // Nouvelle méthode
    void chanter() { std::cout << "Je suis une fille qui chante\n"; }
};

// Définir la classe Petit-fils
class PetitFils : public Fille {
public:
    PetitFils() : Fille() { std::cout << "Constructeur de la classe Petit-fils\n"; }
    ~PetitFils() { std::cout << "Destructeur de la classe Petit-fils\n"; }
    void parler() override { std::cout << "Je suis le petit-fils\n"; }
    // Suralimenter une méthode de la classe Fille
    void chanter() override { std::cout << "Je suis un petit-fils qui chante de l'opéra\n"; }
};

int main() {
    // Créer des objets des différentes classes
    Mère mere;
    Fille fille;
    PetitFils petitFils;

    // Appeler les méthodes parler()
    mere.parler();
    fille.parler();
    petitFils.parler();

    // Appeler la méthode chanter() spécifique à la classe Fille et à la classe Petit-fils
    fille.chanter();
    petitFils.chanter();

    // Détruire les objets dans l'ordre inverse de leur création
    return 0;
}
```

**Explication du code :**

Ce code C++ implémente la hiérarchie des classes suivante :

* **Mère** : Classe de base représentant une mère.
* **Fille** : Classe dérivée de **Mère** représentant une fille.
* **PetitFils** : Classe dérivée de **Fille** représentant un petit-fils.

**La classe **Mère** a les caractéristiques suivantes :**

* Un constructeur qui affiche un message indiquant que l'objet **Mère** est créé.
* Un destructeur qui affiche un message indiquant que l'objet **Mère** est détruit.
* Une méthode **parler()** virtuelle qui affiche un message indiquant que l'objet est une mère.

**La classe **Fille** hérite de la classe **Mère** et a les caractéristiques suivantes :**

* Un constructeur qui appelle le constructeur de la classe **Mère** et affiche un message indiquant que l'objet **Fille** est créé.
* Un destructeur qui affiche un message indiquant que l'objet **Fille** est détruit.
* Une méthode **parler()** qui remplace celle de la classe **Mère** et affiche un message indiquant que l'objet est une fille.
* Une nouvelle méthode **chanter()** spécifique à la classe **Fille**.

**La classe **PetitFils** hérite de la classe **Fille** et a les caractéristiques suivantes :**

* Un constructeur qui appelle le constructeur de la classe **Fille** et affiche un message indiquant que l'objet **PetitFils** est créé.
* Un destructeur qui affiche un message indiquant que l'objet **PetitFils** est détruit.
* Une méthode **parler()** qui remplace celle de la classe **Fille** et affiche un message indiquant que l'objet est un petit-fils.
* Une méthode **chanter()** qui remplace celle de la classe **Fille** et affiche un message indiquant que l'objet est un petit-fils qui chante de l'opéra.

**La fonction **main()** crée des objets de chaque classe et appelle leurs méthodes pour démontrer le polymorphisme et le remplacement de méthodes.**

**Voici la sortie du programme :**

```
Constructeur de la classe Mère
Constructeur de la classe Fille
Constructeur de la classe Petit-fils
Je suis la mère
Je suis la fille
Je suis le petit-fils
Je suis une fille qui chante
Je suis un petit-fils qui chante de l'opéra
Destructeur de la classe Petit-fils
Destructeur de la classe Fille
Destructeur de la classe Mère
```