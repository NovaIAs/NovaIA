Claro! Aqui está um exemplo complexo em C++:

```cpp
#include <iostream>
#include <vector>

using namespace std;

// Classe base abstrata
class Animal {
public:
    virtual void fazerBarulho() = 0; // Método virtual puro
};

// Classe derivada de Animal
class Cachorro : public Animal {
public:
    void fazerBarulho() override {
        cout << "Au Au!" << endl;
    }
};

// Classe derivada de Animal
class Gato : public Animal {
public:
    void fazerBarulho() override {
        cout << "Miau!" << endl;
    }
};

// Classe derivada de Animal
class Vaca : public Animal {
public:
    void fazerBarulho() override {
        cout << "Muuu!" << endl;
    }
};

int main() {
    vector<Animal*> animais;
    animais.push_back(new Cachorro());
    animais.push_back(new Gato());
    animais.push_back(new Vaca());

    for (Animal* animal : animais) {
        animal->fazerBarulho();
    }

    // Libera a memória alocada para os animais
    for (Animal* animal : animais) {
        delete animal;
    }

    return 0;
}
```

Este código demonstra o uso de herança e polimorfismo em C++. Ele define uma classe base `Animal` com um método virtual puro `fazerBarulho()`, que é implementado nas classes derivadas `Cachorro`, `Gato` e `Vaca`. 

No `main()`, criamos um vetor de ponteiros para `Animal` chamado `animais` e adicionamos instâncias das classes derivadas a ele. Em seguida, percorremos o vetor chamando o método `fazerBarulho()` para cada animal, que imprimirá o barulho específico de cada animal.

Por fim, liberamos a memória alocada para os animais utilizando o operador `delete`.