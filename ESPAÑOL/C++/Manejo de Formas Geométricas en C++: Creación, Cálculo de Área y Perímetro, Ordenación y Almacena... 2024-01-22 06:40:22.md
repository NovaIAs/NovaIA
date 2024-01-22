```cpp
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

// Declaración de una clase abstracta para representar una forma geométrica
class Forma {
public:
    virtual float area() = 0;
    virtual float perimetro() = 0;
    virtual void imprimir() = 0;
};

// Declaración de una clase para representar un círculo
class Circulo : public Forma {
private:
    float radio;

public:
    Circulo(float radio) {
        this->radio = radio;
    }

    float area() override {
        return 3.14159265 * radio * radio;
    }

    float perimetro() override {
        return 2 * 3.14159265 * radio;
    }

    void imprimir() override {
        cout << "Círculo con radio " << radio << endl;
    }
};

// Declaración de una clase para representar un rectángulo
class Rectangulo : public Forma {
private:
    float largo;
    float ancho;

public:
    Rectangulo(float largo, float ancho) {
        this->largo = largo;
        this->ancho = ancho;
    }

    float area() override {
        return largo * ancho;
    }

    float perimetro() override {
        return 2 * (largo + ancho);
    }

    void imprimir() override {
        cout << "Rectángulo con largo " << largo << " y ancho " << ancho << endl;
    }
};

// Declaración de una clase para representar un triángulo
class Triangulo : public Forma {
private:
    float base;
    float altura;

public:
    Triangulo(float base, float altura) {
        this->base = base;
        this->altura = altura;
    }

    float area() override {
        return 0.5 * base * altura;
    }

    float perimetro() override {
        float hipotenusa = sqrt(base * base + altura * altura);
        return base + altura + hipotenusa;
    }

    void imprimir() override {
        cout << "Triángulo con base " << base << " y altura " << altura << endl;
    }
};

// Función principal
int main() {
    // Creación de un vector de formas
    vector<Forma*> formas;
    formas.push_back(new Circulo(5));
    formas.push_back(new Rectangulo(10, 5));
    formas.push_back(new Triangulo(3, 4));

    // Iteración sobre el vector de formas para calcular el área y el perímetro de cada forma
    for (Forma* forma : formas) {
        cout << "Área: " << forma->area() << endl;
        cout << "Perímetro: " << forma->perimetro() << endl;
        forma->imprimir();
        cout << endl;
    }

    // Ordenación del vector de formas según su área
    sort(formas.begin(), formas.end(), [](Forma* a, Forma* b) { return a->area() < b->area(); });

    // Impresión del vector de formas ordenado según su área
    cout << "Formas ordenadas según su área:" << endl;
    for (Forma* forma : formas) {
        forma->imprimir();
        cout << endl;
    }

    // Creación de un mapa para almacenar las formas según su tipo
    map<string, vector<Forma*>> formasPorTipo;
    formasPorTipo["Círculo"].push_back(new Circulo(5));
    formasPorTipo["Rectángulo"].push_back(new Rectangulo(10, 5));
    formasPorTipo["Triángulo"].push_back(new Triangulo(3, 4));

    // Iteración sobre el mapa para imprimir las formas según su tipo
    for (auto& [tipo, formas] : formasPorTipo) {
        cout << "Formas de tipo " << tipo << ":" << endl;
        for (Forma* forma : formas) {
            forma->imprimir();
            cout << endl;
        }
        cout << endl;
    }

    // Liberación de la memoria asignada para las formas
    for (Forma* forma : formas) {
        delete forma;
    }

    return 0;
}
```

Explicación del código:

* Se declaran tres clases abstractas para representar una forma geométrica, un círculo, un rectángulo y un triángulo.
* La clase `Forma` define los métodos abstractos `area()`, `perímetro()` e `imprimir()`, que deben ser implementados por las clases derivadas.
* Las clases `Círculo`, `Rectángulo` y `Triángulo` heredan de la clase `Forma` y implementan los métodos abstractos.
* En la función `main()` se crea un vector de formas y se agregan instancias de las clases `Círculo`, `Rectángulo` y `Triángulo`.
* Se itera sobre el vector de formas para calcular el área y el perímetro de cada forma.
* Se ordena el vector de formas según su área.
* Se crea un mapa para almacenar las formas según su tipo.
* Se itera sobre el mapa para imprimir las formas según su tipo.
* Se libera la memoria asignada para las formas.