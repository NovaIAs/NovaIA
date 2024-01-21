```cool
clase Animal {
    cad name;
    cad noise;

    Animal(cad name, cad noise) {
        this.name = name;
        this.noise = noise;
    }

    método makeNoise() {
        cout << "Soy un " << name << " y hago " << noise << "!" << endl;
    }
}

clase Perro : Animal {
    Perro(cad name) : Animal(name, "guau!") {}
}

clase Gato : Animal {
    Gato(cad name) : Animal(name, "miau!") {}
}

clase Vaca : Animal {
    Vaca(cad name) : Animal(name, "muu!") {}
}

clase Granja {
    list<Animal> animales;

    Granja() {}

    método addAnimal(Animal animal) {
        animales.add(animal);
    }

    método makeAllNoise() {
        for (Animal animal : animales) {
            animal.makeNoise();
        }
    }
}

función main() {
    Granja granja = new Granja();

    granja.addAnimal(new Perro("Firulais"));
    granja.addAnimal(new Gato("Michi"));
    granja.addAnimal(new Vaca("Bessie"));

    granja.makeAllNoise();

    return 0;
}
```

Este código define una jerarquía de clases en COOL que representa a los animales y una granja. La clase `Animal` es la clase base de todas las demás clases de animales. Tiene dos atributos, `name` y `noise`, y un método, `makeNoise()`, que imprime el nombre del animal y el ruido que hace.

Las clases `Perro`, `Gato` y `Vaca` son clases derivadas de la clase `Animal`. Cada una de ellas tiene un constructor que llama al constructor de la clase `Animal` y le pasa el nombre del animal y el ruido que hace.

La clase `Granja` representa una granja. Tiene un atributo, `animales`, que es una lista de animales. La clase `Granja` tiene un método, `addAnimal()`, que añade un animal a la lista de animales y un método, `makeAllNoise()`, que llama al método `makeNoise()` de cada animal de la lista.

La función `main()` crea una nueva instancia de la clase `Granja` y luego añade tres animales a la lista de animales: un perro, un gato y una vaca. A continuación, llama al método `makeAllNoise()` de la granja, que hace que cada animal haga ruido.