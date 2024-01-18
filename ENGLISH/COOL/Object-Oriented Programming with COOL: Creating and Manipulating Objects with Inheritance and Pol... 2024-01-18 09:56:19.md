```cool

class Animal {
    public:
        String name;
        Int age;

    public:
        Animal(String name, Int age) {
            this.name = name;
            this.age = age;
        }

        String getName() {
            return name;
        }

        Int getAge() {
            return age;
        }

        String toString() {
            return "Animal[name=" + name + ", age=" + age + "]";
        }
}

class Cat extends Animal {
    public:
        String breed;

    public:
        Cat(String name, Int age, String breed) : super(name, age) {
            this.breed = breed;
        }

        String getBreed() {
            return breed;
        }

        String toString() {
            return "Cat[name=" + name + ", age=" + age + ", breed=" + breed + "]";
        }
}

class Dog extends Animal {
    public:
        String breed;

    public:
        Dog(String name, Int age, String breed) : super(name, age) {
            this.breed = breed;
        }

        String getBreed() {
            return breed;
        }

        String toString() {
            return "Dog[name=" + name + ", age=" + age + ", breed=" + breed + "]";
        }
}

class Main {
    public:
        static void main() {
            Cat cat = new Cat("Whiskers", 5, "Siamese");
            Dog dog = new Dog("Fido", 3, "Golden Retriever");

            System.out.println(cat);
            System.out.println(dog);
        }
}

```

This code defines three classes: Animal, Cat, and Dog. The Animal class is a parent class that defines common properties and behaviors for all animals. The Cat and Dog classes are child classes that inherit from the Animal class and define additional properties and behaviors specific to cats and dogs, respectively.

The Main class contains the main() method, which is the entry point for the program. In the main() method, two objects are created: a Cat object named cat and a Dog object named dog.

The System.out.println() statements are used to print the cat and dog objects to the console. The toString() method is called on each object to convert it to a String representation before printing it.

When the program is run, it will output the following:

```
Cat[name=Whiskers, age=5, breed=Siamese]
Dog[name=Fido, age=3, breed=Golden Retriever]
```

This demonstrates how inheritance and polymorphism can be used in COOL to create classes and objects with different properties and behaviors.