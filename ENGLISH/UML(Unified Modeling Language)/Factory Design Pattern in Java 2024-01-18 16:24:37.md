```
// Define an interface for a product
interface Product {
    public function getName(): string;
    public function getPrice(): float;
}

// Create two concrete product classes
class Book implements Product {
    private $name;
    private $price;

    public function __construct($name, $price) {
        $this->name = $name;
        $this->price = $price;
    }

    public function getName(): string {
        return $this->name;
    }

    public function getPrice(): float {
        return $this->price;
    }
}

class Toy implements Product {
    private $name;
    private $price;

    public function __construct($name, $price) {
        $this->name = $name;
        $this->price = $price;
    }

    public function getName(): string {
        return $this->name;
    }

    public function getPrice(): float {
        return $this->price;
    }
}

// Define an interface for a factory
interface Factory {
    public function createProduct($type): Product;
}

// Create two concrete factory classes
class BookFactory implements Factory {
    public function createProduct($type) {
        switch ($type) {
            case 'fiction':
                return new Book('Fiction Book', 10.00);
            case 'non-fiction':
                return new Book('Non-Fiction Book', 15.00);
            default:
                throw new InvalidArgumentException('Invalid book type');
        }
    }
}

class ToyFactory implements Factory {
    public function createProduct($type) {
        switch ($type) {
            case 'doll':
                return new Toy('Doll', 10.00);
            case 'action figure':
                return new Toy('Action Figure', 15.00);
            default:
                throw new InvalidArgumentException('Invalid toy type');
        }
    }
}

// Create a client class to use the factory
class Client {
    private $factory;

    public function __construct(Factory $factory) {
        $this->factory = $factory;
    }

    public function createProduct($type) {
        return $this->factory->createProduct($type);
    }
}

// Usage:
$bookFactory = new BookFactory();
$toyFactory = new ToyFactory();

$client = new Client($bookFactory);
$book = $client->createProduct('fiction');
echo $book->getName() . ' - $' . $book->getPrice() . PHP_EOL;

$client = new Client($toyFactory);
$toy = $client->createProduct('doll');
echo $toy->getName() . ' - $' . $toy->getPrice() . PHP_EOL;
```

This code implements the factory design pattern, which allows you to create objects without specifying their exact class. This can be useful when you have a large number of similar objects that need to be created, or when you want to defer the creation of an object until runtime.

In this example, we have two factory classes, `BookFactory` and `ToyFactory`, which can be used to create book and toy objects, respectively. We also have a `Client` class that can use either factory to create objects.

The `Client` class first creates a factory object, then uses that factory to create the desired products. The factory object can then create the appropriate product objects, based on the type of product that is requested.

This allows us to easily create different types of products without having to specify their exact class. This can be useful in a number of situations, such as when we are creating a large number of similar objects, or when we want to defer the creation of an object until runtime.