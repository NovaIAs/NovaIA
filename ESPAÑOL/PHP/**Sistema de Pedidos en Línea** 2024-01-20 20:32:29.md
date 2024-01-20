```php
<?php

class Producto {

    private $id;
    private $nombre;
    private $descripcion;
    private $precio;

    public function __construct($id, $nombre, $descripcion, $precio) {
        $this->id = $id;
        $this->nombre = $nombre;
        $this->descripcion = $descripcion;
        $this->precio = $precio;
    }

    public function getId() {
        return $this->id;
    }

    public function getNombre() {
        return $this->nombre;
    }

    public function getDescripcion() {
        return $this->descripcion;
    }

    public function getPrecio() {
        return $this->precio;
    }
}

class Carrito {

    private $productos;

    public function __construct() {
        $this->productos = array();
    }

    public function addProducto($producto) {
        $this->productos[] = $producto;
    }

    public function getProductos() {
        return $this->productos;
    }

    public function getTotal() {
        $total = 0;
        foreach ($this->productos as $producto) {
            $total += $producto->getPrecio();
        }
        return $total;
    }
}

class Cliente {

    private $id;
    private $nombre;
    private $direccion;
    private $telefono;

    public function __construct($id, $nombre, $direccion, $telefono) {
        $this->id = $id;
        $this->nombre = $nombre;
        $this->direccion = $direccion;
        $this->telefono = $telefono;
    }

    public function getId() {
        return $this->id;
    }

    public function getNombre() {
        return $this->nombre;
    }

    public function getDireccion() {
        return $this->direccion;
    }

    public function getTelefono() {
        return $this->telefono;
    }
}

class Pedido {

    private $id;
    private $cliente;
    private $carrito;
    private $fecha;

    public function __construct($id, $cliente, $carrito, $fecha) {
        $this->id = $id;
        $this->cliente = $cliente;
        $this->carrito = $carrito;
        $this->fecha = $fecha;
    }

    public function getId() {
        return $this->id;
    }

    public function getCliente() {
        return $this->cliente;
    }

    public function getCarrito() {
        return $this->carrito;
    }

    public function getFecha() {
        return $this->fecha;
    }
}

$producto1 = new Producto(1, "Producto 1", "Descripción del producto 1", 10.0);
$producto2 = new Producto(2, "Producto 2", "Descripción del producto 2", 15.0);
$producto3 = new Producto(3, "Producto 3", "Descripción del producto 3", 20.0);

$carrito = new Carrito();
$carrito->addProducto($producto1);
$carrito->addProducto($producto2);
$carrito->addProducto($producto3);

$cliente = new Cliente(1, "Cliente 1", "Dirección del cliente 1", "Teléfono del cliente 1");

$pedido = new Pedido(1, $cliente, $carrito, "Fecha del pedido");

echo "ID del pedido: " . $pedido->getId() . "\n";
echo "Cliente del pedido: " . $pedido->getCliente()->getNombre() . "\n";
echo "Carrito del pedido: \n";
foreach ($pedido->getCarrito()->getProductos() as $producto) {
    echo "    ID: " . $producto->getId() . "\n";
    echo "    Nombre: " . $producto->getNombre() . "\n";
    echo "    Descripción: " . $producto->getDescripcion() . "\n";
    echo "    Precio: " . $producto->getPrecio() . "\n";
}
echo "Total del pedido: " . $pedido->getCarrito()->getTotal() . "\n";
echo "Fecha del pedido: " . $pedido->getFecha() . "\n";

```

Explicación del código:

* Se definen cuatro clases: `Producto`, `Carrito`, `Cliente` y `Pedido`.
* La clase `Producto` representa un producto que se puede añadir al carrito. Tiene atributos como el ID, el nombre, la descripción y el precio.
* La clase `Carrito` representa un carrito de la compra. Tiene atributos como la lista de productos que se han añadido al carrito y el total del carrito.
* La clase `Cliente` representa un cliente que realiza un pedido. Tiene atributos como el ID, el nombre, la dirección y el teléfono.
* La clase `Pedido` representa un pedido que realiza un cliente. Tiene atributos como el ID, el cliente, el carrito y la fecha del pedido.
* Se crean tres instancias de la clase `Producto` y se añaden al carrito.
* Se crea una instancia de la clase `Cliente`.
* Se crea una instancia de la clase `Pedido` con el cliente y el carrito.
* Se muestran por pantalla los datos del pedido, incluyendo el ID del pedido, el nombre del cliente, el contenido del carrito y el total del pedido.