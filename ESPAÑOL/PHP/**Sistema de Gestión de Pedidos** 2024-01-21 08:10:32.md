```php
<?php

// Clase Producto
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

// Clase Carrito de Compra
class CarritoDeCompra {

    private $productos = array();

    public function agregarProducto(Producto $producto) {
        $this->productos[] = $producto;
    }

    public function quitarProducto(Producto $producto) {
        foreach ($this->productos as $key => $value) {
            if ($value->getId() == $producto->getId()) {
                unset($this->productos[$key]);
            }
        }
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

// Clase Cliente
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

// Clase Pedido
class Pedido {

    private $id;
    private $cliente;
    private $carritoDeCompra;
    private $fecha;

    public function __construct($id, Cliente $cliente, CarritoDeCompra $carritoDeCompra, $fecha) {