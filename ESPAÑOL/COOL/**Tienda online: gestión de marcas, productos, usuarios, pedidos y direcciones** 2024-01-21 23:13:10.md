```cool
clase [Marca] {
  propiedad nombre : string;
  propiedad logo : imagen;
  propiedad slogan : string;
  propiedad productos : lista[Producto]; // Lista de productos de la marca
}

clase [Producto] {
  propiedad nombre : string;
  propiedad precio : real;
  propiedad descripción : string;
  propiedad marca : Marca; // Marca a la que pertenece el producto
}

clase [Carro] {
  propiedad productos : lista[Producto]; // Lista de productos en el carro
  propiedad total : real; // Total a pagar por los productos en el carro
}

clase [Usuario] {
  propiedad nombre : string;
  propiedad correo : string;
  propiedad contraseña : string;
  propiedad direcciones : lista[Dirección]; // Lista de direcciones del usuario
  propiedad pedidos : lista[Pedido]; // Lista de pedidos del usuario
}

clase [Dirección] {
  propiedad calle : string;
  propiedad ciudad : string;
  propiedad estado : string;
  propiedad código_postal : string;
  propiedad país : string;
}

clase [Pedido] {
  propiedad productos : lista[Producto]; // Lista de productos en el pedido
  propiedad total : real; // Total a pagar por los productos en el pedido
  propiedad estado : string; // Estado del pedido (pendiente, en tránsito, entregado, etc.)
  propiedad dirección : Dirección; // Dirección de entrega del pedido
}

clase [Tienda] {
  propiedad marcas : lista[Marca]; // Lista de marcas disponibles en la tienda
  propiedad productos : lista[Producto]; // Lista de productos disponibles en la tienda
  propiedad carros : lista[Carro]; // Lista de carros de compra creados por los usuarios
  propiedad usuarios : lista[Usuario]; // Lista de usuarios registrados en la tienda
  propiedad pedidos : lista[Pedido]; // Lista de pedidos realizados por los usuarios

  método agregar_producto(producto : Producto) {
    productos.agregar(producto);
  }

  método agregar_marca(marca : Marca) {
    marcas.agregar(marca);
  }

  método crear_carro() {
    carro = new Carro();
    carros.agregar(carro);
    return carro;
  }

  método agregar_producto_a_carro(carro : Carro, producto : Producto) {
    carro.productos.agregar(producto);
    carro.total += producto.precio;
  }

  método quitar_producto_del_carro(carro : Carro, producto : Producto) {
    carro.productos.remover(producto);
    carro.total -= producto.precio;
  }

  método vaciar_carro(carro : Carro) {
    carro.productos.vaciar();
    carro.total = 0;
  }

  método crear_usuario(nombre : string, correo : string, contraseña : string) {
    usuario = new Usuario();
    usuario.nombre = nombre;
    usuario.correo = correo;
    usuario.contraseña = contraseña;
    usuarios.agregar(usuario);
    return usuario;
  }

  método agregar_dirección_a_usuario(usuario : Usuario, dirección : Dirección) {
    usuario.direcciones.agregar(dirección);
  }

  método crear_pedido(usuario : Usuario, carro : Carro, dirección : Dirección) {
    pedido = new Pedido();
    pedido.productos = carro.productos;
    pedido.total = carro.total;
    pedido.estado = "pendiente";
    pedido.dirección = dirección;
    pedidos.agregar(pedido);
    usuario.pedidos.agregar(pedido);
    return pedido;
  }

  método procesar_pedido(pedido : Pedido) {
    pedido.estado = "en tránsito";
  }

  método entregar_pedido(pedido : Pedido) {
    pedido.estado = "entregado";
  }

  método cancelar_pedido(pedido : Pedido) {
    pedido.estado = "cancelado";
  }
}

// Creamos una nueva tienda
tienda = new Tienda();

// Añadimos algunas marcas a la tienda
marca1 = new Marca();
marca1.nombre = "Nike";
marca1.logo = "logo_nike.png";
marca1.slogan = "Just do it";
tienda.agregar_marca(marca1);

marca2 = new Marca();
marca2.nombre = "Adidas";
marca2.logo = "logo_adidas.png";
marca2.slogan = "Impossible is nothing";
tienda.agregar_marca(marca2);

// Añadimos algunos productos a la tienda
producto1 = new Producto();
producto1.nombre = "Zapatillas Nike Air Max 90";
producto1.precio = 120;
producto1.descripción = "Las zapatillas Nike Air Max 90 son un clásico del calzado deportivo. Con su diseño icónico y su amortiguación Air Max, estas zapatillas son perfectas para cualquier actividad.";
producto1.marca = marca1;
tienda.agregar_producto(producto1);

producto2 = new Producto();
producto2.nombre = "Zapatillas Adidas Stan Smith";
producto2.precio = 80;
producto2.descripción = "Las zapatillas Adidas Stan Smith son otro clásico del calzado deportivo. Con su diseño sencillo y elegante, estas zapatillas son perfectas para cualquier ocasión.";
producto2.marca = marca2;
tienda.agregar_producto(producto2);

// Creamos un nuevo usuario
usuario1 = tienda.crear_usuario("Juan", "juan@example.com", "password");

// Añadimos una dirección al usuario
dirección1 = new Dirección();
dirección1.calle = "Calle Mayor, 123";
dirección1.ciudad = "Madrid";
dirección1.estado = "Madrid";
dirección1.código_postal = "28013";
dirección1.país = "España";
tienda.agregar_dirección_a_usuario(usuario1, dirección1);

// Creamos un nuevo carro de compra
carro1 = tienda.crear_carro();

// Añadimos algunos productos al carro de compra
tienda.agregar_producto_a_carro(carro1, producto1);
tienda.agregar_producto_a_carro(carro1, producto2);

// Creamos un nuevo pedido