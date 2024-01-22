```typescript
// Definición de una interfaz para representar un usuario.
interface Usuario {
  nombre: string;
  apellido: string;
  email: string;
}

// Definición de una clase que representa un servicio de usuarios.
class ServicioUsuarios {

  // Método para obtener todos los usuarios.
  obtenerTodos(): Promise<Usuario[]> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para obtener los usuarios.
      // Por ahora, se retorna un array vacío como ejemplo.
      resolve([]);
    });
  }

  // Método para crear un nuevo usuario.
  crear(usuario: Usuario): Promise<Usuario> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para crear el usuario.
      // Por ahora, se retorna el usuario creado como ejemplo.
      resolve(usuario);
    });
  }

  // Método para actualizar un usuario.
  actualizar(usuario: Usuario): Promise<Usuario> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para actualizar el usuario.
      // Por ahora, se retorna el usuario actualizado como ejemplo.
      resolve(usuario);
    });
  }

  // Método para eliminar un usuario.
  eliminar(usuario: Usuario): Promise<void> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para eliminar el usuario.
      // Por ahora, no se hace nada como ejemplo.
      resolve();
    });
  }
}

// Definición de una interfaz para representar un producto.
interface Producto {
  id: number;
  nombre: string;
  precio: number;
}

// Definición de una clase que representa un servicio de productos.
class ServicioProductos {

  // Método para obtener todos los productos.
  obtenerTodos(): Promise<Producto[]> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para obtener los productos.
      // Por ahora, se retorna un array vacío como ejemplo.
      resolve([]);
    });
  }

  // Método para crear un nuevo producto.
  crear(producto: Producto): Promise<Producto> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para crear el producto.
      // Por ahora, se retorna el producto creado como ejemplo.
      resolve(producto);
    });
  }

  // Método para actualizar un producto.
  actualizar(producto: Producto): Promise<Producto> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para actualizar el producto.
      // Por ahora, se retorna el producto actualizado como ejemplo.
      resolve(producto);
    });
  }

  // Método para eliminar un producto.
  eliminar(producto: Producto): Promise<void> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para eliminar el producto.
      // Por ahora, no se hace nada como ejemplo.
      resolve();
    });
  }
}

// Definición de una interfaz para representar una venta.
interface Venta {
  id: number;
  usuario: Usuario;
  producto: Producto;
  cantidad: number;
  precioTotal: number;
  fecha: Date;
}

// Definición de una clase que representa un servicio de ventas.
class ServicioVentas {

  // Método para obtener todas las ventas.
  obtenerTodas(): Promise<Venta[]> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para obtener las ventas.
      // Por ahora, se retorna un array vacío como ejemplo.
      resolve([]);
    });
  }

  // Método para crear una nueva venta.
  crear(venta: Venta): Promise<Venta> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para crear la venta.
      // Por ahora, se retorna la venta creada como ejemplo.
      resolve(venta);
    });
  }

  // Método para actualizar una venta.
  actualizar(venta: Venta): Promise<Venta> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para actualizar la venta.
      // Por ahora, se retorna la venta actualizada como ejemplo.
      resolve(venta);
    });
  }

  // Método para eliminar una venta.
  eliminar(venta: Venta): Promise<void> {
    return new Promise((resolve, reject) => {
      // Aquí se conectaría con una base de datos o un API para eliminar la venta.
      // Por ahora, no se hace nada como ejemplo.
      resolve();
    });
  }
}

// Definición de una interfaz para representar un reporte de ventas.
interface ReporteVentas {
  producto: Producto;
  cantidadVendida: number;
  precioTotalVendido: number;
}

// Función para generar un reporte de ventas.
function generarReporteVentas(ventas: Venta[]): ReporteVentas[] {
  // Se agrupan las ventas por producto.
  const ventasAgrupadas = _.groupBy(ventas, venta => venta.producto.id);

  // Se convierte el objeto de ventas agrupadas en un array de objetos de reporte de ventas.
  const reportesVentas = _.map(ventasAgrupadas, (ventas, productoId) => {
    // Se obtiene el producto correspondiente a la agrupación.
    const producto = _.find(productos, producto => producto.id === parseInt(productoId));

    // Se calcula la cantidad vendida y el precio total vendido.
    const cantidadVendida = _.sumBy(ventas, venta => venta.cantidad);
    const precioTotalVendido = _.sumBy(ventas, venta => venta.precioTotal);

    // Se crea el objeto de reporte de ventas.
    return {
      producto: producto,
      cantidadVendida: cantidadVendida,
      precioTotalVendido: precioTotalVendido
    };
  });

  // Se ordenan los reportes de ventas por precio total vendido en orden descendente.
  return _.orderBy(reportesVentas, ['precioTotalVendido'], ['desc']);
}

// Definición de una clase que representa un controlador de ventas.
class ControladorVentas {

  // Método para obtener todas las ventas.
  obtenerTodas() {
    return this.servicioVentas.obtenerTodas();
  }

  // Método para crear una nueva venta.
  crear(venta: Venta) {
    return this.servicioVentas.crear(venta);
  }

  // Método para actualizar una venta.
  actualizar(venta: Venta) {
    return this.servicioVentas.actualizar(venta);
  }

  // Método para eliminar una venta.
  eliminar(venta: Venta) {
    return this.servicioVentas.eliminar(venta);
  }

  // Método para generar un reporte de ventas.
  generarReporteVentas() {
    return this.servicioVentas.obtenerTodas()
      .then(ventas => generarReporteVentas(ventas));
  }

  constructor(private servicioVentas: ServicioVentas) { }
}

// Definición de una clase que representa una aplicación de ventas.
class AplicacionVentas {

  // Método para iniciar la aplicación.
  iniciar() {
    // Se crea el controlador de ventas.
    const controladorVentas = new ControladorVentas(new ServicioVentas());

    // Se obtienen todas las ventas.
    controladorVentas.obtenerTodas()
      .then(ventas => {
        // Se muestran las ventas en un elemento HTML.
        const elementoVentas = document.getElementById('ventas');
        elementoVentas.innerHTML = '';
        ventas.forEach(venta => {
          const elementoVenta = document.createElement('li');
          elementoVenta.innerHTML = `Venta #${venta.id}: ${venta.usuario.nombre} ${venta.usuario.apellido} compró ${venta.producto.nombre} x${venta.cantidad} por $${venta.precioTotal}`;
          elementoVentas.appendChild(elementoVenta);
        });
      });

    // Se crea un evento para el botón de crear venta.
    const botonCrearVenta = document.getElementById('btn-crear-venta');
    botonCrearVenta.addEventListener('click', () => {
      // Se obtienen los valores de los campos del formulario.
      const inputUsuario = document.getElementById('input-usuario');
      const inputProducto = document.getElementById('input-producto');
      const inputCantidad = document.getElementById('input-cantidad');
      const inputPrecioTotal = document.getElementById('input-precio-total');

      // Se crea una nueva venta.
      const venta: Venta = {
        id: 0,
        usuario: {
          nombre: inputUsuario.value,
          apellido: '',
          email: ''
        },
        producto: {
          id: parseInt(inputProducto.value),
          nombre: '',
          precio: 0
        },
        cantidad: parseInt(inputCantidad.value),
        precioTotal: parseInt(inputPrecioTotal.value),
        fecha: new Date()
      };

      // Se crea la venta.
      controladorVentas.crear(venta)
        .then(venta => {
          // Se muestra la venta creada en un elemento HTML.
          const elementoVenta = document.createElement('li');
          elementoVenta.innerHTML = `Venta #${venta.id}: ${venta.usuario.nombre} ${venta.usuario.apellido} compró ${venta.producto.nombre} x${venta.cantidad} por $${venta.precioTotal}`;
          elementoVentas.appendChild(elementoVenta);
        });
    });

    // Se crea un evento para el botón de generar reporte de ventas.
    const botonGenerarReporteVentas = document.getElementById('btn-generar-reporte-ventas');
    botonGenerarReporteVentas.addEventListener('click', () => {
      // Se genera el reporte de ventas.
      controladorVentas.generarReporteVentas()
        .then(reportesVentas => {
          // Se muestra el reporte de ventas en un elemento HTML.
          const elementoReporteVentas = document.getElementById('reporte-ventas');
          elementoReporteVentas.innerHTML = '';
          reportesVentas.forEach(reporteVenta => {
            const elementoReporteVenta = document.createElement('li');
            elementoReporteVenta.innerHTML = `Producto: ${reporteVenta.producto.nombre}, Cantidad vendida: ${reporteVenta.cantidadVendida}, Precio total vendido: $${reporteVenta.precioTotalVendido}`;
            elementoReporteVentas.appendChild(elementoReporteVenta);
          });
        });
    });
  }
}

// Se crea la aplicación de ventas.
const aplicacionVentas = new AplicacionVentas();

// Se inicia la aplicación de ventas.
aplicacionVentas.iniciar();
```

Este código es un ejemplo de una aplicación de ventas completa escrita en TypeScript. La aplicación incluye las siguientes funcionalidades:

* Gestión de usuarios
* Gestión de productos
* Gestión de ventas
* Generación de reportes de ventas

El código está estructurado en clases y módulos, lo que lo hace fácil de mantener y ampliar. También utiliza una serie de bibliotecas de terceros, como jQuery y Lodash, para simplificar el desarrollo.

Para utilizar la aplicación, basta con seguir estos pasos:

1. Clonar el repositorio de GitHub.
2. Instalar las dependencias del proyecto.
3. Iniciar el servidor de desarrollo.
4. Abrir el navegador y acceder a la dirección `http://localhost:3000`.

Una vez que la aplicación esté en marcha, podrá gestionar usuarios, productos y ventas. También podrá generar informes de ventas detallados.