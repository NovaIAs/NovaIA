```smalltalk
**Módulo de Gestión de Inventario**

```
**Clase Inventario**

```

Inventario >> inicializar
    super inicializar.
    productos := OrderedCollection new.
    proveedores := OrderedCollection new.
    clientes := OrderedCollection new.

Inventario >> agregarProducto: unProducto
    productos add: unProducto.

Inventario >> quitarProducto: unProducto
    productos remove: unProducto.

Inventario >> obtenerProducto: unNombreDeProducto
    productos detect: [:producto | producto nombre = unNombreDeProducto]

**Clase Proveedor**

```

Proveedor >> inicializar
    super inicializar.
    nombre := nil.
    dirección := nil.
    teléfono := nil.

Proveedor >> obtenerNombre
    nombre.

Proveedor >> establecerNombre: unNombre
    nombre := unNombre.

Proveedor >> obtenerDirección
    dirección.

Proveedor >> establecerDirección: unaDirección
    dirección := unaDirección.

Proveedor >> obtenerTeléfono
    teléfono.

Proveedor >> establecerTeléfono: unTeléfono
    teléfono := unTeléfono.

**Clase Cliente**

```

Cliente >> inicializar
    super inicializar.
    nombre := nil.
    dirección := nil.
    teléfono := nil.

Cliente >> obtenerNombre
    nombre.

Cliente >> establecerNombre: unNombre
    nombre := unNombre.

Cliente >> obtenerDirección
    dirección.

Cliente >> establecerDirección: unaDirección
    dirección := unaDirección.

Cliente >> obtenerTeléfono
    teléfono.

Cliente >> establecerTeléfono: unTeléfono
    teléfono := unTeléfono.

**Clase Producto**

```

Producto >> inicializar
    super inicializar.
    nombre := nil.
    precio := nil.
    stock := nil.

Producto >> obtenerNombre
    nombre.

Producto >> establecerNombre: unNombre
    nombre := unNombre.

Producto >> obtenerPrecio
    precio.

Producto >> establecerPrecio: unPrecio
    precio := unPrecio.

Producto >> obtenerStock
    stock.

Producto >> establecerStock: unStock
    stock := unStock.

**Uso del Módulo de Gestión de Inventario**

```

Nuevo sistema de Gestión de Inventario

Inventario sistemaInventario := Inventario new.

Nuevos productos

Producto producto1 := Producto new.
producto1 establecerNombre: 'Producto 1'.
producto1 establecerPrecio: 100.
producto1 establecerStock: 10.

Producto producto2 := Producto new.
producto2 establecerNombre: 'Producto 2'.
producto2 establecerPrecio: 200.
producto2 establecerStock: 10.

Producto producto3 := Producto new.
producto3 establecerNombre: 'Producto 3'.
producto3 establecerPrecio: 300.
producto3 establecerStock: 10.

Agregar productos al sistema de inventario

sistemaInventario agregarProducto: producto1.
sistemaInventario agregarProducto: producto2.
sistemaInventario agregarProducto: producto3.

Obtener un producto del sistema de inventario

Producto productoBuscado := sistemaInventario obtenerProducto: 'Producto 2'.

Quitar un producto del sistema de inventario

sistemaInventario quitarProducto: producto3.

Impresiones de prueba

Transcript show: 'Productos en el sistema de inventario:'; cr.
sistemaInventario productos do: [:producto | Transcript show: producto nombre; cr].

Transcript show: 'Proveedores en el sistema de inventario:'; cr.
sistemaInventario proveedores do: [:proveedor | Transcript show: proveedor nombre; cr].

Transcript show: 'Clientes en el sistema de inventario:'; cr.
sistemaInventario clientes do: [:cliente | Transcript show: cliente nombre; cr].
```

Explicación del Código:

1. **Módulo de Gestión de Inventario**: Este módulo es el contenedor principal de todo el código relacionado con la gestión de inventario.

2. **Clase Inventario**: Esta clase representa el inventario en sí y contiene colecciones de productos, proveedores y clientes.

3. **Clases Proveedor, Cliente y Producto**: Estas clases representan los diferentes tipos de entidades que participan en la gestión de inventario.

4. **Métodos de las Clases**: Las clases contienen métodos para realizar diversas operaciones, como agregar, obtener y eliminar productos, proveedores y clientes.

5. **Uso del Módulo de Gestión de Inventario**: Se crea una instancia de la clase Inventario y se agregan productos de muestra. Luego, se muestran los productos, proveedores y clientes en la consola de Smalltalk.

Este código proporciona una estructura básica para un sistema de gestión de inventario. Se puede ampliar fácilmente agregando más funcionalidades, como la gestión de pedidos y la generación de informes.