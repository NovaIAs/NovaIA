```tcl
# Cargar librerías necesarias
package require Tcl 8.5
package require Tk 8.5
package require TclDB 1.1

# Crear base de datos
db::connect dbname=mifactura.db user=root password=password

# Crear tablas
db::execute {
    CREATE TABLE productos (
        id INTEGER PRIMARY KEY,
        nombre TEXT,
        precio REAL,
        cantidad INTEGER
    );
    CREATE TABLE facturas (
        id INTEGER PRIMARY KEY,
        fecha DATETIME,
        cliente TEXT,
        total REAL
    );
    CREATE TABLE factura_detalle (
        factura_id INTEGER,
        producto_id INTEGER,
        cantidad INTEGER,
        precio_unitario REAL,
        total REAL,
        FOREIGN KEY (factura_id) REFERENCES facturas(id),
        FOREIGN KEY (producto_id) REFERENCES productos(id)
    );
}

# Crear interfaz gráfica de usuario (GUI)
set root [tk::toplevel .]
tk::wm title $root "Facturación"

# Crear cuadros de texto para ingresar datos
set producto [tk::entry $root -width 20]
tk::pack $producto -side left

set cantidad [tk::entry $root -width 10]
tk::pack $cantidad -side left

set precio [tk::entry $root -width 10]
tk::pack $precio -side left

# Crear botón para agregar producto a la factura
set agregar [tk::button $root -text "Agregar"]
tk::pack $agregar -side left

# Crear botón para finalizar la factura
set finalizar [tk::button $root -text "Finalizar"]
tk::pack $finalizar -side left

# Definir función para agregar producto a la factura
proc agregarProducto {} {
    global producto cantidad precio

    if {[db::exists productos $producto]} {
        set producto_id [db::get productos producto]
    } else {
        db::execute {
            INSERT INTO productos (nombre, precio, cantidad)
            VALUES ($producto, $precio, $cantidad);
        }
        set producto_id [db::last_insert_id]
    }

    db::execute {
        INSERT INTO factura_detalle (factura_id, producto_id, cantidad, precio_unitario, total)
        VALUES ($factura_id, $producto_id, $cantidad, $precio, $precio * $cantidad);
    }

    # Actualizar el total de la factura
    set total [db::get facturas $factura_id total]
    set total [expr {$total + ($precio * $cantidad)}]
    db::execute {
        UPDATE facturas SET total=$total WHERE id=$factura_id;
    }
}

# Definir función para finalizar la factura
proc finalizarFactura {} {
    global factura_id

    # Actualizar el total de la factura
    set total [db::get facturas $factura_id total]
    set total [expr {$total + ($precio * $cantidad)}]
    db::execute {
        UPDATE facturas SET total=$total WHERE id=$factura_id;
    }

    # Imprimir la factura
    puts "Factura No: $factura_id"
    puts "Fecha: [db::get facturas $factura_id fecha]"
    puts "Cliente: [db::get facturas $factura_id cliente]"
    puts "\n"
    puts "==================================="
    puts "Producto\tCantidad\tPrecio Unitario\tTotal"
    puts "==================================="
    db::foreach factura_detalle $factura_id {
        puts "[db::get productos $producto_id nombre]\t$cantidad\t$precio_unitario\t$total"
    }
    puts "==================================="
    puts "Total:\t\t\t\t$total\n"
}

# Crear factura nueva
set factura_id [db::last_insert_id]
db::execute {
    INSERT INTO facturas (id, fecha, cliente, total)
    VALUES ($factura_id, datetime('now'), 'Cliente Prueba', 0);
}

# Asociar botones a funciones
tk::bind $agregar <Button-1> agregarProducto
tk::bind $finalizar <Button-1> finalizarFactura

# Iniciar el bucle de eventos de la GUI
tk::mainloop
```

Explicación del código:

1. Cargamos las librerías necesarias para la gestión de bases de datos, la interfaz gráfica de usuario y la conexión a la base de datos.

2. Creamos la base de datos y las tablas necesarias para almacenar los productos, las facturas y los detalles de las facturas.

3. Creamos la interfaz gráfica de usuario (GUI) con cuadros de texto para ingresar datos de los productos, botones para agregar productos a la factura y finalizar la factura, y una etiqueta para mostrar el total de la factura.

4. Definimos funciones para agregar productos a la factura y finalizar la factura.

5. Creamos una nueva factura y la almacenamos en la base de datos.

6. Asociamos los botones a las funciones correspondientes.

7. Iniciamos el bucle de eventos de la GUI, que permite ejecutar la aplicación y responder a eventos del usuario.