```erlang
-module(punto_de_venta).
-export([iniciar/0, agregar_producto/2, eliminar_producto/1, modificar_producto/2, realizar_venta/1, consultar_venta/1]).

-record(producto, {id, nombre, precio}).
-record(venta, {id, fecha, productos}).

iniciar() ->
    Productos = [],
    Ventas = [],
    iniciar_bucle(Productos, Ventas).

iniciar_bucle(Productos, Ventas) ->
    Comando = io:get_line("Comando (agregar, eliminar, modificar, vender, consultar, salir): "),
    case Comando of
        "agregar" -> Productos = agregar_producto(Productos);
        "eliminar" -> Productos = eliminar_producto(Productos);
        "modificar" -> Productos = modificar_producto(Productos);
        "vender" -> Ventas = realizar_venta(Productos, Ventas);
        "consultar" -> consultar_venta(Ventas);
        "salir" -> erlang:halt();
        _ -> io:fwrite("Comando inválido. Inténtalo de nuevo.\n")
    end,
    iniciar_bucle(Productos, Ventas).

agregar_producto(Productos) ->
    io:fwrite("Nombre del producto: "),
    Nombre = io:get_line(),
    io:fwrite("Precio: "),
    Precio = io:get_line(),
    Productos ++ [producto:new(erlang:length(Productos) + 1, Nombre, Precio)].

eliminar_producto(Productos) ->
    io:fwrite("ID del producto a eliminar: "),
    ID = io:get_line(),
    Productos -- [producto:new(ID, _, _)].

modificar_producto(Productos) ->
    io:fwrite("ID del producto a modificar: "),
    ID = io:get_line(),
    case lists:keyfind(ID, 1, Productos) of
        undefined -> io:fwrite("Producto no encontrado.\n");
        Producto ->
            io:fwrite("Nuevo nombre del producto: "),
            NuevoNombre = io:get_line(),
            io:fwrite("Nuevo precio: "),
            NuevoPrecio = io:get_line(),
            Productos -- [Producto] ++ [producto:new(ID, NuevoNombre, NuevoPrecio)]
    end.

realizar_venta(Productos, Ventas) ->
    ProductosVendidos = [],
    Total = realizar_venta_bucle(Productos, ProductosVendidos, 0),
    Ventas ++ [venta:new(erlang:length(Ventas) + 1, erlang:date(), ProductosVendidos)],
    io:fwrite("\nTotal: ~p\n", [Total]).

realizar_venta_bucle(Productos, ProductosVendidos, Total) ->
    io:fwrite("ID del producto: "),
    ID = io:get_line(),
    case lists:keyfind(ID, 1, Productos) of
        undefined -> io:fwrite("Producto no encontrado.\n"), Total;
        Producto ->
            io:fwrite("Cantidad: "),
            Cantidad = io:get_line(),
            Precio = Producto#producto.precio,
            ProductosVendidos ++ [producto:new(ID, Producto#producto.nombre, Precio * Cantidad)],
            Total + Precio * Cantidad,
            realizar_venta_bucle(Productos, ProductosVendidos, Total)
    end.

consultar_venta(Ventas) ->
    io:fwrite("ID de la venta: "),
    ID = io:get_line(),
    case lists:keyfind(ID, 1, Ventas) of
        undefined -> io:fwrite("Venta no encontrada.\n");
        Venta ->
            io:fwrite("Fecha: ~s\n", [Venta#venta.fecha]),
            io:fwrite("Productos:\n"),
            [io:fwrite(" - ~s: ~p\n", [Producto#producto.nombre, Producto#producto.precio]) || Producto <- Venta#venta.productos]
    end.
```

Este código simula un punto de venta en Erlang. El objetivo es administrar una lista de productos y permitir al usuario agregar, eliminar, modificar y vender productos, así como realizar ventas y consultar el historial de ventas.

El código utiliza el módulo `io` para interactuar con el usuario mediante la lectura de entradas y la escritura de salidas. También utiliza el módulo `lists` para manipular las listas y el módulo `erlang` para manejar las fechas y otras funciones del sistema.

El código comienza con la función `iniciar/0`, que inicia el programa y llama a la función `iniciar_bucle/2` con las listas de productos y ventas vacías como argumentos.

La función `iniciar_bucle/2` muestra un menú con las opciones disponibles y espera la entrada del usuario. En función de la opción elegida, llama a las funciones correspondientes para realizar la acción requerida.

La función `agregar_producto/1` permite al usuario agregar un nuevo producto a la lista de productos. La función `eliminar_producto/1` permite al usuario eliminar un producto de la lista de productos. La función `modificar_producto/2` permite al usuario modificar el nombre o el precio de un producto de la lista de productos.

La función `realizar_venta/2` permite al usuario realizar una venta. La función `consultar_venta/1` permite al usuario consultar el historial de ventas.

El código utiliza el módulo `record` para definir dos tipos de registros, `producto` y `venta`. El registro `producto` tiene tres campos: `id`, `nombre` y `precio`. El registro `venta` tiene tres campos: `id`, `fecha` y `productos`.

El código utiliza la cláusula `case` para manejar diferentes casos y elegir la acción adecuada en función de la entrada del usuario. El código también utiliza el operador `++` para concatenar listas, el operador `--` para eliminar elementos de una lista y el operador `lists:keyfind/3` para buscar un elemento en una lista mediante una clave.

El código utiliza la cláusula `if` para manejar diferentes casos y elegir la acción adecuada en función del resultado de una expresión. El código también utiliza el operador `erlang:halt()` para detener el programa.

El código utiliza la función `io:fwrite/2` para escribir una salida en la consola. El código también utiliza la función `io:get_line()` para leer una entrada de la consola.

El código utiliza la función `erlang:date()` para obtener la fecha actual.