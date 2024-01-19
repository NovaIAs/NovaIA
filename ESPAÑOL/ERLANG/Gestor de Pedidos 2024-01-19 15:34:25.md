```erlang
-module(gestor_de_pedidos).
-export([iniciar/0, procesar_pedido/1, obtener_producto/1, actualizar_stock/2]).

% Inicia el sistema de gestión de pedidos.
iniciar() ->
    register(gestor_de_pedidos, self()).

% Procesa un pedido.
procesar_pedido(Pedido) ->
    case obtener_producto(Pedido#pedido.producto) of
        {ok, Producto} ->
            actualizar_stock(Producto#producto.id, Producto#producto.stock - Pedido#pedido.cantidad);
        {error, _} ->
            error
    end.

% Obtiene un producto del catálogo.
obtener_producto(ProductoID) ->
    case whereis(catalogo) of
        undefined ->
            error;
        CatalogoPID ->
            CatalogoPID ! {obtener_producto, ProductoID},
            receive
                {producto, Producto} ->
                    {ok, Producto};
                {error, _} ->
                    error
            end
    end.

% Actualiza el stock de un producto.
actualizar_stock(ProductoID, NuevoStock) ->
    case whereis(catalogo) of
        undefined ->
            error;
        CatalogoPID ->
            CatalogoPID ! {actualizar_stock, ProductoID, NuevoStock},
            receive
                ok ->
                    ok;
                {error, _} ->
                    error
            end
    end.
```

Explicación del código:

- El módulo `gestor_de_pedidos` define las funciones necesarias para gestionar los pedidos de una tienda online.
- La función `iniciar()` inicia el sistema de gestión de pedidos y registra el proceso con el nombre `gestor_de_pedidos`.
- La función `procesar_pedido()` recibe un pedido y lo procesa. Primero, obtiene el producto del catálogo utilizando la función `obtener_producto()`. Si el producto existe, actualiza el stock del producto utilizando la función `actualizar_stock()`. Si el producto no existe, devuelve un error.
- La función `obtener_producto()` recibe el ID de un producto y obtiene el producto del catálogo. Primero, comprueba si el proceso `catalogo` está registrado. Si no lo está, devuelve un error. Si está registrado, envía un mensaje al proceso `catalogo` solicitando el producto. Cuando recibe la respuesta, la devuelve.
- La función `actualizar_stock()` recibe el ID de un producto y el nuevo stock del producto. Primero, comprueba si el proceso `catalogo` está registrado. Si no lo está, devuelve un error. Si está registrado, envía un mensaje al proceso `catalogo` solicitando que actualice el stock del producto. Cuando recibe la respuesta, la devuelve.

Este código es un ejemplo de cómo se puede utilizar Erlang para desarrollar sistemas concurrentes. Erlang es un lenguaje de programación muy adecuado para desarrollar este tipo de sistemas, ya que ofrece características como la concurrencia, la distribución y la tolerancia a fallos.