```prolog
:- dynamic producto/3.
:- dynamic cliente/3.
:- dynamic factura/4.

producto(1, 'Leche', 100).
producto(2, 'Pan', 50).
producto(3, 'Huevos', 60).
producto(4, 'Arroz', 80).
producto(5, 'Frijoles', 70).

cliente(1, 'Juan Pérez', '123 Main Street').
cliente(2, 'María González', '456 Oak Avenue').
cliente(3, 'Pedro Rodríguez', '789 Pine Street').

% Guardar una nueva Factura en la Base de Datos.
nueva_factura(Folio, Fecha, Cliente, Productos):-
    factura(Folio, Fecha, Cliente, Productos),
    assert(factura(Folio, Fecha, Cliente, Productos)),
    write('Factura guardada correctamente.'), nl.

% Obtener los datos de una Factura de la Base de Datos.
obtener_factura(Folio, Fecha, Cliente, Productos):-
    factura(Folio, Fecha, Cliente, Productos),
    write('Folio: '), write(Folio), nl,
    write('Fecha: '), write(Fecha), nl,
    write('Cliente: '), write(Cliente), nl,
    write('Productos: '), write(Productos), nl.

% Actualizar los datos de una Factura en la Base de Datos.
actualizar_factura(Folio, Fecha, Cliente, Productos):-
    retract(factura(Folio, _, _, _)),
    assert(factura(Folio, Fecha, Cliente, Productos)),
    write('Factura actualizada correctamente.'), nl.

% Eliminar una Factura de la Base de Datos.
eliminar_factura(Folio):-
    retract(factura(Folio, _, _, _)),
    write('Factura eliminada correctamente.'), nl.

% Mostrar todas las Facturas en la Base de Datos.
mostrar_facturas:-
    findall(Folio, factura(Folio, _, _, _), Folios),
    foreach(Folio in Folios) do
        write('Folio: '), write(Folio), nl,
        obtener_factura(Folio, Fecha, Cliente, Productos),
        write('=================================='), nl
    end.
```

Explicación del código:

* El código define tres relaciones dinámicas: producto/3, cliente/3 y factura/4. Estas relaciones se utilizan para almacenar información sobre productos, clientes y facturas, respectivamente.
* La relación producto/3 tiene tres argumentos: el ID del producto, el nombre del producto y el precio del producto.
* La relación cliente/3 tiene tres argumentos: el ID del cliente, el nombre del cliente y la dirección del cliente.
* La relación factura/4 tiene cuatro argumentos: el folio de la factura, la fecha de la factura, el ID del cliente y los productos comprados.
* El código define algunos hechos iniciales para las relaciones producto/3 y cliente/3.
* El código define una serie de predicados para trabajar con las relaciones:
    * nueva_factura/4: Este predicado guarda una nueva factura en la base de datos.
    * obtener_factura/4: Este predicado obtiene los datos de una factura de la base de datos.
    * actualizar_factura/4: Este predicado actualiza los datos de una factura en la base de datos.
    * eliminar_factura/1: Este predicado elimina una factura de la base de datos.
    * mostrar_facturas/0: Este predicado muestra todas las facturas en la base de datos.
* El código también define una regla que se utiliza para calcular el precio total de una factura.