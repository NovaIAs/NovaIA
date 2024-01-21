```erlang
-module(cadena_de_responsabilidades).

-export([procesar/2]).

-record(solicitud, {nombre, tipo}).

procesar(Solicitud, Cadena) ->
  case Cadena of
    [] ->
      io:format("No hay responsables para procesar la solicitud.~n");
    [Responsable | RestoCadena] ->
      case Responsable:manejar(Solicitud) of
        ok ->
          io:format("La solicitud ha sido procesada.~n");
        {error, Motivo} ->
          io:format("La solicitud no pudo ser procesada: ~p.~n", [Motivo]),
          procesar(Solicitud, RestoCadena)
      end
  end.

-record(responsable_contable, {}).

manejar(ResponsableContable, Solicitud) ->
  case Solicitud#solicitud.tipo of
    "contabilidad" ->
      io:format("El responsable contable está procesando la solicitud.~n"),
      ok;
    _ ->
      {error, "El responsable contable no puede procesar la solicitud."}
  end.

-record(responsable_compras, {}).

manejar(ResponsableCompras, Solicitud) ->
  case Solicitud#solicitud.tipo of
    "compras" ->
      io:format("El responsable de compras está procesando la solicitud.~n"),
      ok;
    _ ->
      {error, "El responsable de compras no puede procesar la solicitud."}
  end.

-record(responsable_ventas, {}).

manejar(ResponsableVentas, Solicitud) ->
  case Solicitud#solicitud.tipo of
    "ventas" ->
      io:format("El responsable de ventas está procesando la solicitud.~n"),
      ok;
    _ ->
      {error, "El responsable de ventas no puede procesar la solicitud."}
  end.

-record(solicitud_contable, {nombre, descripcion, monto}).

-record(solicitud_compras, {nombre, descripcion, cantidad}).

-record(solicitud_ventas, {nombre, descripcion, precio}).

main(_) ->
  SolicitudContable = #solicitud_contable{nombre = "Solicitud contable 1",
                                       descripcion = "Pago de facturas",
                                       monto = 10000},

  SolicitudCompras = #solicitud_compras{nombre = "Solicitud de compras 1",
                                     descripcion = "Compra de materiales",
                                     cantidad = 50},

  SolicitudVentas = #solicitud_ventas{nombre = "Solicitud de ventas 1",
                                   descripcion = "Venta de productos",
                                   precio = 20000},

  CadenaResponsables = [
    #responsable_contable{},
    #responsable_compras{},
    #responsable_ventas{}],

  procesar(SolicitudContable, CadenaResponsables),

  procesar(SolicitudCompras, CadenaResponsables),

  procesar(SolicitudVentas, CadenaResponsables),

  ok.
```

Explicación del código:

* El módulo `cadena_de_responsabilidades` define la función `procesar/2`, que recibe una solicitud y una cadena de responsables y procesa la solicitud pasando por los responsables de la cadena hasta que uno de ellos la procese con éxito o se llegue al final de la cadena.
* Los registros `responsable_contable`, `responsable_compras` y `responsable_ventas` representan los tres tipos de responsables que pueden procesar solicitudes.
* La función `manejar/2` de cada responsable recibe una solicitud y devuelve un resultado que indica si la solicitud ha sido procesada con éxito o no.
* Los registros `solicitud_contable`, `solicitud_compras` y `solicitud_ventas` representan los tres tipos de solicitudes que se pueden procesar.
* La función `main/1` crea tres solicitudes, una de cada tipo, y las procesa utilizando una cadena de responsables que contiene un responsable contable, un responsable de compras y un responsable de ventas.

El código utiliza el patrón de diseño Cadena de Responsabilidades, en el que una solicitud se pasa de un responsable a otro hasta que uno de ellos la procesa con éxito. Esto permite desacoplar los responsables de la solicitud y distribuir la responsabilidad de su procesamiento entre varios objetos.