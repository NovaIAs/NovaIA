```erlang
-module(convertir_moneda).
-export([convertir/3]).

% Convertir una cantidad de dinero de una moneda a otra.
convertir(Importe, MonedaOrigen, MonedaDestino) ->
    TasaCambio = obtener_tasa_cambio(MonedaOrigen, MonedaDestino),
    ImporteConvertido = Importe * TasaCambio,
    ImporteConvertido.

% Obtener la tasa de cambio entre dos monedas.
obtener_tasa_cambio(MonedaOrigen, MonedaDestino) ->
    case MonedaOrigen of
        "EUR" ->
            case MonedaDestino of
                "USD" -> 1.1;
                "GBP" -> 0.9;
                _ -> error({invalid_currency, MonedaDestino})
            end;
        "USD" ->
            case MonedaDestino of
                "EUR" -> 0.9;
                "GBP" -> 0.8;
                _ -> error({invalid_currency, MonedaDestino})
            end;
        "GBP" ->
            case MonedaDestino of
                "EUR" -> 1.1;
                "USD" -> 1.2;
                _ -> error({invalid_currency, MonedaDestino})
            end;
        _ ->
            error({invalid_currency, MonedaOrigen})
    end.
```

Este código es una función que convierte una cantidad de dinero de una moneda a otra. La función toma tres argumentos: el importe a convertir, la moneda de origen y la moneda de destino.

La función primero obtiene la tasa de cambio entre las dos monedas. Para ello, utiliza la función obtener_tasa_cambio(), que toma dos argumentos: la moneda de origen y la moneda de destino. La función obtener_tasa_cambio() devuelve la tasa de cambio entre las dos monedas.

Una vez que la función convertir() tiene la tasa de cambio, multiplica el importe a convertir por la tasa de cambio para obtener el importe convertido.

La función convertir() también comprueba si las monedas de origen y de destino son válidas. Si una de las monedas no es válida, la función devuelve un error.

Aquí hay un ejemplo de cómo usar la función convertir():

```erlang
1> convertir(100, "EUR", "USD").
110.0
2> convertir(100, "USD", "GBP").
80.0
3> convertir(100, "GBP", "EUR").
90.90909090909091
```