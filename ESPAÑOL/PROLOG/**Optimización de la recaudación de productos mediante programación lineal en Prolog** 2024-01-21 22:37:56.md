```prolog
:- use_module(library(clpfd)).

% Definición de las variables
producto(PC, RO, P, Q, R).
% PC: Precio de costo
% RO: Rendimiento operativo
% P: Precio de venta
% Q: Cantidad producida
% R: Recaudación

% Restricciones
% 1. El rendimiento operativo es del 10% del precio de costo
:- foreach(producto(PC, RO, _, _, _)) in all_solutions do
    RO #= PC * 0.1.
% 2. El precio de venta es el precio de costo más el rendimiento operativo
:- foreach(producto(PC, _, P, _, _)) in all_solutions do
    P #= PC + RO.
% 3. La cantidad producida es de 100 unidades
:- foreach(producto(_, _, _, Q, _)) in all_solutions do
    Q #= 100.
% 4. La recaudación es el precio de venta por la cantidad producida
:- foreach(producto(_, _, P, Q, R)) in all_solutions do
    R #= P * Q.

% Objetivo
:- maximize(sum([R]), [producto(_, _, _, _, R)]).

% Impresión de la solución
:- foreach(producto(PC, RO, P, Q, R)) in all_solutions do
    writeln(producto: PC, RO, P, Q, R).
```

Explicación del código:

1. Definición de las variables:
   - `producto(PC, RO, P, Q, R)`: Representa un producto con su precio de costo, rendimiento operativo, precio de venta, cantidad producida y recaudación.

2. Restricciones:
   - La primera restricción establece que el rendimiento operativo es el 10% del precio de costo.
   - La segunda restricción establece que el precio de venta es el precio de costo más el rendimiento operativo.
   - La tercera restricción establece que la cantidad producida es de 100 unidades.
   - La cuarta restricción establece que la recaudación es el precio de venta por la cantidad producida.

3. Objetivo:
   - El objetivo del programa es maximizar la recaudación total de todos los productos.

4. Impresión de la solución:
   - El programa imprime la solución que maximiza la recaudación total, mostrando los valores de las variables para cada producto.