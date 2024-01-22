```smalltalk
(Objeto sucursal) new: 'BBVA'

(Objeto sucursal) new: 'Santander'
(Objeto sucursal) new: 'La Caixa'
(Objeto sucursal) new: 'Sabadell'
(Objeto sucursal) new: 'Bankia'

(Objeto empleado) new: 'Juan'
(Objeto empleado) new: 'María'
(Objeto empleado) new: 'José'
(Objeto empleado) new: 'Ana'
(Objeto empleado) new: 'Luis'

(Objeto sucursal) allDo: [ :sucursal |
    sucursal allDo: [ :empleado |
        empleado trabajo: 'Cliente' ] ]

(Objeto cliente) new: 'Antonio'
(Objeto cliente) new: 'Beatriz'
(Objeto cliente) new: 'Carlos'
(Objeto cliente) new: 'Dolores'
(Objeto cliente) new: 'Enrique'

(Objeto sucursal) allDo: [ :sucursal |
    sucursal clientesDo: [ :cliente |
        cliente comprar: (Objeto artículo) new: 'Teléfono' ] ]

(Objeto sucursal) allDo: [ :sucursal |
    sucursal clientesDo: [ :cliente |
        cliente comprar: (Objeto artículo) new: 'Ordenador' ] ]

(Objeto sucursal) allDo: [ :sucursal |
    sucursal clientesDo: [ :cliente |
        cliente comprar: (Objeto artículo) new: 'Televisión' ] ]
```

Explicación:

1. Definimos la clase `Sucursal` y creamos instancias de sucursales bancarias utilizando el método `new`.

2. Definimos la clase `Empleado` y creamos instancias de empleados utilizando el método `new`.

3. Asignamos a cada empleado un trabajo utilizando el método `trabajo`.

4. Definimos la clase `Cliente` y creamos instancias de clientes utilizando el método `new`.

5. Asignamos a cada sucursal una lista de clientes utilizando el método `clientesDo`.

6. Asignamos a cada cliente una lista de artículos comprados utilizando el método `comprar`.

7. Utilizamos el método `allDo` para iterar sobre las sucursales y realizar operaciones en cada una de ellas.

8. Utilizamos el método `clientesDo` para iterar sobre los clientes de cada sucursal y realizar operaciones en cada uno de ellos.

9. Utilizamos el método `comprar` para asignar a cada cliente una lista de artículos comprados.