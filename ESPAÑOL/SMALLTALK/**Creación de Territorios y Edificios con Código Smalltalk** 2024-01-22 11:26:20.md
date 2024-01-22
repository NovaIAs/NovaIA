```smalltalk
Territorio := Municipio new nombre: 'Territorio'.
Población := Municipio new nombre: 'Población'.
Hospital := Municipio new nombre: 'Hospital'.
Parque := Municipio new nombre: 'Parque'.
Residencial := Municipio new nombre: 'Residencial'.
Comercial := Municipio new nombre: 'Comercial'.
Industrial := Municipio new nombre: 'Industrial'.

Territorio addMunicipio: Población.
Territorio addMunicipio: Hospital.
Territorio addMunicipio: Parque.
Territorio addMunicipio: Residencial.
Territorio addMunicipio: Comercial.
Territorio addMunicipio: Industrial.

Territorio addEdificio: Escuela one Municipio: Población.
Territorio addEdificio: Universidad one Municipio: Población.
Territorio addEdificio: Hospital one Municipio: Hospital.
Territorio addEdificio: ParqueCentral one Municipio: Parque.
Territorio addEdificio: EdificioResidencial one Municipio: Residencial.
Territorio addEdificio: EdificioComercial one Municipio: Comercial.
Territorio addEdificio: Fábrica one Municipio: Industrial.

Territorio show.

Escuela := Territorio municipioNamed: 'Población' edificioNamed: 'Escuela'.
Universidad := Territorio municipioNamed: 'Población' edificioNamed: 'Universidad'.
Hospital := Territorio municipioNamed: 'Hospital' edificioNamed: 'Hospital'.
ParqueCentral := Territorio municipioNamed: 'Parque' edificioNamed: 'ParqueCentral'.
EdificioResidencial := Territorio municipioNamed: 'Residencial' edificioNamed: 'EdificioResidencial'.
EdificioComercial := Territorio municipioNamed: 'Comercial' edificioNamed: 'EdificioComercial'.
Fábrica := Territorio municipioNamed: 'Industrial' edificioNamed: 'Fábrica'.

```

### Explicación:

1. Definimos los objetos `Territorio`, `Población`, `Hospital`, `Parque`, `Residencial`, `Comercial`, `Industrial`, que son de tipo `Municipio`.


2. Luego, agregamos los municipios a `Territorio` utilizando el método `addMunicipio`.


3. Añadimos edificios a cada municipio utilizando el método `addEdificio`.


4. Utilizamos el método `show` para mostrar la estructura del territorio.


5. Recuperamos los edificios mediante el método `municipioNamed` y `edificioNamed`.