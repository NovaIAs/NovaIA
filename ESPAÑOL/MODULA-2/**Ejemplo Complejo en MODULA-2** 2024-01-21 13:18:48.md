**Código en MODULA-2:**

```modula-2
MODULE EjemploComplejo;
FROM FormatString IMPORT Printf;

TYPE
  Nombre = ARRAY 20 OF CHAR;
  Persona = RECORD
    nombre: Nombre;
    edad: CARDINAL;
    altura: REAL;
  END;
  Coche = RECORD
    marca: Nombre;
    modelo: Nombre;
    matricula: Nombre;
  END;
  Garaje = ARRAY 3 OF Coche;
  Ciudad = RECORD
    nombre: Nombre;
    poblacion: CARDINAL;
    alcalde: Persona;
    garaje: Garaje;
  END;
  Pais = RECORD
    nombre: Nombre;
    capital: Ciudad;
    ciudades: ARRAY 5 OF Ciudad;
  END;

VAR
  pais1: Pais;
  pais2: Pais;

PROCEDURE ImprimirPersona(persona: Persona);
BEGIN
  Printf("Nombre: %s\n", persona.nombre);
  Printf("Edad: %d\n", persona.edad);
  Printf("Altura: %.2f\n", persona.altura);
END ImprimirPersona;

PROCEDURE ImprimirCoche(coche: Coche);
BEGIN
  Printf("Marca: %s\n", coche.marca);
  Printf("Modelo: %s\n", coche.modelo);
  Printf("Matrícula: %s\n", coche.matricula);
END ImprimirCoche;

PROCEDURE ImprimirCiudad(ciudad: Ciudad);
BEGIN
  Printf("Nombre: %s\n", ciudad.nombre);
  Printf("Población: %d\n", ciudad.poblacion);
  ImprimirPersona(ciudad.alcalde);
  FOR i := 1 TO 3 DO
    ImprimirCoche(ciudad.garaje[i]);
  END;
END ImprimirCiudad;

PROCEDURE ImprimirPais(pais: Pais);
BEGIN
  Printf("Nombre: %s\n", pais.nombre);
  ImprimirCiudad(pais.capital);
  FOR i := 1 TO 5 DO
    ImprimirCiudad(pais.ciudades[i]);
  END;
END ImprimirPais;

PROCEDURE InicializarPaises;
BEGIN
  pais1.nombre := "España";
  pais1.capital.nombre := "Madrid";
  pais1.capital.poblacion := 3200000;
  pais1.capital.alcalde.nombre := "José Luis Martínez-Almeida";
  pais1.capital.alcalde.edad := 67;
  pais1.capital.alcalde.altura := 1.82;
  FOR i := 1 TO 3 DO
    pais1.capital.garaje[i].marca := "Mercedes-Benz";
    pais1.capital.garaje[i].modelo := "Clase S";
    pais1.capital.garaje[i].matricula := "1234 ABC";
  END;
  pais1.ciudades[1].nombre := "Barcelona";
  pais1.ciudades[1].poblacion := 1600000;
  pais1.ciudades[1].alcalde.nombre := "Ada Colau";
  pais1.ciudades[1].alcalde.edad := 48;
  pais1.ciudades[1].alcalde.altura := 1.65;
  FOR i := 1 TO 3 DO
    pais1.ciudades[1].garaje[i].marca := "Audi";
    pais1.ciudades[1].garaje[i].modelo := "A8";
    pais1.ciudades[1].garaje[i].matricula := "5678 DEF";
  END;
  pais1.ciudades[2].nombre := "Valencia";
  pais1.ciudades[2].poblacion := 800000;
  pais1.ciudades[2].alcalde.nombre := "Joan Ribó";
  pais1.ciudades[2].alcalde.edad := 71;
  pais1.ciudades[2].alcalde.altura := 1.75;
  FOR i := 1 TO 3 DO
    pais1.ciudades[2].garaje[i].marca := "BMW";
    pais1.ciudades[2].garaje[i].modelo := "Serie 7";
    pais1.ciudades[2].garaje[i].matricula := "9012 GHI";
  END;
  pais2.nombre := "Francia";
  pais2.capital.nombre := "París";
  pais2.capital.poblacion := 2200000;
  pais2.capital.alcalde.nombre := "Anne Hidalgo";
  pais2.capital.alcalde.edad := 63;
  pais2.capital.alcalde.altura := 1.68;
  FOR i := 1 TO 3 DO
    pais2.capital.garaje[i].marca := "Renault";
    pais2.capital.garaje[i].modelo := "Espace";
    pais2.capital.garaje[i].matricula := "2345 A