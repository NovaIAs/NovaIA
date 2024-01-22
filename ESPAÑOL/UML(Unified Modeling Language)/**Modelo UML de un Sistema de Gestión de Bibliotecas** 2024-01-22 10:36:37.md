```
<?xml version="1.0" encoding="UTF-8"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            xmlns:uml="http://schema.omg.org/spec/UML/2.1.2/Infrastructure"
            xmlns:xmi="http://www.omg.org/XMI"
            targetNamespace="http://www.example.org/uml/class"
            elementFormDefault="qualified">
  <xmi:Documentation>
    Este es un modelo UML de un sistema de gestión de bibliotecas.
  </xmi:Documentation>
  <xsd:complexType name="Biblioteca">
    <xsd:sequence>
      <xsd:element name="nombre" type="xsd:string"/>
      <xsd:element name="dirección" type="xsd:string"/>
      <xsd:element name="libros" type="xsd:IDREFS"/>
      <xsd:element name="socios" type="xsd:IDREFS"/>
      <xsd:element name="préstamos" type="xsd:IDREFS"/>
    </xsd:sequence>
  </xsd:complexType>
  <xsd:complexType name="Libro">
    <xsd:sequence>
      <xsd:element name="título" type="xsd:string"/>
      <xsd:element name="autor" type="xsd:string"/>
      <xsd:element name="editorial" type="xsd:string"/>
      <xsd:element name="año" type="xsd:int"/>
    </xsd:sequence>
  </xsd:complexType>
  <xsd:complexType name="Socio">
    <xsd:sequence>
      <xsd:element name="nombre" type="xsd:string"/>
      <xsd:element name="apellidos" type="xsd:string"/>
      <xsd:element name="DNI" type="xsd:string"/>
      <xsd:element name="dirección" type="xsd:string"/>
      <xsd:element name="email" type="xsd:string"/>
      <xsd:element name="préstamos" type="xsd:IDREFS"/>
    </xsd:sequence>
  </xsd:complexType>
  <xsd:complexType name="Préstamo">
    <xsd:sequence>
      <xsd:element name="fechaPréstamo" type="xsd:date"/>
      <xsd:element name="fechaDevolución" type="xsd:date"/>
      <xsd:element name="libro" type="xsd:IDREF"/>
      <xsd:element name="socio" type="xsd:IDREF"/>
    </xsd:sequence>
  </xsd:complexType>
  <xsd:element name="Biblioteca" type="Biblioteca"/>
  <xsd:element name="Libro" type="Libro"/>
  <xsd:element name="Socio" type="Socio"/>
  <xsd:element name="Préstamo" type="Préstamo"/>
</xsd:schema>
```

Este código UML define un modelo de un sistema de gestión de bibliotecas. El modelo incluye las siguientes clases:

* Biblioteca: Una biblioteca tiene un nombre, una dirección, una lista de libros, una lista de socios y una lista de préstamos.
* Libro: Un libro tiene un título, un autor, una editorial y un año de publicación.
* Socio: Un socio tiene un nombre, un apellido, un DNI, una dirección y un correo electrónico.
* Préstamo: Un préstamo tiene una fecha de préstamo, una fecha de devolución, un libro y un socio.

El modelo también incluye las siguientes relaciones:

* Una biblioteca puede tener muchos libros.
* Una biblioteca puede tener muchos socios.
* Una biblioteca puede tener muchos préstamos.
* Un libro puede ser prestado por muchos socios.
* Un socio puede prestar muchos libros.
* Un préstamo está relacionado con un libro y un socio.

Este modelo UML se puede utilizar para generar código fuente en varios lenguajes de programación. También se puede utilizar para documentar el sistema de gestión de bibliotecas.