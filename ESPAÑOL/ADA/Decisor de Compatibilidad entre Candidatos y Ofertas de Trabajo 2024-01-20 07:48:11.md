```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Decida is
   type Conjunto_de_Lenguajes is (Ada, Cobol, Eiffel, Java, Perl, Python, Ruby);
   type Tipo_de_Dato_Permitido is (Entero, Flotante, Caracter, Booleano);

   type Registro_de_Experiencia is
      Lenguaje: Conjunto_de_Lenguajes;
      Anios_de_Experiencia: Natural;
      Tipo_de_Dato_Preferido: Tipo_de_Dato_Permitido;
   end record;

   type Registro_de_Oferta is
      Nombre_de_Empresa: String(1..30);
      Posicion_Ofrecida: String(1..20);
      Salario_Ofertado: Float;
      Lenguaje_Necesario: Conjunto_de_Lenguajes;
   end record;

   Objetivo_Salarial: Float := 50000.0;

   Candidatos: array (1..5) of Registro_de_Experiencia;
   Ofertas: array (1..5) of Registro_de_Oferta;

  -- Llene las listas de candidatos y ofertas con datos de ejemplo
  Candidatos(1).Lenguaje := Ada; Candidatos(1).Anios_de_Experiencia := 5; Candidatos(1).Tipo_de_Dato_Preferido := Flotante;
  Candidatos(2).Lenguaje := Cobol; Candidatos(2).Anios_de_Experiencia := 10; Candidatos(2).Tipo_de_Dato_Preferido := Entero;
  Candidatos(3).Lenguaje := Eiffel; Candidatos(3).Anios_de_Experiencia := 2; Candidatos(3).Tipo_de_Dato_Preferido := Booleano;
  Candidatos(4).Lenguaje := Java; Candidatos(4).Anios_de_Experiencia := 7; Candidatos(4).Tipo_de_Dato_Preferido := Caracter;
  Candidatos(5).Lenguaje := Perl; Candidatos(5).Anios_de_Experiencia := 4; Candidatos(5).Tipo_de_Dato_Preferido := Entero;

  Ofertas(1).Nombre_de_Empresa := "Empresa Alfa"; Ofertas(1).Posicion_Ofrecida := "Programador Senior"; Ofertas(1).Salario_Ofertado := 60000.0; Ofertas(1).Lenguaje_Necesario := Ada;
  Ofertas(2).Nombre_de_Empresa := "Empresa Beta"; Ofertas(2).Posicion_Ofrecida := "Analista de Sistemas"; Ofertas(2).Salario_Ofertado := 55000.0; Ofertas(2).Lenguaje_Necesario := Cobol;
  Ofertas(3).Nombre_de_Empresa := "Empresa Gamma"; Ofertas(3).Posicion_Ofrecida := "Diseñador de Software"; Ofertas(3).Salario_Ofertado := 48000.0; Ofertas(3).Lenguaje_Necesario := Eiffel;
  Ofertas(4).Nombre_de_Empresa := "Empresa Delta"; Ofertas(4).Posicion_Ofrecida := "Ingeniero de Software"; Ofertas(4).Salario_Ofertado := 70000.0; Ofertas(4).Lenguaje_Necesario := Java;
  Ofertas(5).Nombre_de_Empresa := "Empresa Epsilon"; Ofertas(5).Posicion_Ofrecida := "Desarrollador Web"; Ofertas(5).Salario_Ofertado := 52000.0; Ofertas(5).Lenguaje_Necesario := Perl;

  -- Iterar a través de los candidatos y ofertas para encontrar un buen ajuste
  for i in Candidatos'Range loop
     for j in Ofertas'Range loop
        if Candidatos(i).Lenguaje = Ofertas(j).Lenguaje_Necesario and Candidatos(i).Anios_de_Experiencia >= 5 and
           Candidatos(i).Tipo_de_Dato_Preferido = Ofertas(j).Lenguaje_Necesario and Ofertas(j).Salario_Ofertado >= Objetivo_Salarial then
           -- Imprimir los detalles de la coincidencia
           Put_Line("Candidato " & Integer'Image(i) & ": " & Candidatos(i).Lenguaje'Image &
               ", " & Candidatos(i).Anios_de_Experiencia'Image & " años de experiencia, prefiere " & Candidatos(i).Tipo_de_Dato_Preferido'Image);
           Put_Line("Oferta " & Integer'Image(j) & ": " & Ofertas(j).Nombre_de_Empresa & ", " & Ofertas(j).Posicion_Ofrecida &
               ", $" & Float'Image(Ofertas(j).Salario_Ofertado));
           Put_Line("¡Esta es una buena coincidencia!");
        end if;
     end loop;
  end loop;
end Decida;
```

Explicación:

1. **Tipos de Datos Personalizados**: Se definen dos tipos de datos personalizados: `Registro_de_Experiencia` y `Registro_de_Oferta`. Estos tipos almacenan información sobre candidatos y ofertas de trabajo, respectivamente.

2. **Arrays**: Se utilizan dos arrays, `Candidatos` y `Ofertas`, para almacenar registros de candidatos y ofertas. Esto permite iterar fácilmente sobre los datos y encontrar coincidencias.

3. **Objetivo Salarial**: La variable `Objetivo_Salarial` especifica el objetivo salarial del candidato. Las ofertas con un salario inferior a este objetivo serán descartadas.

4. **Bucle Anidado**: Se utilizan dos bucles anidados para iterar a través de los arrays de candidatos y ofertas. Esto permite comparar cada candidato con cada oferta.

5. **Condiciones de Coincidencia**: Dentro de los bucles anidados, se evalúan varias condiciones para determinar si hay una coincidencia adecuada:
   - El lenguaje de programación del candidato debe coincidir con el lenguaje requerido por la oferta.
   - El candidato debe tener al menos 5 años de experiencia.
   - El tipo de dato preferido del candidato debe coincidir con el lenguaje requerido por la oferta.
   - El salario ofertado debe ser mayor o igual que el objetivo salarial del candidato.

6. **Impresión de Resultados**: Si se encuentra una coincidencia adecuada, se imprimen los detalles tanto del candidato como de la oferta, junto con un mensaje de que es una buena coincidencia.

Este código de ADA realiza una compleja comparación entre candidatos y ofertas de trabajo, teniendo en cuenta varios factores para determinar las coincidencias adecuadas.