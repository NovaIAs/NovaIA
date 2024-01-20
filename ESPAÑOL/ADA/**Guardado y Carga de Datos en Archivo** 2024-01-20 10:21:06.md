```ada
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

procedure Guardar_Datos is

  type Dato is record
    Nombre : String(1..20);
    Edad    : Integer;
  end record;

  type Datos is array (Positive range <>) of Dato;

  type Manejador_Datos is access Datos;

  type Archivo_Datos is new File_Type;

  type Punto_Menu is (Guardar, Cargar, Listar, Salir, Ninguno);

  Variables:
    Manejador   : Manejador_Datos;
    Archivo     : Archivo_Datos;
    Guardar_Nombre : String(1..20); -- Se usará en Guardar
    Cargar_Nombre : String(1..20); -- Se usará en Cargar
    Opcion      : Punto_Menu; -- Opción seleccionada en el menú

begin
  loop
    exit when Opcion = Ninguno; -- El usuario ha elegido salir

    Ada.Text_IO.Put_Line("¿Qué quieres hacer?");
    Ada.Text_IO.Put_Line("1. Guardar datos");
    Ada.Text_IO.Put_Line("2. Cargar datos");
    Ada.Text_IO.Put_Line("3. Listar datos");
    Ada.Text_IO.Put_Line("4. Salir");
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Get(Opcion); -- Pedimos la opción al usuario

    case Opcion is
      when Guardar =>
        Ada.Text_IO.Put("Nombre del archivo donde guardar los datos: ");
        Ada.Text_IO.Get_Line(Guardar_Nombre);
        Manejador := new Datos(10); -- Creamos un nuevo arreglo de datos
        Manejador.all(1).Nombre := "Juan";
        Manejador.all(1).Edad := 20;
        Manejador.all(2).Nombre := "María";
        Manejador.all(2).Edad := 25;
        Ada.Streams.Stream_IO.Open(Archivo, File_Mode_Out, Guardar_Nombre); -- Abrimos el archivo
        Ada.Streams.Stream_IO.Write(Archivo, Manejador'access); -- Guardamos los datos en el archivo
        Ada.Streams.Stream_IO.Close(Archivo); -- Cerramos el archivo
      when Cargar =>
        Ada.Text_IO.Put("Nombre del archivo de donde cargar los datos: ");
        Ada.Text_IO.Get_Line(Cargar_Nombre);
        Ada.Streams.Stream_IO.Open(Archivo, File_Mode_In, Cargar_Nombre); -- Abrimos el archivo
        Ada.Streams.Stream_IO.Read(Archivo, Manejador'access); -- Cargamos los datos del archivo
        Ada.Streams.Stream_IO.Close(Archivo); -- Cerramos el archivo
      when Listar =>
        if Manejador is null then
          Ada.Text_IO.Put_Line("No hay datos cargados");
        else
          for I in 1..Manejador'length loop
            Ada.Text_IO.Put_Line(Manejador.all(I).Nombre);
            Ada.Text_IO.Put_Line(Integer'Image(Manejador.all(I).Edad));
          end loop;
        end if;
      when Salir =>
        Ada.Text_IO.Put_Line("Saliendo...");
      when Ninguno =>
        null;
    end case;
  end loop;
end Guardar_Datos;
```

Explicación del código:

* El código comienza declarando varios tipos de datos, entre ellos:
    * `Dato`: Un registro que contiene el nombre y la edad de una persona.
    * `Datos`: Un arreglo de registros `Dato`.
    * `Manejador_Datos`: Un puntero a un arreglo de registros `Dato`.
    * `Archivo_Datos`: Un tipo de archivo utilizado para almacenar datos en un archivo.
    * `Punto_Menu`: Una enumeración que representa las diferentes opciones disponibles en el menú.

* El procedimiento `Guardar_Datos` es el punto de entrada principal del programa.

* El programa entra en un bucle que se ejecuta hasta que el usuario selecciona la opción "Salir".

* Dentro del bucle, el programa muestra un menú de opciones al usuario y le pide que introduzca una opción.

* Según la opción seleccionada por el usuario, el programa realiza las siguientes acciones:
    * **Guardar**: Pide al usuario el nombre de un archivo, crea un nuevo arreglo de datos, lo rellena con algunos datos de ejemplo y lo guarda en el archivo.
    * **Cargar**: Pide al usuario el nombre de un archivo, abre el archivo, carga los datos del archivo en un arreglo de datos y los muestra por pantalla.
    * **Listar**: Si hay un arreglo de datos cargado, lo muestra por pantalla. De lo contrario, muestra un mensaje indicando que no hay datos cargados.
    * **Salir**: Sale del bucle y termina el programa.

* El programa utiliza el paquete `Ada.Streams.Stream_IO` para abrir y cerrar archivos, y para leer y escribir datos en archivos.

* El programa utiliza el paquete `Ada.Text_IO` para mostrar mensajes por pantalla y para obtener la entrada del usuario.