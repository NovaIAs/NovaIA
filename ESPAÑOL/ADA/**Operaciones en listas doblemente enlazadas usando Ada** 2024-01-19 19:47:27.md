```ada
with Ada.Text_IO;    use Ada.Text_IO;

procedure Ada_complejo is
   package Ada_complejo_pkg is new Ada.Containers.Doubly_Linked_Lists (Integer);
   use Ada_complejo_pkg;
   Lista_de_enteros : Doubly_Linked_List;
   procedure Añadir_al_inicio (Elemento : in out Integer) is
      begin
         Insert (Lista_de_enteros, Elemento, First);
      end Añadir_al_inicio;
   procedure Borrar_del_inicio is
      begin
         Delete (Lista_de_enteros, First);
      end Borrar_del_inicio;
   procedure Añadir_al_final (Elemento : in out Integer) is
      begin
         Insert (Lista_de_enteros, Elemento, Last);
      end Añadir_al_final;
   procedure Borrar_del_final is
      begin
         Delete (Lista_de_enteros, Last);
      end Borrar_del_final;
   procedure Mostrar_lista is
      begin
         for I in Lista_de_enteros'Range loop
            Put (Integer'Image (Lista_de_enteros (I)));
         end loop;
         New_Line;
      end Mostrar_lista;
begin
   Añadir_al_inicio (1);
   Añadir_al_inicio (2);
   Añadir_al_inicio (3);
   Añadir_al_final (4);
   Mostrar_lista;
   Borrar_del_inicio;
   Borrar_del_final;
   Mostrar_lista;
end Ada_complejo;
```

Explicación del código:

* El paquete `Ada.Containers.Doubly_Linked_Lists` proporciona una implementación de listas doblemente enlazadas en Ada.
* La lista `Lista_de_enteros` se declara como una lista de enteros.
* Los procedimientos `Añadir_al_inicio`, `Borrar_del_inicio`, `Añadir_al_final` y `Borrar_del_final` permiten añadir y borrar elementos de la lista, ya sea al principio o al final.
* El procedimiento `Mostrar_lista` recorre la lista y muestra sus elementos en la consola.
* El programa principal crea una lista de enteros, añade algunos elementos a la lista, los borra y luego muestra el contenido de la lista en la consola.