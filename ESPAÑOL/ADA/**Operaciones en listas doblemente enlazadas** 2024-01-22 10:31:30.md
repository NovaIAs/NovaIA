```ada
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Show_List (List : Ada.Containers.Doubly_Linked_Lists.List;
                    Name : String) is
begin
  Ada.Text_IO.Put (Name);
  for Item in List'Access loop
    Ada.Integer_Text_IO.Put (Item);
    if Item'Succ' /= List'Access then
      Ada.Text_IO.Put (", ");
    end if;
  end loop;
  Ada.Text_IO.New_Line;
end Show_List;

procedure Main is
  A : Ada.Containers.Doubly_Linked_Lists.List :=
      Ada.Containers.Doubly_Linked_Lists.Create;
  B : Ada.Containers.Doubly_Linked_Lists.List :=
      Ada.Containers.Doubly_Linked_Lists.Create;
  C : Ada.Containers.Doubly_Linked_Lists.List :=
      Ada.Containers.Doubly_Linked_Lists.Create;
  H : Ada.Containers.Doubly_Linked_Lists.List :=
      Ada.Containers.Doubly_Linked_Lists.Create;

begin
  for I in 1 .. 10 loop
    Ada.Containers.Doubly_Linked_Lists.Insert (H, I);
  end loop;

  for I in 10 downto 1 loop
    Ada.Containers.Doubly_Linked_Lists.Insert (T, I);
  end loop;

  Show_List (A, "A: ");
  Show_List (B, "B: ");
  Show_List (C, "C: ");
  Show_List (H, "H: ");

  Ada.Containers.Doubly_Linked_Lists.Insert (A, 11);
  Ada.Containers.Doubly_Linked_Lists.Insert (A, 12);

  Ada.Containers.Doubly_Linked_Lists.Insert_First (B, 13);
  Ada.Containers.Doubly_Linked_Lists.Insert_Last (B, 14);

  Ada.Containers.Doubly_Linked_Lists.Copy (A, C);
  Ada.Containers.Doubly_Linked_Lists.Insert_After (C,
                                               C'Last,
                                               15);
  Ada.Containers.Doubly_Linked_Lists.Insert_Before (C,
                                                C'First,
                                                16);

  Show_List (A, "A: ");
  Show_List (B, "B: ");
  Show_List (C, "C: ");
  Show_List (H, "H: ");

  Ada.Containers.Doubly_Linked_Lists.Delete (H,
                                            Ada.Containers.Doubly_Linked_Lists.First (H));
  Ada.Containers.Doubly_Linked_Lists.Delete (H,
                                            Ada.Containers.Doubly_Linked_Lists.Last (H));

  Show_List (A, "A: ");
  Show_List (B, "B: ");
  Show_List (C, "C: ");
  Show_List (H, "H: ");

  Ada.Containers.Doubly_Linked_Lists.Reverse (A);
  Ada.Containers.Doubly_Linked_Lists.Reverse (B);

  Show_List (A, "A: ");
  Show_List (B, "B: ");
  Show_List (C, "C: ");
  Show_List (H, "H: ");

  Ada.Containers.Doubly_Linked_Lists.Merge (C, H);
  Ada.Containers.Doubly_Linked_Lists.Merge (C, A);
  Ada.Containers.Doubly_Linked_Lists.Merge (C, B);

  Show_List (C, "C: ");
end Main;
```

Este código crea cuatro listas doblemente enlazadas: A, B, C y H. Luego, muestra estas listas en la pantalla.

Luego, se realizan las siguientes operaciones en las listas:

* Se insertan los números del 1 al 10 en la lista A y los números del 10 al 1 en la lista B.
* Se copia la lista A en la lista C.
* Se insertan los números 15 y 16 en la lista C.
* Se eliminan el primer y el último elemento de la lista H.
* Se invierten las listas A y B.
* Se fusionan las listas C, H y A en la lista C.

Finalmente, se muestra la lista C en la pantalla.

Este código es bastante complejo, pero es un buen ejemplo de cómo utilizar las listas doblemente enlazadas en Ada.