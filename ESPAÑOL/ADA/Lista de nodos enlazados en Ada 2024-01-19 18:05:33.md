```ada
with Ada.Text_IO;

procedure Complejo is

  type Elemento is new String(1..20);

  type Nodo is record
    info  : Elemento;
    next  : Nodo'Access;
  end record;

  type Lista is access Nodo;

  procedure Inserta_Principio(Lista: in out Lista; E: in Elemento) is
  begin
    New_Nodo := new Nodo(E, Lista);
    Lista := New_Nodo;
  end Inserta_Principio;

  procedure Inserta_Final(Lista: in out Lista; E: in Elemento) is
  begin
    if Lista = null then
      New_Nodo := new Nodo(E, null);
      Lista := New_Nodo;
    else
      Inserta_Final(Lista.next, E);
    end if;
  end Inserta_Final;

  procedure Elimina_Principio(Lista: in out Lista) is
  begin
    if Lista = null then
      Ada.Text_IO.Put_Line("La lista está vacía");
    else
      Lista := Lista.next;
    end if;
  end Elimina_Principio;

  procedure Elimina_Final(Lista: in out Lista) is
  begin
    if Lista = null then
      Ada.Text_IO.Put_Line("La lista está vacía");
    elsif Lista.next = null then
      Lista := null;
    else
      Elimina_Final(Lista.next);
    end if;
  end Elimina_Final;

  procedure Imprimir(Lista: in Lista) is
  begin
    if Lista = null then
      Ada.Text_IO.Put_Line("La lista está vacía");
    else
      Ada.Text_IO.Put_Line(Lista.info);
      Imprimir(Lista.next);
    end if;
  end Imprimir;

  function Buscar(Lista: in Lista; E: in Elemento) return Boolean is
  begin
    if Lista = null then
      return False;
    elsif Lista.info = E then
      return True;
    else
      return Buscar(Lista.next, E);
    end if;
  end Buscar;

begin
  Mi_Lista := null;

  Inserta_Principio(Mi_Lista, "Juan");
  Inserta_Principio(Mi_Lista, "María");
  Inserta_Final(Mi_Lista, "Pedro");
  Inserta_Final(Mi_Lista, "Ana");

  Imprimir(Mi_Lista);

  Elimina_Principio(Mi_Lista);
  Elimina_Final(Mi_Lista);

  Imprimir(Mi_Lista);

  if Buscar(Mi_Lista, "Pedro") then
    Ada.Text_IO.Put_Line("Pedro está en la lista");
  else
    Ada.Text_IO.Put_Line("Pedro no está en la lista");
  end if;
end Complejo;
```

Este código define una lista de nodos enlazados. Cada nodo contiene un elemento (una cadena de caracteres) y una referencia al siguiente nodo de la lista. Las operaciones sobre la lista son: insertar un elemento al principio de la lista, insertar un elemento al final de la lista, eliminar el primer elemento de la lista, eliminar el último elemento de la lista, imprimir la lista, y buscar un elemento en la lista.

El código también define un tipo de datos llamado Elemento, que es una cadena de caracteres de longitud máxima 20. El tipo de datos Nodo es un registro que contiene un elemento y una referencia al siguiente nodo de la lista. La lista es una variable de tipo Lista, que es un puntero a un nodo.

Las procedimientos Inserta_Principio e Inserta_Final insertan un elemento al principio y al final de la lista, respectivamente. El procedimiento Elimina_Principio elimina el primer elemento de la lista, y el procedimiento Elimina_Final elimina el último elemento de la lista. El procedimiento Imprimir imprime la lista. El procedimiento Buscar busca un elemento en la lista y devuelve True si lo encuentra, o False si no lo encuentra.

El programa principal crea una lista vacía, inserta los elementos "Juan", "María", "Pedro" y "Ana" en la lista, imprime la lista, elimina el primer y el último elemento de la lista, imprime la lista de nuevo, y busca el elemento "Pedro" en la lista.