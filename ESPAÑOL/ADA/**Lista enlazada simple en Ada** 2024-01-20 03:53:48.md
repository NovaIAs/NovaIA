```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors; use Ada.Containers.Vectors;

type Nodo is record
    dato : Integer;
    siguiente : Nodo'Access;
end record;

type Lista is access Nodo;

procedure Insertar_inicio(dato : Integer; lista : in out Lista) is
begin
    nuevo : Nodo := (new Nodo'(dato, lista));
    lista := nuevo;
end Insertar_inicio;

procedure Insertar_final(dato : Integer; lista : in out Lista) is
begin
    if lista = null then
        lista := (new Nodo'(dato, null));
    else
        aux : Lista := lista;
        while aux.siguiente /= null loop
            aux := aux.siguiente;
        end loop;
        aux.siguiente := (new Nodo'(dato, null));
    end if;
end Insertar_final;

procedure Eliminar_inicio(lista : in out Lista) is
begin
    if lista = null then
        Put_Line("La lista está vacía");
    else
        aux : Lista := lista.siguiente;
        lista := aux;
    end if;
end Eliminar_inicio;

procedure Eliminar_final(lista : in out Lista) is
begin
    if lista = null then
        Put_Line("La lista está vacía");
    elsif lista.siguiente = null then
        lista := null;
    else
        aux : Lista := lista;
        while aux.siguiente.siguiente /= null loop
            aux := aux.siguiente;
        end loop;
        aux.siguiente := null;
    end if;
end Eliminar_final;

procedure Mostrar_lista(lista : Lista) is
begin
    if lista = null then
        Put_Line("La lista está vacía");
    else
        aux : Lista := lista;
        while aux /= null loop
            Put(aux.dato);
            New_Line;
            aux := aux.siguiente;
        end loop;
    end if;
end Mostrar_lista;

procedure Buscar(dato : Integer; lista : Lista) is
begin
    if lista = null then
        Put_Line("La lista está vacía");
    elsif lista.dato = dato then
        Put_Line("El dato se encuentra en la lista");
    else
        Buscar(dato, lista.siguiente);
    end if;
end Buscar;

procedure main is
    lista : Lista := null;
begin
    Insertar_inicio(10, lista);
    Insertar_final(20, lista);
    Insertar_inicio(5, lista);
    Insertar_final(30, lista);

    Mostrar_lista(lista);

    Eliminar_inicio(lista);
    Eliminar_final(lista);

    Mostrar_lista(lista);

    Buscar(20, lista);
end main;
```

Este código crea una lista enlazada simple en Ada. La lista puede almacenar enteros. El código define un tipo de registro llamado `Nodo` que representa los nodos de la lista. Cada nodo tiene dos campos: `dato` que almacena el entero y `siguiente` que es un puntero a otro nodo de la lista.

El tipo `Lista` es un puntero a un nodo de la lista.

El código define cuatro procedimientos: `Insertar_inicio`, `Insertar_final`, `Eliminar_inicio` y `Eliminar_final` para insertar y eliminar elementos de la lista.

El procedimiento `Mostrar_lista` muestra los elementos de la lista.

El procedimiento `Buscar` busca un elemento en la lista.

El procedimiento `main` es el punto de entrada del programa. En este procedimiento se crea una lista, se insertan algunos elementos en la lista, se muestran los elementos de la lista, se eliminan algunos elementos de la lista y se muestra de nuevo la lista.

Al final del procedimiento `main` se llama al procedimiento `Buscar` para buscar un elemento en la lista.