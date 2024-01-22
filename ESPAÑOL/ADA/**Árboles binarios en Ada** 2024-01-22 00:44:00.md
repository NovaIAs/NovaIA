```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Complejo_en_Ada is

    type Binary_Tree_Node is
      record
        Value : Positive;
        Left  : Binary_Tree_Node'Access;
        Right : Binary_Tree_Node'Access;
      end record;
    use Binary_Tree_Node;

    procedure Insert (Tree : in out Binary_Tree_Node'Access;
                    Value : in Positive) is
    begin
      if Tree = null then
        Tree := new Binary_Tree_Node'(Value, null, null);
      elsif Value < Tree.Value then
        Insert (Tree.Left, Value);
      else
        Insert (Tree.Right, Value);
      end if;
    end Insert;

    procedure Print (Tree : in Binary_Tree_Node'Access) is
    begin
      if Tree /= null then
        Print (Tree.Left);
        Put (Image (Tree.Value));
        New_Line;
        Print (Tree.Right);
      end if;
    end Print;

    Tree : Binary_Tree_Node'Access := null;

    for I in 1 .. 10 loop
      Insert (Tree, I);
    end loop;

    Print (Tree);
begin
  null;
end Complejo_en_Ada;
```
Explicación:

* El código crea un tipo de datos llamado `Binary_Tree_Node` que representa un nodo de un árbol binario.
* El código también define dos procedimientos, `Insert` y `Print`, que se utilizan para insertar y recorrer el árbol binario, respectivamente.
* El procedimiento `Insert` toma un nodo del árbol y un valor, e inserta el nodo en el árbol en el lugar correcto según su valor.
* El procedimiento `Print` toma un nodo del árbol y lo recorre en orden inorden, imprimiendo el valor de cada nodo en una línea.
* El código crea un árbol binario inicializando el nodo raíz del árbol en `null`, y luego llama al procedimiento `Insert` para insertar 10 valores en el árbol.
* Por último, el código llama al procedimiento `Print` para recorrer e imprimir el árbol.