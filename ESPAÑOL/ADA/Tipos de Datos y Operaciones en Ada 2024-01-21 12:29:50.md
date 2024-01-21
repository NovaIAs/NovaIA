```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

procedure CalcularEcuacion(a: in out float; b: in out float; c: in out float) is
    -- Cálculo de las raíces de una ecuación cuadrática
    discriminant
        Delta: float;
    begin
        -- Discriminante
        Delta := b * b - 4.0 * a * c;

        case Delta > 0.0 is
            when true =>
                -- Raíces reales
                Ada.Text_IO.Put_Line("Raíces reales:");
                Ada.Float_Text_IO.Put(
                    Ada.Float_Text_IO.Float'Image(
                        (-b + Ada.Float(Ada.Strings.Fixed.Positive'Root_Value)) / (2.0 * a)
                    )
                );
                Ada.Text_IO.New_Line;
                Ada.Float_Text_IO.Put(
                    Ada.Float_Text_IO.Float'Image(
                        (-b - Ada.Float(Ada.Strings.Fixed.Positive'Root_Value)) / (2.0 * a)
                    )
                );
                Ada.Text_IO.New_Line;
            when false =>
                -- Raíces complejas
                Ada.Text_IO.Put_Line("Raíces complejas:");
                Ada.Float_Text_IO.Put(
                    Ada.Float_Text_IO.Float'Image(
                        (-b / (2.0 * a))
                    )
                );
                Ada.Text_IO.Put("+");
                Ada.Float_Text_IO.Put(
                    Ada.Float_Text_IO.Float'Image(
                        Ada.Float(Ada.Strings.Fixed.Positive'Root_Value) * Ada.Float(Ada.Strings.Fixed.Positive'Root_Value) / (2.0 * a)
                    )
                );
                Ada.Text_IO.Put("i");
                Ada.Text_IO.New_Line;
                Ada.Float_Text_IO.Put(
                    Ada.Float_Text_IO.Float'Image(
                        (-b / (2.0 * a))
                    )
                );
                Ada.Text_IO.Put("-");
                Ada.Float_Text_IO.Put(
                    Ada.Float_Text_IO.Float'Image(
                        Ada.Float(Ada.Strings.Fixed.Positive'Root_Value) * Ada.Float(Ada.Strings.Fixed.Positive'Root_Value) / (2.0 * a)
                    )
                );
                Ada.Text_IO.Put("i");
                Ada.Text_IO.New_Line;
        end case;
    end CalcularEcuacion;

type Pila is array(1..10) of integer;
-- Pila para guardar números enteros

procedure OrdenarPila(pila: in out Pila) is
    -- Ordena la pila en orden ascendente
    temp: integer;
    i: integer;

    for i in pila'Range loop
        for j in i + 1..pila'Last loop
            if pila(i) > pila(j) then
                temp := pila(i);
                pila(i) := pila(j);
                pila(j) := temp;
            end if;
        end loop;
    end loop;
end OrdenarPila;

procedure ImprimirPila(pila: in Pila) is
    -- Imprime los números de la pila
    i: integer;

    for i in pila'Range loop
        Ada.Integer_Text_IO.Put(pila(i));
        Ada.Text_IO.Put(", ");
    end loop;

    Ada.Text_IO.New_Line;
end ImprimirPila;

type Lista is array(1..10) of string;
-- Lista para guardar cadenas de caracteres

procedure OrdenarLista(lista: in out Lista) is
    -- Ordena la lista en orden alfabético
    temp: string;
    i: integer;

    for i in lista'Range loop
        for j in i + 1..lista'Last loop
            if lista(i) > lista(j) then
                temp := lista(i);
                lista(i) := lista(j);
                lista(j) := temp;
            end if;
        end loop;
    end loop;
end OrdenarLista;

procedure ImprimirLista(lista: in Lista) is
    -- Imprime los elementos de la lista
    i: integer;

    for i in lista'Range loop
        Ada.Text_IO.Put(lista(i));
        Ada.Text_IO.Put(", ");
    end loop;

    Ada.Text_IO.New_Line;
end ImprimirLista;

procedure Main is
    -- Variables
    a, b, c: float;
    pila: Pila := (1, 3, 5, 2, 4, 6, 8, 7, 9, 0);
    lista: Lista := ("uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez");

    -- Cálculo de las raíces de una ecuación cuadrática
    Ada.Text_IO.Put("Ingrese los coeficientes de la ecuación cuadrática (a, b, c): ");
    Ada.Float_Text_IO.Get(a);
    Ada.Float_Text_IO.Get(b);
    Ada.Float_Text_IO.Get(c);

    CalcularEcuacion(a, b, c);

    -- Ordenamiento de la pila
    OrdenarPila(pila