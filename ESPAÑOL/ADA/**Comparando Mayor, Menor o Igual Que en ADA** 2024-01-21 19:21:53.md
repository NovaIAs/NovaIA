```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Mayor_Menor_Igual_Que is
	Val1, Val2 : Integer := 0;
begin
	Ada.Text_IO.Put_Line("Valores a comparar:");
	Ada.Integer_Text_IO.Get(Val1);
	Ada.Integer_Text_IO.Get(Val2);
	if Val1 = Val2 then
		Ada.Text_IO.Put_Line("Son iguales");
	elsif Val1 > Val2 then
		Ada.Text_IO.Put_Line("El primero es mayor");
	else
		Ada.Text_IO.Put_Line("El segundo es mayor");
	end if;
end Mayor_Menor_Igual_Que;
```

Explicación:

* El código compara dos valores enteros introducidos por el usuario y muestra por pantalla si son iguales, si el primero es mayor o si el segundo es mayor.
* La función Ada.Integer_Text_IO.Get() se utiliza para leer un valor entero de la entrada estándar y almacenarlo en la variable correspondiente.
* La instrucción if elsif else se utiliza para comparar los dos valores y mostrar el resultado correspondiente.
* El código está bien indentado y comentado para facilitar su comprensión.