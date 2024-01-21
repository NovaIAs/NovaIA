```ada
con Generador_de_indices es
   subtype Nombres is String(1..30);
   function Generar_indice(raiz : String) return Nombres is (
      for i in Nombres'Range loop
         result := generar_nombre(raiz & natural'Image(i));
         if not Es_indice_valido(result) then
            continue;
         end if;
         return result;
      end loop;
   );

   function Es_indice_valido(indice : String) return Boolean is (
      for i in indice'Range loop
         if not indice(i) in '0' .. '9' or indice(i) in 'a' .. 'z' or indice(i) in 'A' .. 'Z' then
            return False;
         end if;
      end loop;
      return True;
   );

   function Generar_nombre(prefijo : String) return Nombres is (
      if prefijo'Length < 4 then
         return prefijo;
      end if;

      declare
         prefijos : array(1..30) of String := (prefijo'Left(i) for i in prefijo'Range);
      begin
         for i in prefijos'Range loop
            if Es_indice_valido(prefijos(i)) then
               return prefijos(i);
            end if;
         end loop;

         return prefijo;
      end;
   );

   procedure Pruebas is (
      n : Nombres := Generar_indice("A");
   begin
      while n /= "" loop
         Put_line(n);
         n := Generar_indice(n);
      end loop;
   );
begin
   Pruebas;
end Generador_de_indices;
```