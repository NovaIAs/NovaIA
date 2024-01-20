```ada
with Ada.Text_IO;                      -- Para escribir en consola
with Ada.Integer_Text_IO;               -- Para mostrar números
with Ada.Strings.Fixed;                  -- Para trabajar con cadenas de texto
with Ada.Strings.Unbounded;              -- Para trabajar con cadenas de texto ilimitadas
with Ada.Containers.Generic_Simple_Vectors;  -- Para trabajar con vectores genéricos
with Ada.Containers.Doubly_Linked_Lists;  -- Para trabajar con listas doblemente enlazadas
with Ada.Containers.Ordered_Maps;      -- Para trabajar con mapas ordenados
with Ada.Numerics.Elementary_Functions; -- Para usar funciones matemáticas básicas

procedure Cajero_Automatico is

   -- Definir el tipo de dato "Cuenta"
   type Cuenta is record
      numero : String(1..16);      -- Número de cuenta
      saldo : Float;               -- Saldo de la cuenta
   end record;

   -- Definir un vector para almacenar las cuentas
   Cuentas : Ada.Containers.Generic_Simple_Vectors.Vector(1..100) of Cuenta;

   -- Definir un mapa para almacenar los clientes por su número de identificación
   Clientes : Ada.Containers.Ordered_Maps.Map(String, Cuenta);

   -- Procedimiento para agregar una cuenta
   procedure Agregar_Cuenta(numero : String; saldo : Float) is
   begin
      -- Crear una nueva cuenta
      nueva_cuenta : Cuenta := (numero => numero, saldo => saldo);

      -- Añadir la cuenta al vector de cuentas
      Cuentas.Append(nueva_cuenta);

      -- Añadir la cuenta al mapa de clientes
      Clientes(numero) := nueva_cuenta;
   end Agregar_Cuenta;

   -- Procedimiento para consultar el saldo de una cuenta
   procedure Consultar_Saldo(numero : String) is
   begin
      -- Obtener la cuenta asociada al número de cuenta
      cuenta : Cuenta := Clientes(numero);

      -- Mostrar el saldo de la cuenta
      Ada.Text_IO.Put_Line("Saldo: " & Ada.Integer_Text_IO.Integer'Image(cuenta.saldo));
   end Consultar_Saldo;

   -- Procedimiento para depositar dinero en una cuenta
   procedure Depositar_Dinero(numero : String; cantidad : Float) is
   begin
      -- Obtener la cuenta asociada al número de cuenta
      cuenta : Cuenta := Clientes(numero);

      -- Añadir la cantidad depositada al saldo de la cuenta
      cuenta.saldo := cuenta.saldo + cantidad;

      -- Actualizar la cuenta en el vector de cuentas
      Cuentas(Cuentas.Find(cuenta)) := cuenta;

      -- Actualizar la cuenta en el mapa de clientes
      Clientes(numero) := cuenta;
   end Depositar_Dinero;

   -- Procedimiento para retirar dinero de una cuenta
   procedure Retirar_Dinero(numero : String; cantidad : Float) is
   begin
      -- Obtener la cuenta asociada al número de cuenta
      cuenta : Cuenta := Clientes(numero);

      -- Comprobar si hay suficiente saldo para retirar la cantidad
      if cuenta.saldo >= cantidad then

         -- Retirar la cantidad del saldo de la cuenta
         cuenta.saldo := cuenta.saldo - cantidad;

         -- Actualizar la cuenta en el vector de cuentas
         Cuentas(Cuentas.Find(cuenta)) := cuenta;

         -- Actualizar la cuenta en el mapa de clientes
         Clientes(numero) := cuenta;

      else

         -- Mostrar un mensaje de error
         Ada.Text_IO.Put_Line("Error: Saldo insuficiente.");

      end if;
   end Retirar_Dinero;

   -- Procedimiento para listar todas las cuentas
   procedure Listar_Cuentas is
   begin
      -- Recorrer el vector de cuentas
      for cuenta in Cuentas loop

         -- Mostrar el número de cuenta y el saldo
         Ada.Text_IO.Put_Line("Cuenta: " & cuenta.numero & ", Saldo: " & Ada.Integer_Text_IO.Integer'Image(cuenta.saldo));

      end loop;
   end Listar_Cuentas;

begin

   -- Añadir algunas cuentas de ejemplo
   Agregar_Cuenta("1234567890", 1000.0);
   Agregar_Cuenta("9876543210", 2000.0);

   -- Consultar el saldo de una cuenta
   Consultar_Saldo("1234567890");

   -- Depositar dinero en una cuenta
   Depositar_Dinero("1234567890", 500.0);

   -- Retirar dinero de una cuenta
   Retirar_Dinero("1234567890", 300.0);

   -- Listar todas las cuentas
   Listar_Cuentas;

end Cajero_Automatico;
```

Explicación del código:

* Se definen los tipos de datos y procedimientos necesarios para crear un cajero automático.
* Se definen un vector para almacenar las cuentas y un mapa para almacenar los clientes por su número de identificación.
* Se definen procedimientos para agregar una cuenta, consultar el saldo de una cuenta, depositar dinero en una cuenta, retirar dinero de una cuenta y listar todas las cuentas.
* Se añaden algunas cuentas de ejemplo al cajero automático.
* Se consulta el saldo de una cuenta para mostrar al usuario su saldo actual.
* Se deposita dinero en una cuenta para incrementar su saldo.
* Se retira dinero de una cuenta para disminuir su saldo.
* Se listan todas las cuentas para mostrar al usuario todas las cuentas disponibles.