```ada
-- Este código simula una red neuronal artificial de perceptrón multicapa para el reconocimiento de imágenes.

-- Definición de los módulos principales
module Cuerpo;
   use Std_Logic_Vector;

   -- Tipo de datos para las neuronas
   type Neurona is record
      pesos: Std_Logic_Vector(0 to 7);
      umbral: Std_Logic;
      salida: Std_Logic;
   end record;

   -- Tipo de datos para las capas de neuronas
   type Capa is array (Std_Logic_Vector) of Neurona;

   -- Procedimiento para inicializar una capa de neuronas
   procedure Inicializar_Capa (capa: out Capa);

   -- Procedimiento para entrenar una capa de neuronas
   procedure Entrenar_Capa (capa: out Capa; patrones: in Std_Logic_Vector(0 to 7); etiquetas: in Std_Logic);

   -- Procedimiento para simular una capa de neuronas
   procedure Simular_Capa (capa: in out Capa; patron: in Std_Logic_Vector; etiqueta: out Std_Logic);
end Cuerpo;

-- Implementación de los módulos principales
with Cuerpo;
use Cuerpo;

package body Cuerpo is

   -- Procedimiento para inicializar una capa de neuronas
   procedure Inicializar_Capa (capa: out Capa) is
      begin
         for i in capa'range loop
            capa(i).pesos(0 to 7) := (others => '0');
            capa(i).umbral := '0';
            capa(i).salida := '0';
         end loop;
      end Inicializar_Capa;

   -- Procedimiento para entrenar una capa de neuronas
   procedure Entrenar_Capa (capa: out Capa; patrones: in Std_Logic_Vector(0 to 7); etiquetas: in Std_Logic) is
      begin
         for i in capa'range loop
            capa(i).pesos(0 to 7) := patrones;
            capa(i).umbral := etiquetas;
         end loop;
      end Entrenar_Capa;

   -- Procedimiento para simular una capa de neuronas
   procedure Simular_Capa (capa: in out Capa; patron: in Std_Logic_Vector; etiqueta: out Std_Logic) is
      begin
         for i in capa'range loop
            capa(i).salida := '0';
            for j in 0 to 7 loop
               capa(i).salida := capa(i).salida or (patron(j) and capa(i).pesos(j));
            end loop;
            if capa(i).salida >= capa(i).umbral then
               capa(i).salida := '1';
            end if;
         end loop;
         etiqueta := capa(0).salida;
      end Simular_Capa;

end Cuerpo;

-- Módulo de prueba
with Cuerpo;
use Cuerpo;

procedure Prueba is
   type Patrones is array (Std_Logic_Vector) of Std_Logic_Vector;
   type Etiquetas is array (Std_Logic) of Std_Logic;
   type Capa1 is array (Std_Logic_Vector(0 to 7)) of Neurona;
   type Capa2 is array (Std_Logic_Vector(0 to 2)) of Neurona;

   patrones: Patrones := ((
      "00000000", "00000001", "00000010", "00000011",
      "00000100", "00000101", "00000110", "00000111",
      "00001000", "00001001", "00001010", "00001011",
      "00001100", "00001101", "00001110", "00001111"
   ), (
      "00000001", "00000010", "00000011", "00000100",
      "00000101", "00000110", "00000111", "00001000",
      "00001001", "00001010", "00001011", "00001100",
      "00001101", "00001110", "00001111", "00010000"
   ), (
      "00000010", "00000011", "00000100", "00000101",
      "00000110", "00000111", "00001000", "00