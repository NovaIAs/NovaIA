## Diagrama de Clases

```
+----------------+
| Clase Persona |
+----------------+
| - nombre: String |
| - edad: int |
| - altura: float |
| - peso: float |
| - género: String |
+----------------+

+----------------++-----------------+
| Clase Dirección | | Clase Teléfono |
+----------------++-----------------+
| - calle: String | | - número: String |
| - número: String | | - tipo: String |
| - ciudad: String | | - compañía: String |
+----------------++-----------------+

+----------------------------+
| Clase CuentaBancaria |
+----------------------------+
| - número: String |
| - saldo: float |
| - tipo: String |
| - titular: Persona |
+----------------------------+

+-----------------------------------+
| Clase TransacciónBancaria |
+-----------------------------------+
| - fecha: Date |
| - hora: Time |
| - importe: float |
| - tipo: String |
| - cuentaOrigen: CuentaBancaria |
| - cuentaDestino: CuentaBancaria |
+-----------------------------------+

+------------------------------------+
| Clase PréstamoBancario |
+------------------------------------+
| - cantidad: float |
| - plazo: int |
| - tasaDeInterés: float |
| - cuotas: float |
| - solicitante: Persona |
+------------------------------------+
```

## Diagrama de Secuencia

```
Persona: Solicita un préstamo bancario
Banco: Verifica la información crediticia de la persona
Banco: Aprueba o rechaza el préstamo
Persona: Firma los documentos del préstamo
Banco: Desembolsa el préstamo
Persona: Realiza pagos mensuales del préstamo
Banco: Registra los pagos del préstamo
Persona: Liquida el préstamo
Banco: Cierra la cuenta del préstamo
```

## Diagrama de Estado

```
Persona: 
  - Estado inicial: Solicita un préstamo bancario
  - Estados intermedios: 
    - Información crediticia verificada
    - Préstamo aprobado
    - Préstamo rechazado
  - Estado final: Préstamo liquidado
```

## Diagrama de Actividades

```
Persona: 
  - Solicita un préstamo bancario
  - Proporciona información crediticia
  - Firma los documentos del préstamo
  - Realiza pagos mensuales del préstamo
  - Liquida el préstamo

Banco: 
  - Verifica la información crediticia de la persona
  - Aprueba o rechaza el préstamo
  - Desembolsa el préstamo
  - Registra los pagos del préstamo
  - Cierra la cuenta del préstamo
```

## Descripción del Código

El código UML anterior modela un sistema de préstamos bancarios. El sistema consta de las siguientes clases:

* **Persona:** Representa a una persona que solicita un préstamo bancario.
* **Dirección:** Representa la dirección de una persona.
* **Teléfono:** Representa el teléfono de una persona.
* **CuentaBancaria:** Representa una cuenta bancaria de una persona.
* **TransacciónBancaria:** Representa una transacción bancaria realizada por una persona.
* **PréstamoBancario:** Representa un préstamo bancario solicitado por una persona.

El código UML también incluye los siguientes diagramas:

* **Diagrama de clases:** Muestra las clases del sistema y sus relaciones.
* **Diagrama de secuencia:** Muestra la secuencia de pasos que se siguen cuando una persona solicita un préstamo bancario.
* **Diagrama de estado:** Muestra los estados por los que pasa una persona al solicitar un préstamo bancario.
* **Diagrama de actividades:** Muestra las actividades que se realizan para solicitar un préstamo bancario.

El código UML anterior es un ejemplo de cómo se puede utilizar UML para modelar un sistema de software. UML es un lenguaje de modelado gráfico que se utiliza para especificar, visualizar, construir y documentar los sistemas de software. UML es un lenguaje estándar que es independiente de la plataforma y del lenguaje de programación.