```erlang
-modelulo(tarea_compleja).

-export([iniciar_tarea/0]).

 iniciar_tarea() ->
    fun_1() ! {ejecutar},
    fun_2() ! {ejecutar},
    resultado(indefinido).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fun_1() ->
    receive
        {ejecutar} -> 
            Dato1 = hacer_algo_1(),
            fun_2() ! Dato1,
            fun_1()
    end.

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fun_2() ->
    receive
        Dato1 ->
            Dato2 = hacer_algo_2(Dato1),
            fun_3() ! Dato2,
            fun_2()
    end.

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fun_3() ->
    receive
        Dato2 ->
            Dato3 = hacer_algo_3(Dato2),
            resultado(Dato3),
            fun_3()
    end.

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
resultado(Resultado) ->
    receive
    end.

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hacer_algo_1() ->
    "%Resultado de Algo 1%".

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hacer_algo_2(Dato1) ->
    "%Resultado de Dato1%".

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hacer_algo_3(Dato2) ->
    "%Resultado de Dato2%".

```

**Explicación:**

**Tarea Compleja:**
Este código representa una tarea compleja que consta de tres funciones.

**Inicio de la Tarea:**
La función "iniciar_tarea()" inicia la tarea enviando mensajes a las tres funciones "fun_1()", "fun_2()" y "fun_3()".

**Funciones:**
Cada función representa una parte de la tarea y está diseñada para recibir mensajes y realizar una acción específica:

- "fun_1()": Realiza la primera parte de la tarea y envía un mensaje a "fun_2()".
- "fun_2()": Realiza la segunda parte de la tarea y envía un mensaje a "fun_3()".
- "fun_3()": Realiza la tercera y última parte de la tarea y envía un mensaje a "resultado()".

**Resultado:**
La función "resultado()" recibe el resultado de la tarea y lo almacena en una variable "Resultado". Esta función espera más mensajes, pero como no se envían más, la tarea se completa.

**Funciones de Apoyo:**
Este código también incluye tres funciones de apoyo: "hacer_algo_1()", "hacer_algo_2()" y "hacer_algo_3()". Estas funciones realizan cálculos y generan los resultados necesarios para completar la tarea.

**Flujo de Mensajes:**
Los mensajes se pasan entre las funciones mediante el mecanismo de mensajería de Erlang, que permite una comunicación asíncrona entre procesos.

En resumen, este código demuestra una tarea compleja en Erlang, utilizando funciones que se comunican mediante mensajes para lograr un resultado final. El código se compone de múltiples funciones especializadas que trabajan juntas para completar la tarea.