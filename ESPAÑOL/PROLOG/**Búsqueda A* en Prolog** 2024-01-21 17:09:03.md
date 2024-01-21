```prolog

% Implementación del algoritmo de búsqueda A* en Prolog.

% Definición de la función `a*`, que recibe como argumentos:
% - Un estado inicial.
% - Un estado objetivo.
% - Una función de evaluación.
% - Una función de coste.
% Y devuelve la secuencia de acciones que conduce del estado inicial al estado objetivo.

a_estrella(Estado_Inicial, Estado_Objetivo, Evaluacion, Coste, Camino) :-
    % Creamos una cola de prioridad para almacenar los nodos a explorar.
    cola_prioridad(Cola),

    % Añadimos el estado inicial a la cola de prioridad.
    insertar_nodo(Cola, Estado_Inicial, 0).

    % Mientras la cola de prioridad no esté vacía, continuamos la búsqueda.
    mientras_no_es_vacio(Cola) :-
        % Extraemos el nodo con la mejor puntuación de la cola de prioridad.
        Extraer_nodo(Cola, Nodo),

        % Comprobamos si el nodo es el estado objetivo.
        es_estado_objetivo(Nodo, Estado_Objetivo) :-
            % Si es el estado objetivo, devolvemos el camino hasta el nodo.
            construir_camino(Nodo, Camino).

        % Si no es el estado objetivo, continuamos la búsqueda.
        si_no_es_estado_objetivo(Nodo, Estado_Objetivo) :-
            % Generamos los nodos sucesores del nodo actual.
            generar_sucesores(Nodo, Sucesores),

            % Para cada nodo sucesor, calculamos su puntuación y lo añadimos a la cola de prioridad.
            para_cada_sucesor(Sucesores) :-
                Calcular_puntuacion(Nodo, Sucesor, Puntuacion),
                insertar_nodo(Cola, Sucesor, Puntuacion),

            % Eliminamos el nodo actual de la cola de prioridad.
            eliminar_nodo(Cola, Nodo).

    % Si la cola de prioridad está vacía, significa que no se ha encontrado un camino.
    si_cola_prioridad_es_vacia() :-
        % Devolvemos un camino vacío.
        Camino = [].

% Definición de la función `construir_camino`, que recibe como argumentos:
% - Un nodo.
% - Una lista vacía.
% Y devuelve la secuencia de acciones que conduce del nodo inicial al nodo dado.

construir_camino(Nodo, Camino) :-
    % Si el nodo es el nodo inicial, devolvemos el camino vacío.
    es_nodo_inicial(Nodo) :-
        Camino = [].

    % Si no es el nodo inicial, continuamos la construcción del camino.
    si_no_es_nodo_inicial(Nodo) :-
        % Obtenemos el padre del nodo actual.
        obtener_padre(Nodo, Padre),

        % Construimos el camino desde el padre del nodo actual.
        construir_camino(Padre, Camino_Padre),

        % Añadimos la acción que conduce del padre al nodo actual al camino.
        accion(Padre, Nodo, Accion),
        Camino = Camino_Padre + Accion.

% Definición de la función `insertar_nodo`, que recibe como argumentos:
% - Una cola de prioridad.
% - Un nodo.
% - Una puntuación.
% Y añade el nodo a la cola de prioridad con la puntuación dada.

insertar_nodo(Cola, Nodo, Puntuacion) :-
    % Creamos un elemento de la cola de prioridad con el nodo y la puntuación.
    elemento(Nodo, Puntuacion),

    % Insertamos el elemento en la cola de prioridad.
    insertar(Cola, elemento(Nodo, Puntuacion)).

% Definición de la función `extraer_nodo`, que recibe como argumentos:
% - Una cola de prioridad.
% - Un nodo.
% Y extrae el nodo con la mejor puntuación de la cola de prioridad.

extraer_nodo(Cola, Nodo) :-
    % Extraemos el elemento con la mejor puntuación de la cola de prioridad.
    Extraer(Cola, Elemento),

    % Obtenemos el nodo del elemento.
    Nodo = Elemento.Nodo.

% Definición de la función `eliminar_nodo`, que recibe como argumentos:
% - Una cola de prioridad.
% - Un nodo.
% Y elimina el nodo de la cola de prioridad.

eliminar_nodo(Cola, Nodo) :-
    % Buscamos el elemento con el nodo dado en la cola de prioridad.
    Buscar(Cola, Elemento, Nodo),

    % Eliminamos el elemento de la cola de prioridad.
    Eliminar(Cola, Elemento).

% Definición de la función `generar_sucesores`, que recibe como argumentos:
% - Un nodo.
% - Una lista vacía.
% Y devuelve la lista de nodos sucesores del nodo dado.

generar_sucesores(Nodo, Sucesores) :-
    % Creamos una lista vacía para almacenar los nodos sucesores.
    Sucesores = [].

    % Para cada acción posible, aplicamos la acción al nodo actual y añadimos el nodo sucesor a la lista de sucesores.
    para_cada_accion(Acciones) :-
        accion(Nodo, Accion, Nodo_Sucesor),
        Sucesores = Sucesores + Nodo_Sucesor.

% Definición de la función `calcular_puntuacion`, que recibe como argumentos:
% - Un nodo.
% - Un nodo sucesor.
% - Una variable para almacenar la puntuación.
% Y calcula la puntuación del nodo sucesor.

calcular_puntuacion(Nodo, Nodo_Sucesor, Puntuacion) :-
    % Calculamos la función de evaluación del nodo sucesor.
    Evaluacion(Nodo_Sucesor, Evaluacion_Nodo_Sucesor),

    % Calculamos el coste del arco que conduce del nodo actual al nodo sucesor.
    Coste(Nodo, Nodo_Sucesor, Coste_Arco),

    % Sumamos la función de evaluación y el coste del arco para obtener la puntuación.
    Puntuacion = Evaluacion_Nodo_Sucesor + Coste_Arco.

% Definición de la función `es_estado_objetivo`, que recibe como argumentos:
% - Un nodo.
% - Un estado objetivo.
% Y devuelve `verdadero` si el nodo es el estado objetivo, y `falso` en caso contrario.

es_estado_objetivo(Nodo, Estado_Objetivo) :-
    Nodo = Estado_Objetivo.

% Definición de la función `es_nodo_inicial`, que recibe como argumento un nodo y devuelve `verdadero` si el nodo es el nodo inicial, y `falso` en caso contrario.

es_nodo_inicial(Nodo) :-
    Nodo = Nodo_Inicial.

% Definición de la función `obtener_padre`, que recibe como argumentos:
% - Un nodo.
% - Una variable para almacenar el padre del nodo.
% Y obtiene el padre del nodo dado.

obtener_padre(Nodo, Padre) :-
    Nodo_Padre = Padre,
    accion(Nodo_Padre, Accion, Nodo).

% Definición de la función `accion`, que recibe como argumentos:
% - Un nodo.
% - Una acción.
% - Un nodo sucesor.
% Y aplica la acción al nodo dado para obtener el nodo sucesor.

accion(Nodo, Accion, Nodo_Sucesor) :-
    Accion(Nodo, Nodo_Sucesor).
```