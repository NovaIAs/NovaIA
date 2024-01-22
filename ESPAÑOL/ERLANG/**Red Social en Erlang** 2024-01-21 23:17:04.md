```erlang

-module(red_social).
-export([crear_red_social/0, unirse_a_red_social/2, publicar_estado/3, obtener_estado/2]).

-record(usuario, {id, nombre, estado}).
-record(red_social, {usuarios, publicaciones}).

crear_red_social() ->
    #red_social{usuarios = [], publicaciones = []}.

unirse_a_red_social(RedSocial, Usuario) ->
    NewUsuarios = [Usuario | RedSocial#red_social.usuarios],
    RedSocial#red_social{usuarios = NewUsuarios}.

publicar_estado(RedSocial, Usuario, Estado) ->
    NewPublicaciones = [{Usuario, Estado} | RedSocial#red_social.publicaciones],
    RedSocial#red_social{publicaciones = NewPublicaciones}.

obtener_estado(RedSocial, Usuario) ->
    case lists:keyfind(Usuario, 1, RedSocial#red_social.publicaciones) of
        false -> "No hay estado publicado para este usuario";
        Publicacion -> Publicacion
    end.

```

Explicación del código:

* El módulo `red_social` define una función `crear_red_social/0` que crea una nueva red social vacía, una función `unirse_a_red_social/2` que añade un nuevo usuario a la red social, una función `publicar_estado/3` que permite a un usuario publicar un estado en la red social, y una función `obtener_estado/2` que devuelve el estado publicado por un usuario.

* El registro `usuario` define los campos `id`, `nombre` y `estado` para representar a los usuarios de la red social.

* El registro `red_social` define los campos `usuarios` y `publicaciones` para representar la red social.

* La función `crear_red_social/0` crea una nueva red social vacía.

* La función `unirse_a_red_social/2` añade un nuevo usuario a la red social.

* La función `publicar_estado/3` permite a un usuario publicar un estado en la red social.

* La función `obtener_estado/2` devuelve el estado publicado por un usuario.