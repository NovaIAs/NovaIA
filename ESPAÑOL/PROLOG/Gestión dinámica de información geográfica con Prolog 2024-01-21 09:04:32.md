```prolog
:- dynamic pais/2.
:- dynamic provincia/3.
:- dynamic ciudad/4.
:- dynamic calle/5.
:- dynamic numero_de_puerta/6.

% Añadir un nuevo país
pais(nombre_pais, codigo_pais).

% Añadir una nueva provincia
provincia(nombre_provincia, codigo_provincia, codigo_pais).

% Añadir una nueva ciudad
ciudad(nombre_ciudad, codigo_ciudad, codigo_provincia, codigo_pais).

% Añadir una nueva calle
calle(nombre_calle, codigo_calle, codigo_ciudad, codigo_provincia, codigo_pais).

% Añadir un nuevo número de puerta
numero_de_puerta(numero_de_puerta, nombre_calle, codigo_calle, codigo_ciudad, codigo_provincia, codigo_pais).

% Obtener el código de un país por su nombre
pais(nombre_pais, codigo_pais):-
    findall(codigo_pais, pais(nombre_pais, codigo_pais), CodigosPais),
    sort(CodigosPais, [CodigoPais]),
    CodigoPais.

% Obtener el código de una provincia por su nombre y código de país
provincia(nombre_provincia, codigo_provincia, codigo_pais):-
    findall(codigo_provincia, provincia(nombre_provincia, codigo_provincia, codigo_pais), CodigosProvincia),
    sort(CodigosProvincia, [CodigoProvincia]),
    CodigoProvincia.

% Obtener el código de una ciudad por su nombre, código de provincia y código de país
ciudad(nombre_ciudad, codigo_ciudad, codigo_provincia, codigo_pais):-
    findall(codigo_ciudad, ciudad(nombre_ciudad, codigo_ciudad, codigo_provincia, codigo_pais), CodigosCiudad),
    sort(CodigosCiudad, [CodigoCiudad]),
    CodigoCiudad.

% Obtener el código de una calle por su nombre, código de ciudad, código de provincia y código de país
calle(nombre_calle, codigo_calle, codigo_ciudad, codigo_provincia, codigo_pais):-
    findall(codigo_calle, calle(nombre_calle, codigo_calle, codigo_ciudad, codigo_provincia, codigo_pais), CodigosCalle),
    sort(CodigosCalle, [CodigoCalle]),
    CodigoCalle.

% Obtener el número de puerta por su número, nombre de calle, código de calle, código de ciudad, código de provincia y código de país