```PROLOG // Crea una lista de ciudades y sus ubicaciones.
ciudades([
    barcelona(41.3851, 2.1734),
    madrid(40.4168, -3.7038),
    sevilla(37.3828, -5.9963),
    valencia(39.4699, -0.3759),
    bilbao(43.2630, -2.9348),
    zaragoza(41.6563, -0.8763),
    malaga(36.7201, -4.4203),
    murcia(37.9875, -1.1300),
    palma(39.5702, 2.6507),
    las_palmas(28.0997, -15.4134)
]).

// Se define una regla para determinar si un lugar está al norte de otro, en función de sus latitudes.
al_norte_de(Ciudad1, Ciudad2) :-
    ciudades(Ciudades),
    member(Ciudad1_Info, Ciudades),
    member(Ciudad2_Info, Ciudades),
    Ciudad1_Info =.. [Ciudad1, Lat1, _],
    Ciudad2_Info =.. [Ciudad2, Lat2, _],
    Lat1 > Lat2.

// En esta regla, se determina si un lugar está al sur de otro, en función de sus latitudes.
al_sur_de(Ciudad1, Ciudad2) :-
    ciudades(Ciudades),
    member(Ciudad1_Info, Ciudades),
    member(Ciudad2_Info, Ciudades),
    Ciudad1_Info =.. [Ciudad1, Lat1, _],
    Ciudad2_Info =.. [Ciudad2, Lat2, _],
    Lat1 < Lat2.

// Se establece una regla para comprobar si un lugar está al este de otro, en función de sus longitudes.
al_este_de(Ciudad1, Ciudad2) :-
    ciudades(Ciudades),
    member(Ciudad1_Info, Ciudades),
    member(Ciudad2_Info, Ciudades),
    Ciudad1_Info =.. [Ciudad1, _, Lon1],
    Ciudad2_Info =.. [Ciudad2, _, Lon2],
    Lon1 > Lon2.

// Se establece una regla para verificar si un lugar está al oeste de otro, en función de sus longitudes.
al_oeste_de(Ciudad1, Ciudad2) :-
    ciudades(Ciudades),
    member(Ciudad1_Info, Ciudades),
    member(Ciudad2_Info, Ciudades),
    Ciudad1_Info =.. [Ciudad1, _, Lon1],
    Ciudad2_Info =.. [Ciudad2, _, Lon2],
    Lon1 < Lon2.

// Se introduce una regla para determinar si un lugar está al noreste de otro, combinando las condiciones de al_norte_de y al_este_de.
al_noreste_de(Ciudad1, Ciudad2) :-
    al_norte_de(Ciudad1, Ciudad2),
    al_este_de(Ciudad1, Ciudad2).

// Se define una regla para verificar si un lugar está al sureste de otro, combinando las condiciones de al_sur_de y al_este_de.
al_sureste_de(Ciudad1, Ciudad2) :-
    al_sur_de(Ciudad1, Ciudad2),
    al_este_de(Ciudad1, Ciudad2).

// Se introduce una regla para verificar si un lugar está al noroeste de otro, combinando las condiciones de al_norte_de y al_oeste_de.
al_noroeste_de(Ciudad1, Ciudad2) :-
    al_norte_de(Ciudad1, Ciudad2),
    al_oeste_de(Ciudad1, Ciudad2).

// Se define una regla para determinar si un lugar está al suroeste de otro, combinando las condiciones de al_sur_de y al_oeste_de.
al_suroeste_de(Ciudad1, Ciudad2) :-
    al_sur_de(Ciudad1, Ciudad2),
    al_oeste_de(Ciudad1, Ciudad2).

// Se establece una regla para comprobar si un lugar está en el norte de España, basándose en su latitud.
en_el_norte_de_espana(Ciudad) :-
    ciudades(Ciudades),
    member(Ciudad_Info, Ciudades),
    Ciudad_Info =.. [Ciudad, Lat, _],
    Lat > 42.

// Se define una regla para verificar si un lugar está en el sur de España, en función de su latitud.
en_el_sur_de_espana(Ciudad) :-
    ciudades(Ciudades),
    member(Ciudad_Info, Ciudades),
    Ciudad_Info =.. [Ciudad, Lat, _],
    Lat < 42.

// Se establece una regla para verificar si un lugar está en el este de España, basándose en su longitud.
en_el_este_de_espana(Ciudad) :-
    ciudades(Ciudades),
    member(Ciudad_Info, Ciudades),
    Ciudad_Info =.. [Ciudad, _, Lon],
    Lon > -2.

// Se define una regla para verificar si un lugar está en el oeste de España, basándose en su longitud.
en_el_oeste_de_espana(Ciudad) :-
    ciudades(Ciudades),
    member(Ciudad_Info, Ciudades),
    Ciudad_Info =.. [Ciudad, _, Lon],
    Lon < -2.

// Se establece una regla para verificar si un lugar está en el centro de España, basándose en su latitud y longitud.
en_el_centro_de_espana(Ciudad) :-
    ciudades(Ciudades),
    member(Ciudad_Info, Ciudades),
    Ciudad_Info =.. [Ciudad, Lat, Lon],
    Lat >= 40, Lat =< 42,
    Lon >= -4, Lon =< 0.

// Se establece una regla para determinar si un lugar es capital de provincia, en función de su nombre.
capital_de_provincia(Ciudad) :-
    [
        barcelona,
        madrid,
        sevilla,
        valencia,
        bilbao,
        zaragoza,
        malaga,
        murcia,
        palma,
        las_palmas
    ] = CiudadesCapitales,
    member(Ciudad, CiudadesCapitales).

// Se establecen consultas de ejemplo para mostrar el uso de las reglas definidas.
// Se pregunta si Zaragoza está al norte de Sevilla.
al_norte_de(zaragoza, sevilla).

// Se pregunta si Barcelona está al sur de Murcia.
al_sur_de(barcelona, murcia).

// Se pregunta si Valencia está al este de Bilbao.
al_este_de(valencia, bilbao).

// Se pregunta si Madrid está al oeste de Málaga.
al_oeste_de(madrid, malaga).

// Se pregunta si Palma está al noreste de Las Palmas.
al_noreste_de(palma, las_palmas).

// Se pregunta si Sevilla está al sureste de Zaragoza.
al_sureste_de(sevilla, zaragoza).

// Se pregunta si Bilbao está al noroeste de Barcelona.
al_noroeste_de(bilbao, barcelona).

// Se pregunta si Murcia está al suroeste