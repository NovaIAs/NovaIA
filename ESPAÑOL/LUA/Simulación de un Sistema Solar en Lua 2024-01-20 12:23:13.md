```lua
-- Este código crea una simulación de un sistema solar con planetas orbitando alrededor del sol.

-- Definir las masas de los planetas y del sol.
masaSol = 1.989e30 -- kg
masaTierra = 5.972e24 -- kg
masaLuna = 7.348e22 -- kg
masaMarte = 6.417e23 -- kg
masaJupiter = 1.898e27 -- kg
masaSaturno = 5.684e26 -- kg
masaUrano = 8.681e25 -- kg
masaNeptuno = 1.024e26 -- kg

-- Definir la constante de gravitación universal.
G = 6.674e-11 -- m^3 kg^-1 s^-2

-- Definir las posiciones iniciales de los planetas y del sol.
posicionSol = {0, 0, 0} -- m
posicionTierra = {0, 1.5e11, 0} -- m
posicionLuna = {4.4e8, 1.5e11, 0} -- m
posicionMarte = {2.4e11, 0, 0} -- m
posicionJupiter = {7.7e11, 0, 0} -- m
posicionSaturno = {1.4e12, 0, 0} -- m
posicionUrano = {2.8e12, 0, 0} -- m
posicionNeptuno = {4.5e12, 0, 0} -- m

-- Definir las velocidades iniciales de los planetas y del sol.
velocidadSol = {0, 0, 0} -- m/s
velocidadTierra = {29.7e3, 0, 0} -- m/s
velocidadLuna = {0, 1.2e3, 0} -- m/s
velocidadMarte = {24.1e3, 0, 0} -- m/s
velocidadJupiter = {13.1e3, 0, 0} -- m/s
velocidadSaturno = {9.6e3, 0, 0} -- m/s
velocidadUrano = {6.8e3, 0, 0} -- m/s
velocidadNeptuno = {5.4e3, 0, 0} -- m/s

-- Definir el intervalo de tiempo.
dt = 3600 -- s

-- Crear una tabla para almacenar las posiciones y velocidades de los planetas y del sol.
cuerpos = {}
cuerpos[1] = {posicionSol, velocidadSol}
cuerpos[2] = {posicionTierra, velocidadTierra}
cuerpos[3] = {posicionLuna, velocidadLuna}
cuerpos[4] = {posicionMarte, velocidadMarte}
cuerpos[5] = {posicionJupiter, velocidadJupiter}
cuerpos[6] = {posicionSaturno, velocidadSaturno}
cuerpos[7] = {posicionUrano, velocidadUrano}
cuerpos[8] = {posicionNeptuno, velocidadNeptuno}

-- Simular el sistema solar durante 100 años.
for t = 0, 100 * 365 * 24 * 60 * 60, dt do
  -- Calcular las fuerzas entre los planetas y el sol.
  fuerzas = {}
  for i = 1, #cuerpos do
    fuerzas[i] = {0, 0, 0} -- N
    for j = 1, #cuerpos do
      if i ~= j then
        r = {cuerpos[j][1][1] - cuerpos[i][1][1], cuerpos[j][1][2] - cuerpos[i][1][2], cuerpos[j][1][3] - cuerpos[i][1][3]} -- m
        r_mag = math.sqrt(r[1]^2 + r[2]^2 + r[3]^2) -- m
        fuerzas[i] = fuerzas[i] + {G * masaSol * masaTierra / r_mag^3 * r[1], G * masaSol * masaTierra / r_mag^3 * r[2], G * masaSol * masaTierra / r_mag^3 * r[3]} -- N
      end
    end
  end

  -- Actualizar las posiciones y velocidades de los planetas y del sol.
  for i = 1, #cuerpos do
    cuerpos[i][1] = cuerpos[i][1] + cuerpos[i][2] * dt -- m
    cuerpos[i][2] = cuerpos[i][2] + fuerzas[i] / masaSol * dt -- m/s
  end
end

-- Mostrar las posiciones finales de los planetas y del sol.
for i = 1, #cuerpos do
  print(cuerpos[i][1])
end
```

Explicación del código:

* En primer lugar, se definen las masas de los planetas y del sol, así como la constante de gravitación universal.
* A continuación, se definen las posiciones y velocidades iniciales de los planetas y del sol.
* Se crea una tabla para almacenar las posiciones y velocidades de los planetas y del sol.
* Se simula el sistema solar durante 100 años.
* Por último, se muestran las posiciones finales de los planetas y del sol.