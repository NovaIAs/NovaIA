**Elección de un Candidato para un Puesto de Trabajo**

```php
<?php

// Función para evaluar a un candidato
function evaluarCandidato($candidato) {
  $calificación = 0;

  // Sumamos puntos por cada habilidad relevante
  if ($candidato->tieneHabilidad("Programación")) {
    $calificación += 10;
  }
  if ($candidato->tieneHabilidad("Diseño")) {
    $calificación += 5;
  }
  if ($candidato->tieneHabilidad("Marketing")) {
    $calificación += 5;
  }

  // Sumamos puntos por cada año de experiencia
  $calificación += $candidato->getExperiencia() * 2;

  // Restamos puntos por cada falta de habilidad crítica
  if (!$candidato->tieneHabilidad("Comunicación")) {
    $calificación -= 5;
  }
  if (!$candidato->tieneHabilidad("Trabajo en equipo")) {
    $calificación -= 5;
  }

  // Devolvemos la calificación final
  return $calificación;
}

// Función para seleccionar al mejor candidato
function seleccionarMejorCandidato($candidatos) {
  $mejorCandidato = null;
  $mejorCalificación = 0;

  // Iteramos sobre los candidatos
  foreach ($candidatos as $candidato) {
    // Evaluamos al candidato
    $calificación = evaluarCandidato($candidato);

    // Si la calificación del candidato es mejor que la mejor calificación actual, actualizamos al mejor candidato
    if ($calificación > $mejorCalificación) {
      $mejorCandidato = $candidato;
      $mejorCalificación = $calificación;
    }
  }

  // Devolvemos al mejor candidato
  return $mejorCandidato;
}

// Obtenemos una lista de candidatos
$candidatos = array();
$candidatos[] = new Candidato("Juan", array("Programación", "Diseño"), 2);
$candidatos[] = new Candidato("María", array("Programación", "Marketing"), 3);
$candidatos[] = new Candidato("Pedro", array("Diseño", "Marketing"), 4);

// Seleccionamos al mejor candidato
$mejorCandidato = seleccionarMejorCandidato($candidatos);

// Imprimimos el nombre del mejor candidato
echo "El mejor candidato es: " . $mejorCandidato->getNombre();

?>
```

**Explicación:**

Este código define dos funciones: `evaluarCandidato()` y `seleccionarMejorCandidato()`. La función `evaluarCandidato()` evalúa a un candidato y devuelve una calificación numérica. La función `seleccionarMejorCandidato()` toma una lista de candidatos y devuelve el mejor candidato en función de sus calificaciones.

El código también define una clase `Candidato` que representa a un candidato a un puesto de trabajo. La clase `Candidato` tiene tres propiedades: nombre, habilidades y experiencia.

El código crea una lista de candidatos y luego utiliza la función `seleccionarMejorCandidato()` para seleccionar al mejor candidato. Finalmente, el código imprime el nombre del mejor candidato.