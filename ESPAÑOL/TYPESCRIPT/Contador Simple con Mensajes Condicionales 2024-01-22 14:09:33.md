```typescript
// Importamos la biblioteca de React y la función useState para gestionar el estado del componente.
import React, { useState } from 'react';

// Creamos una función para definir el componente principal de nuestra aplicación.
const Aplicacion = () => {
  // Utilizamos el hook useState para crear una variable de estado llamada "contador" y una función para actualizarla.
  // El valor inicial del contador es 0.
  const [contador, setContador] = useState(0);

  // Creamos una función para manejar el evento de clic en el botón.
  // Esta función incrementará el contador en 1 cada vez que se haga clic en el botón.
  const handleClick = () => {
    setContador(contador + 1);
  };

  // Creamos una renderización condicional para mostrar un mensaje diferente dependiendo del valor del contador.
  let mensaje;
  if (contador === 0) {
    mensaje = 'El contador está en cero.';
  } else if (contador % 2 === 0) {
    mensaje = 'El contador es par.';
  } else {
    mensaje = 'El contador es impar.';
  }

  // Devolvemos el JSX para renderizar el componente.
  // El componente incluye un botón que llama a la función handleClick y un párrafo que muestra el mensaje.
  return (
    <div>
      <button onClick={handleClick}>Incrementar contador</button>
      <p>{mensaje}</p>
    </div>
  );
};

// Exportamos el componente para que pueda ser utilizado en otros módulos.
export default Aplicacion;
```

Explicación del código:

1. **Importación de bibliotecas:** Importamos la biblioteca de React y la función useState para gestionar el estado del componente.
2. **Definición del componente principal:** Creamos una función para definir el componente principal de nuestra aplicación, llamado `Aplicacion`.
3. **Creación de la variable de estado:** Utilizamos el hook useState para crear una variable de estado llamada "contador" y una función para actualizarla. El valor inicial del contador es 0.
4. **Definición de la función para manejar el evento de clic:** Creamos una función llamada `handleClick` para manejar el evento de clic en el botón. Esta función incrementará el contador en 1 cada vez que se haga clic en el botón.
5. **Renderización condicional:** Creamos una renderización condicional para mostrar un mensaje diferente dependiendo del valor del contador. El mensaje cambiará en función de si el contador es 0, par o impar.
6. **Retorno del JSX:** Devolvemos el JSX para renderizar el componente. El componente incluye un botón que llama a la función handleClick y un párrafo que muestra el mensaje.
7. **Exportación del componente:** Exportamos el componente para que pueda ser utilizado en otros módulos.