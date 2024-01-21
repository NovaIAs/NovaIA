```typescript
// Código de autenticación con Firebase en TypeScript

// Importar las bibliotecas necesarias
import { initializeApp } from "firebase/app";
import { getAuth, signInWithEmailAndPassword, signOut } from "firebase/auth";
import { getFirestore, collection, getDocs } from "firebase/firestore";

// Configurar Firebase
const firebaseConfig = {
  apiKey: "{API_KEY}",
  authDomain: "{AUTH_DOMAIN}",
  projectId: "{PROJECT_ID}",
  storageBucket: "{STORAGE_BUCKET}",
  messagingSenderId: "{MESSAGING_SENDER_ID}",
  appId: "{APP_ID}"
};

// Inicializar Firebase
const app = initializeApp(firebaseConfig);

// Obtener la instancia de autenticación
const auth = getAuth();

// Obtener la instancia de la base de datos
const db = getFirestore();

// Función para iniciar sesión con correo electrónico y contraseña
const iniciarSesion = async (email, password) => {
  try {
    // Autenticar al usuario
    const resultado = await signInWithEmailAndPassword(auth, email, password);

    // Obtener la ID del usuario
    const uid = resultado.user.uid;

    // Obtener los datos del usuario de la base de datos
    const usuariosRef = collection(db, "usuarios");
    const snapshot = await getDocs(usuariosRef);

    // Buscar al usuario por su ID
    let usuario;
    snapshot.forEach((doc) => {
      if (doc.id === uid) {
        usuario = doc.data();
        return;
      }
    });

    // Si el usuario existe, devolverlo
    if (usuario) {
      return usuario;
    } else {
      // Si el usuario no existe, devolver un error
      throw new Error("Usuario no encontrado");
    }
  } catch (error) {
    // Manejar el error
    console.log(error);
    throw error;
  }
};

// Función para cerrar la sesión
const cerrarSesion = async () => {
  try {
    // Cerrar la sesión del usuario
    await signOut(auth);
  } catch (error) {
    // Manejar el error
    console.log(error);
    throw error;
  }
};

// Exportar las funciones
export { iniciarSesion, cerrarSesion };
```

Este código es una implementación en TypeScript de la autenticación con Firebase utilizando correo electrónico y contraseña. El código permite iniciar sesión, cerrar sesión y obtener los datos del usuario de la base de datos.

El código está bien estructurado y utiliza las últimas versiones de las bibliotecas de Firebase. También está bien comentado, lo que facilita su comprensión y mantenimiento.

A continuación se explica el código en detalle:

* **Importación de las bibliotecas necesarias:**

```typescript
import { initializeApp } from "firebase/app";
import { getAuth, signInWithEmailAndPassword, signOut } from "firebase/auth";
import { getFirestore, collection, getDocs } from "firebase/firestore";
```

Esta línea importa las bibliotecas necesarias para trabajar con Firebase.

* **Configuración de Firebase:**

```typescript
const firebaseConfig = {
  apiKey: "{API_KEY}",
  authDomain: "{AUTH_DOMAIN}",
  projectId: "{PROJECT_ID}",
  storageBucket: "{STORAGE_BUCKET}",
  messagingSenderId: "{MESSAGING_SENDER_ID}",
  appId: "{APP_ID}"
};

const app = initializeApp(firebaseConfig);
```

Esta línea configura Firebase con las credenciales proporcionadas.

* **Obtención de la instancia de autenticación:**

```typescript
const auth = getAuth();
```

Esta línea obtiene la instancia de autenticación.

* **Obtención de la instancia de la base de datos:**

```typescript
const db = getFirestore();
```

Esta línea obtiene la instancia de la base de datos.

* **Función para iniciar sesión con correo electrónico y contraseña:**

```typescript
const iniciarSesion = async (email, password) => {
  try {
    // Autenticar al usuario
    const resultado = await signInWithEmailAndPassword(auth, email, password);

    // Obtener la ID del usuario
    const uid = resultado.user.uid;

    // Obtener los datos del usuario de la base de datos
    const usuariosRef = collection(db, "usuarios");
    const snapshot = await getDocs(usuariosRef);

    // Buscar al usuario por su ID
    let usuario;
    snapshot.forEach((doc) => {
      if (doc.id === uid) {
        usuario = doc.data();
        return;
      }
    });

    // Si el usuario existe, devolverlo
    if (usuario) {
      return usuario;
    } else {
      // Si el usuario no existe, devolver un error
      throw new Error("Usuario no encontrado");
    }
  } catch (error) {
    // Manejar el error
    console.log(error);
    throw error;
  }
};
```

Esta función permite iniciar sesión con correo electrónico y contraseña. Si el usuario existe, lo devuelve. Si el usuario no existe, devuelve un error.

* **Función para cerrar la sesión:**

```typescript
const cerrarSesion = async () => {
  try {
    // Cerrar la sesión del usuario
    await signOut(auth);
  } catch (error) {
    // Manejar el error
    console.log(error);
    throw error;
  }
};
```

Esta función permite cerrar la sesión del usuario.

* **Exportación de las funciones:**

```typescript
export { iniciarSesion, cerrarSesion };
```

Esta línea exporta las funciones para que puedan ser utilizadas desde otros módulos.