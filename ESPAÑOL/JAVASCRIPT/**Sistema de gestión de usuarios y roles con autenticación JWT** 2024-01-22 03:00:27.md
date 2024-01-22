```javascript
// Sistema de gestión de usuarios y roles con autenticación JWT

// Importar las librerías necesarias
const express = require('express');
const mongoose = require('mongoose');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');

// Crear la aplicación Express
const app = express();

// Usar el middleware de JSON para poder leer el cuerpo de las peticiones
app.use(express.json());

// Conectarse a la base de datos de MongoDB
mongoose.connect('mongodb://localhost:27017/usuarios', { useNewUrlParser: true, useUnifiedTopology: true });

// Crear el esquema de usuario
const userSchema = new mongoose.Schema({
  nombre: String,
  email: String,
  contraseña: String,
  roles: [String]
});

// Crear el modelo de usuario
const User = mongoose.model('User', userSchema);

// Crear el esquema de rol
const roleSchema = new mongoose.Schema({
  nombre: String,
  permisos: [String]
});

// Crear el modelo de rol
const Role = mongoose.model('Role', roleSchema);

// Crear un usuario administrador y un rol de administrador
const adminRole = new Role({ nombre: 'administrador', permisos: ['crear usuarios', 'modificar usuarios', 'eliminar usuarios', 'asignar roles'] });
const adminUser = new User({ nombre: 'Juan Admin', email: 'admin@example.com', contraseña: bcrypt.hashSync('admin123', 10), roles: ['administrador'] });

adminRole.save();
adminUser.save();

// Crear un rol de usuario normal
const userRole = new Role({ nombre: 'usuario', permisos: ['ver propios datos', 'modificar propios datos'] });
userRole.save();

// Crear una función para generar un token JWT
const generateAccessToken = (user) => {
  return jwt.sign({ id: user._id, roles: user.roles }, 'claveSecreta', { expiresIn: '1h' });
};

// Crear una función para proteger las rutas que requieren autenticación
const authMiddleware = (req, res, next) => {
  const token = req.headers['authorization'];
  if (!token) {
    return res.status(401).json({ error: 'No se proporcionó el token de autenticación' });
  }

  jwt.verify(token, 'claveSecreta', (err, decoded) => {
    if (err) {
      return res.status(401).json({ error: 'Token de autenticación inválido' });
    }

    req.user = decoded;
    next();
  });
};

// Definir las rutas de la API

// Ruta para crear un usuario
app.post('/usuarios', async (req, res) => {
  const hashedPassword = await bcrypt.hash(req.body.contraseña, 10);
  const user = new User({ nombre: req.body.nombre, email: req.body.email, contraseña: hashedPassword, roles: req.body.roles });
  await user.save();
  res.status(201).json(user);
});

// Ruta para obtener todos los usuarios
app.get('/usuarios', authMiddleware, async (req, res) => {
  const users = await User.find();
  res.status(200).json(users);
});

// Ruta para obtener un usuario por su ID
app.get('/usuarios/:id', authMiddleware, async (req, res) => {
  const user = await User.findById(req.params.id);
  if (!user) {
    return res.status(404).json({ error: 'Usuario no encontrado' });
  }

  res.status(200).json(user);
});

// Ruta para modificar un usuario por su ID
app.put('/usuarios/:id', authMiddleware, async (req, res) => {
  const user = await User.findById(req.params.id);
  if (!user) {
    return res.status(404).json({ error: 'Usuario no encontrado' });
  }

  user.nombre = req.body.nombre;
  user.email = req.body.email;
  if (req.body.contraseña) {
    user.contraseña = await bcrypt.hash(req.body.contraseña, 10);
  }

  await user.save();
  res.status(200).json(user);
});

// Ruta para eliminar un usuario por su ID
app.delete('/usuarios/:id', authMiddleware, async (req, res) => {
  const user = await User.findById(req.params.id);
  if (!user) {
    return res.status(404).json({ error: 'Usuario no encontrado' });
  }

  await user.delete();
  res.status(200).json({ message: 'Usuario eliminado' });
});

// Ruta para crear un rol
app.post('/roles', authMiddleware, async (req, res) => {
  const role = new Role({ nombre: req.body.nombre, permisos: req.body.permisos });
  await role.save();
  res.status(201).json(role);
});

// Ruta para obtener todos los roles
app.get('/roles', authMiddleware, async (req, res) => {
  const roles = await Role.find();
  res.status(200).json(roles);
});

// Ruta para obtener un rol por su ID
app.get('/roles/:id', authMiddleware, async (req, res) => {
  const role = await Role.findById(req.params.id);
  if (!role) {
    return res.status(404).json({ error: 'Rol no encontrado' });
  }

  res.status(200).json(role);
});

// Ruta para modificar un rol por su ID
app.put('/roles/:id', authMiddleware, async (req, res) => {
  const role = await Role.findById(req.params.id);
  if (!role) {
    return res.status(404).json({ error: 'Rol no encontrado' });
  }

  role.nombre = req.body.nombre;
  role.permisos = req.body.permisos;

  await role.save();
  res.status(200).json(role);
});

// Ruta para eliminar un rol por su ID
app.delete('/roles/:id', authMiddleware, async (req, res) => {
  const role = await Role.findById(req.params.id);
  if (!role) {
    return res.status(404).json({ error: 'Rol no encontrado' });
  }

  await role.delete();
  res.status(200).json({ message: 'Rol eliminado' });
});

// Ruta para autenticar un usuario y obtener un token JWT
app.post('/login', async (req, res) => {
  const user = await User.findOne({ email: req.body.email });
  if (!user) {
    return res.status(404).json({ error: 'Usuario no encontrado' });
  }

  const passwordIsValid = await bcrypt.compare(req.body.contraseña, user.contraseña);
  if (!passwordIsValid) {
    return res.status(401).json({ error: 'Contraseña incorrecta' });
  }

  const token = generateAccessToken(user);
  res.status(200).json({ token });
});

// Iniciar el servidor
app.listen(3000, () => {
  console.log('El servidor está escuchando en el puerto 3000');
});
```

Explicación del código:

* Se importan las librerías necesarias.
* Se crea la aplicación Express.
* Se usa el middleware de JSON para poder leer el cuerpo de las peticiones.
* Se conecta a la base de datos de MongoDB.
* Se crean los esquemas y modelos de usuario y rol.
* Se crea un usuario administrador y un rol de administrador.
* Se crea un rol de usuario normal.
* Se crea una función para generar un token JWT.
* Se crea una función para proteger las rutas que requieren autenticación.
* Se definen las rutas de la API:
    * Crear un usuario.
    * Obtener todos los usuarios.
    * Obtener un usuario por su ID.
    * Modificar un usuario por su ID.
    * Eliminar un usuario por su ID.
    * Crear un rol.
    * Obtener todos los roles.
    * Obtener un rol por su ID.
    * Modificar un rol por su ID.
    * Eliminar un rol por su ID.
    * Autenticar un usuario y obtener un token JWT.
* Se inicia el servidor.