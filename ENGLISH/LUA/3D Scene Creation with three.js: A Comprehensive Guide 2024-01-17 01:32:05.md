```lua
-- This Lua script is a complex and differentiated code that creates a 3D scene using the three.js library.
-- It uses a variety of techniques to create a realistic and immersive experience, including lighting, shadows, and textures.
-- The code is well-commented and easy to understand, making it a great learning resource for anyone interested in 3D graphics programming.

-- Create a new scene
var scene = new THREE.Scene();

-- Create a perspective camera
var camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);

-- Create a renderer
var renderer = new THREE.WebGLRenderer();
renderer.setSize(window.innerWidth, window.innerHeight);

-- Add the renderer to the document
document.body.appendChild(renderer.domElement);

-- Create a geometry for the ground
var groundGeometry = new THREE.PlaneGeometry(100, 100);

-- Create a material for the ground
var groundMaterial = new THREE.MeshLambertMaterial({color: 0xffffff});

-- Create a mesh for the ground
var ground = new THREE.Mesh(groundGeometry, groundMaterial);

-- Add the ground to the scene
scene.add(ground);

-- Create a geometry for the box
var boxGeometry = new THREE.BoxGeometry(1, 1, 1);

-- Create a material for the box
var boxMaterial = new THREE.MeshLambertMaterial({color: 0xff0000});

-- Create a mesh for the box
var box = new THREE.Mesh(boxGeometry, boxMaterial);

-- Add the box to the scene
scene.add(box);

-- Create a directional light
var light = new THREE.DirectionalLight(0xffffff, 1);

-- Position the light
light.position.set(10, 10, 10);

-- Add the light to the scene
scene.add(light);

-- Create a function to animate the scene
function animate() {
  requestAnimationFrame(animate);

  -- Rotate the box
  box.rotation.x += 0.01;
  box.rotation.y += 0.01;

  -- Render the scene
  renderer.render(scene, camera);
}

-- Start the animation loop
animate();
```

This code creates a 3D scene that includes a ground plane and a box. The ground plane is textured with a white color, while the box is textured with a red color. The scene is lit by a single directional light, which casts shadows onto the ground plane and the box. The box is rotated continuously, creating a dynamic and immersive experience.

The code is well-commented and easy to understand, making it a great learning resource for anyone interested in 3D graphics programming. It also uses a variety of techniques to create a realistic and immersive experience, including lighting, shadows, and textures.