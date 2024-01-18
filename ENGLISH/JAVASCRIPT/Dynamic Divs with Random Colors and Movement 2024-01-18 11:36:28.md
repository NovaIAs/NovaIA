```javascript
// Define a function to generate a random number between two values
function getRandomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Define a function to generate a random color
function getRandomColor() {
  const colors = ['red', 'orange', 'yellow', 'green', 'blue', 'indigo', 'violet'];
  return colors[getRandomNumber(0, colors.length - 1)];
}

// Define a function to create a new div element
function createDivElement() {
  const div = document.createElement('div');
  div.style.width = getRandomNumber(100, 200) + 'px';
  div.style.height = getRandomNumber(100, 200) + 'px';
  div.style.backgroundColor = getRandomColor();
  div.style.position = 'absolute';
  div.style.top = getRandomNumber(0, window.innerHeight - 200) + 'px';
  div.style.left = getRandomNumber(0, window.innerWidth - 200) + 'px';

  return div;
}

// Create a function to add a div element to the DOM
function addDivElement() {
  const div = createDivElement();
  document.body.appendChild(div);
}

// Create a function to remove a div element from the DOM
function removeDivElement(div) {
  document.body.removeChild(div);
}

// Create a function to move a div element
function moveDivElement(div) {
  div.style.top = getRandomNumber(0, window.innerHeight - 200) + 'px';
  div.style.left = getRandomNumber(0, window.innerWidth - 200) + 'px';
}

// Create a function to change the color of a div element
function changeDivColor(div) {
  div.style.backgroundColor = getRandomColor();
}

// Create an array to store the div elements
const divs = [];

// Add 10 div elements to the DOM
for (let i = 0; i < 10; i++) {
  addDivElement();
}

// Get the div elements from the DOM
const divs = document.querySelectorAll('div');

// Add event listeners to the div elements
for (let i = 0; i < divs.length; i++) {
  divs[i].addEventListener('click', function() {
    moveDivElement(this);
    changeDivColor(this);
  });
}

// Add a button to the DOM
const button = document.createElement('button');
button.innerHTML = 'Remove All Divs';
document.body.appendChild(button);

// Add an event listener to the button
button.addEventListener('click', function() {
  for (let i = 0; i < divs.length; i++) {
    removeDivElement(divs[i]);
  }
});
```

This code creates 10 div elements and adds them to the DOM. Each div element has a random width, height, background color, and position. The code also adds event listeners to the div elements so that when they are clicked, they move to a new random position and change their background color. There is also a button that, when clicked, removes all of the div elements from the DOM.

The code is complex because it uses a number of different functions and event listeners. However, the code is also well-organized and easy to understand. The functions are all named descriptively and the code is well-commented.

Here is a breakdown of the code:

* The `getRandomNumber()` function generates a random number between two values.
* The `getRandomColor()` function generates a random color.
* The `createDivElement()` function creates a new div element with a random width, height, background color, and position.
* The `addDivElement()` function adds a div element to the DOM.
* The `removeDivElement()` function removes a div element from the DOM.
* The `moveDivElement()` function moves a div element to a new random position.
* The `changeDivColor()` function changes the background color of a div element.
* The `divs` array stores the div elements.
* The `for` loop adds 10 div elements to the DOM.
* The `querySelectorAll()` method gets the div elements from the DOM.
* The `addEventListener()` method adds event listeners to the div elements.
* The `button` variable creates a button element.
* The `innerHTML` property sets the HTML content of the button element.
* The `appendChild()` method adds the button element to the DOM.
* The `addEventListener()` method adds an event listener to the button element.
* The `for` loop removes all of the div elements from the DOM.