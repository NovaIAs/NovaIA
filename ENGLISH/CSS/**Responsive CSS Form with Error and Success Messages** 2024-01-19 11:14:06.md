```css
/* Style the parent container */
.container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
  height: 100vh;
  background-color: #f5f5f5;
}

/* Style the title */
.title {
  font-size: 3rem;
  font-weight: bold;
  margin-bottom: 1rem;
}

/* Style the subtitle */
.subtitle {
  font-size: 1.5rem;
  font-weight: normal;
  margin-bottom: 2rem;
}

/* Style the input field */
.input-field {
  width: 200px;
  height: 50px;
  padding: 10px;
  border: 1px solid #ccc;
  border-radius: 5px;
  margin-bottom: 1rem;
}

/* Style the button */
.button {
  width: 150px;
  height: 50px;
  padding: 10px;
  background-color: #3498db;
  color: #fff;
  border: none;
  border-radius: 5px;
  cursor: pointer;
}

/* Style the error message */
.error-message {
  color: red;
  font-size: 1.2rem;
  margin-bottom: 1rem;
}

/* Style the success message */
.success-message {
  color: green;
  font-size: 1.2rem;
  margin-bottom: 1rem;
}

/* Media query for small screens */
@media (max-width: 768px) {
  .container {
    flex-direction: column;
    align-items: center;
    justify-content: center;
  }

  .title {
    font-size: 2rem;
  }

  .subtitle {
    font-size: 1rem;
  }

  .input-field {
    width: 150px;
    height: 40px;
  }

  .button {
    width: 100px;
    height: 40px;
  }
}

/* Explaining the code */

/* The .container class is the parent container that holds all the other elements. It is styled to be a flexbox container with a column direction and centered items. */

/* The .title and .subtitle classes style the title and subtitle text, respectively. */

/* The .input-field class styles the input field. It has a fixed width and height, a border, and rounded corners. */

/* The .button class styles the button. It has a fixed width and height, a background color, a white color, no border, and rounded corners. */

/* The .error-message and .success-message classes style the error and success messages, respectively. They are colored red and green, respectively, and have a larger font size. */

/* The @media (max-width: 768px) media query targets small screens (devices with a maximum width of 768 pixels). It adjusts the styles of the container, title, subtitle, input field, and button to make them more suitable for smaller screens. */
```