```javascript
// Importing necessary libraries
import React, { useState } from "react";
import ReactDOM from "react-dom";
import "./styles.css";

// Creating the main functional component
const App = () => {
  // Using React's useState hook to manage state
  const [count, setCount] = useState(0);
  const [toggle, setToggle] = useState(false);

  // Defining a function to handle button click
  const handleClick = () => {
    // Incrementing the count and updating the state
    setCount(count + 1);
    // Toggling the value of toggle and updating the state
    setToggle(!toggle);
  };

  // Rendering the React component
  return (
    <div className="App">
      {/* Displaying the count value */}
      <h1>Count: {count}</h1>
      {/* Displaying a button */}
      <button onClick={handleClick}>
        {/* Displaying the button text based on the value of toggle */}
        {toggle ? "Click to Increment" : "Click to Reset"}
      </button>
    </div>
  );
};

// Rendering the App component to the root element
ReactDOM.render(
  // Wrapping the App component with React.StrictMode for improved development experience
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById("root")
);
```

**Explanation:**

- We start by importing the necessary libraries, including React and ReactDOM.

- The main functional component, `App`, is defined using ES6 arrow function syntax.

- Inside the `App` component, we use React's `useState` hook to manage two state variables: `count` and `toggle`. The `count` state variable represents the count value, and the `toggle` state variable is used to toggle the button text between "Click to Increment" and "Click to Reset."

- The `handleClick` function is defined to handle the button click event. This function increments the `count` state variable and toggles the `toggle` state variable.

- The `App` component renders the current count value and a button. The button's text is determined by the `toggle` state variable.

- The `handleClick` function is attached to the onClick event handler of the button.

- The App component is rendered to the root element of the HTML document.