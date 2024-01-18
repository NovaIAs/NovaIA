```javascript
// Import necessary modules.
import React from "react";
import ReactDOM from "react-dom";
import { BrowserRouter as Router, Route, Switch } from "react-router-dom";

// Define the main component.
const App = () => {
  return (
    <Router>
      <Switch>
        <Route exact path="/" component={Home} />
        <Route path="/about" component={About} />
        <Route path="/contact" component={Contact} />
      </Switch>
    </Router>
  );
};

// Define the home component.
const Home = () => {
  return (
    <div>
      <h1>Welcome to the Home Page</h1>
      <p>This is the home page of the website.</p>
    </div>
  );
};

// Define the about component.
const About = () => {
  return (
    <div>
      <h1>Welcome to the About Page</h1>
      <p>This is the about page of the website.</p>
    </div>
  );
};

// Define the contact component.
const Contact = () => {
  return (
    <div>
      <h1>Welcome to the Contact Page</h1>
      <p>This is the contact page of the website.</p>
    </div>
  );
};

// Render the main component to the DOM.
ReactDOM.render(<App />, document.getElementById("root"));
```

**Explanation:**

This code uses the React framework to create a simple single-page application (SPA) with three pages: a home page, an about page, and a contact page.

Here's a breakdown of the code:

1. **Import Statements:**

   - `import React from "react";`: This line imports the React library.
   - `import ReactDOM from "react-dom";`: This line imports the ReactDOM library.
   - `import { BrowserRouter as Router, Route, Switch } from "react-router-dom";`: This line imports the necessary components from the `react-router-dom` library for routing.

2. **App Component:**

   - The `App` component is the main component of the application.
   - It uses the `Router` component to define routes for the application.
   - It contains three `Route` components, each mapping a path to a different component (`Home`, `About`, and `Contact`).

3. **Home, About, and Contact Components:**

   - These are simple components that define the content for each page of the application.

4. **Rendering the App Component:**

   - The last line of the code, `ReactDOM.render(<App />, document.getElementById("root"));`, renders the `App` component to the DOM. The `document.getElementById("root")` part selects the HTML element with the ID "root" and renders the `App` component inside it.

Overall, this code is a basic example of a React application with multiple pages using React Router for routing.