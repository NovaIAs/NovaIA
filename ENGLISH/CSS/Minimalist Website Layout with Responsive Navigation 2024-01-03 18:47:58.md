```css
/* The universal selector (*) targets all elements on the page. We're setting the font family to "Arial, Helvetica, sans-serif", which is a commonly used stack for web fonts. */
* {
  font-family: "Arial", Helvetica, sans-serif;
}

/* The body selector targets the <body> element, which contains the main content of the webpage. We're setting the background color to "white" and the margin to "0", which ensures that the content starts at the very top of the page. */
body {
  background-color: white;
  margin: 0;
}

/* The header selector targets the <header> element, which usually contains the title and navigation links of the page. We're setting the background color to "black" and the text color to "white", creating a sleek and contrasting header. */
header {
  background-color: black;
  color: white;
}

/* The nav selector targets the <nav> element, which contains a list of links used for navigation. We're setting the list style type to "none", removing the default styling of bullet points, and setting the padding and margin to "0", for a clean and compact navigation bar. */
nav {
  list-style-type: none;
  padding: 0;
  margin: 0;
}

/* The nav li selector targets the <li> elements within the navigation bar. We're setting the display to "inline-block", allowing the list items to sit horizontally next to each other, and we're adding a margin-right to create some spacing between them. */
nav li {
  display: inline-block;
  margin-right: 10px;
}

/* The nav li a selector targets the <a> elements within the navigation bar, which are the actual links. We're setting the text decoration to "none", removing the default underline from links, and setting the color to "white", matching the header text color. */
nav li a {
  text-decoration: none;
  color: white;
}

/* The main selector targets the <main> element, which contains the main content of the page. We're setting the padding to "20px", creating some space between the content and the edges of the page. */
main {
  padding: 20px;
}

/* The article selector targets the <article> elements within the main content area. Articles are typically used for blog posts or other self-contained pieces of content. We're setting the background color to "white" and the padding to "20px", creating a clean and spacious container for the content. */
article {
  background-color: white;
  padding: 20px;
}

/* The h1 selector targets the <h1> elements within the article content. These are typically used for the title of the article. We're setting the font size to "24px" and the color to "black". */
article h1 {
  font-size: 24px;
  color: black;
}

/* The p selector targets the <p> elements within the article content. These are typically used for the body text of the article. We're setting the font size to "16px", the color to "black", and the line-height to "1.5", making the text easy to read. */
article p {
  font-size: 16px;
  color: black;
  line-height: 1.5;
}

/* The footer selector targets the <footer> element, which typically contains information about the copyright, contact details, or other legal information. We're setting the background color to "black" and the text color to "white". */
footer {
  background-color: black;
  color: white;
}

/* The footer p selector targets the <p> element within the footer. This is typically used for displaying the copyright information or other legal text. We're setting the font size to "12px" and the text align to "center". */
footer p {
  font-size: 12px;
  text-align: center;
}

/* Additional media queries for different screen sizes can be added here to adjust the layout and styling for different devices. */

@media (max-width: 768px) {
  /* This media query targets devices with a maximum width of 768px, which typically includes smartphones and small tablets. We're making some adjustments to the layout to make it more suitable for smaller screens. */

  /* Change the navigation bar to a hamburger menu icon. */
  nav {
    display: flex;
    flex-direction: column;
    align-items: center;
  }

  /* Hide the navigation bar links initially. */
  nav li {
    display: none;
  }

  /* Add a button to toggle the navigation bar. */
  nav button {
    display: block;
    background-color: black;
    color: white;
    padding: 10px;
    border: none;
    cursor: pointer;
  }

  /* When the button is clicked, toggle the display of the navigation bar links. */
  nav button:active + ul {
    display: flex;
    flex-direction: column;
    align-items: center;
  }
}

@media (min-width: 769px) {
  /* This media query targets devices with a minimum width of 769px, which typically includes tablets and desktop computers. We're making some adjustments to the layout to make it more suitable for larger screens. */

  /* Make the navigation bar horizontal again. */
  nav {
    display: flex;
    flex-direction: row;
    justify-content: space-between;
  }

  /* Show the navigation bar links initially. */
  nav li {
    display: block;
  }

  /* Hide the hamburger menu button. */
  nav button {
    display: none;
  }
}
```

This CSS code creates a basic website layout with a header, navigation bar, main content area, and footer. It uses a clean and modern design with black and white colors and clear typography. The code includes media queries to adjust the layout for different screen sizes, ensuring a good user experience across various devices.