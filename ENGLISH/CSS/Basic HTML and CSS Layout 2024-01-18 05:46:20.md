```css
/*
 * Base rules
 */

html, body {
  height: 100%;
}

body {
  font-family: Arial, sans-serif;
  margin: 0;
  padding: 0;
}

/*
 * Header
 */

header {
  background-color: #f1f1f1;
  padding: 20px;
}

header h1 {
  font-size: 24px;
  font-weight: bold;
}

/*
 * Main content
 */

main {
  display: flex;
  flex-direction: row;
  height: calc(100% - 80px);
}

/*
 * Sidebar
 */

aside {
  background-color: #f4f4f4;
  flex-basis: 200px;
  padding: 20px;
}

aside h2 {
  font-size: 18px;
  font-weight: bold;
}

aside ul {
  list-style-type: none;
  padding: 0;
}

aside li {
  margin-bottom: 10px;
}

/*
 * Content area
 */

section {
  flex: 1;
  padding: 20px;
}

section h2 {
  font-size: 24px;
  font-weight: bold;
}

section p {
  font-size: 16px;
}

/*
 * Footer
 */

footer {
  background-color: #f1f1f1;
  padding: 20px;
  text-align: center;
}

footer p {
  font-size: 12px;
}

/*
 * Media queries
 */

@media (max-width: 768px) {
  main {
    flex-direction: column;
  }

  aside {
    flex-basis: auto;
  }

  section {
    margin-top: 20px;
  }
}
```

This code creates a basic web page layout with a header, sidebar, content area, and footer. The layout is responsive, meaning that it will adjust to different screen sizes.

The HTML structure of the page is as follows:

```html
<!DOCTYPE html>
<html>
<head>
  <title>My Web Page</title>
  <link rel="stylesheet" href="style.css">
</head>
<body>
  <header>
    <h1>My Web Page</h1>
  </header>
  <main>
    <aside>
      <h2>Sidebar</h2>
      <ul>
        <li>Item 1</li>
        <li>Item 2</li>
        <li>Item 3</li>
      </ul>
    </aside>
    <section>
      <h2>Content Area</h2>
      <p>This is the content area of the page.</p>
    </section>
  </main>
  <footer>
    <p>Copyright 2023</p>
  </footer>
</body>
</html>
```

The CSS code is as follows:

```css
/*
 * Base rules
 */

html, body {
  height: 100%;
}

body {
  font-family: Arial, sans-serif;
  margin: 0;
  padding: 0;
}

/*
 * Header
 */

header {
  background-color: #f1f1f1;
  padding: 20px;
}

header h1 {
  font-size: 24px;
  font-weight: bold;
}

/*
 * Main content
 */

main {
  display: flex;
  flex-direction: row;
  height: calc(100% - 80px);
}

/*
 * Sidebar
 */

aside {
  background-color: #f4f4f4;
  flex-basis: 200px;
  padding: 20px;
}

aside h2 {
  font-size: 18px;
  font-weight: bold;
}

aside ul {
  list-style-type: none;
  padding: 0;
}

aside li {
  margin-bottom: 10px;
}

/*
 * Content area
 */

section {
  flex: 1;
  padding: 20px;
}

section h2 {
  font-size: 24px;
  font-weight: bold;
}

section p {
  font-size: 16px;
}

/*
 * Footer
 */

footer {
  background-color: #f1f1f1;
  padding: 20px;
  text-align: center;
}

footer p {
  font-size: 12px;
}

/*
 * Media queries
 */

@media (max-width: 768px) {
  main {
    flex-direction: column;
  }

  aside {
    flex-basis: auto;
  }

  section {
    margin-top: 20px;
  }
}
```

The base rules section sets the height of the HTML and body elements to 100%, and sets the font-family for the body element.

The header section styles the header element, which contains the page title.

The main content section styles the main element, which contains the sidebar and content area. The sidebar and content area are displayed as flexbox items, with the sidebar on the left and the content area on the right.

The sidebar section styles the sidebar element, which contains a list of links.

The content area section styles the section element, which contains the main content of the page.

The footer section styles the footer element, which contains the copyright notice.

The media queries section contains a media query that changes the layout of the page when the viewport width is less than 768px. When the viewport width is less than 768px, the sidebar and content area are stacked on top of each other, instead of being displayed side by side.