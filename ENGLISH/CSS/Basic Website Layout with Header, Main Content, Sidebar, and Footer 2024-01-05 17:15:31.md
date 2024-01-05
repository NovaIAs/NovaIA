```css
/* Reset styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Body */

body {
  font-family: Arial, sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: #333;
  background-color: #fff;
}

/* Header */

header {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #f1f1f1;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

header h1 {
  float: left;
  margin: 0;
  padding: 0 20px;
  font-size: 24px;
  line-height: 60px;
  color: #333;
}

header nav {
  float: right;
  margin: 0;
  padding: 0;
}

header nav ul {
  list-style: none;
  display: flex;
}

header nav ul li {
  margin: 0 10px;
}

header nav ul li a {
  display: block;
  padding: 0 10px;
  line-height: 60px;
  color: #333;
  text-decoration: none;
}

header nav ul li a:hover {
  background-color: #f5f5f5;
}

/* Main */

main {
  margin-top: 60px;
  padding: 20px;
}

/* Article */

article {
  margin-bottom: 20px;
}

article h2 {
  margin: 0;
  padding: 0;
  font-size: 20px;
  line-height: 1.2;
  color: #333;
}

article p {
  margin-top: 10px;
  font-size: 16px;
  line-height: 1.5;
  color: #666;
}

/* Aside */

aside {
  float: right;
  width: 200px;
  margin-left: 20px;
}

aside h3 {
  margin: 0;
  padding: 0;
  font-size: 18px;
  line-height: 1.2;
  color: #333;
}

aside ul {
  list-style: none;
  margin: 0;
  padding: 0;
}

aside ul li {
  margin-bottom: 10px;
}

aside ul li a {
  display: block;
  padding: 0 10px;
  line-height: 20px;
  color: #666;
  text-decoration: none;
}

aside ul li a:hover {
  background-color: #f5f5f5;
}

/* Footer */

footer {
  position: fixed;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #f1f1f1;
  box-shadow: 0 -1px 3px rgba(0, 0, 0, 0.1);
}

footer p {
  margin: 0;
  padding: 0 20px;
  line-height: 60px;
  color: #666;
}
```

This code creates a basic website layout with a header, main content area, sidebar, and footer. The header contains a logo and navigation menu, the main content area contains articles and posts, the sidebar contains a list of links, and the footer contains a copyright notice. The code is written in CSS and uses the latest CSS3 techniques, such as flexbox and box-shadow.

Here is a more detailed explanation of the code:

* The `*` selector resets all the default styles of the browser. This is done to ensure that the website looks the same across all browsers.
* The `body` selector sets the font family, font size, line height, color, and background color of the website.
* The `header` selector creates the header of the website. It is positioned at the top of the page and has a height of 60px. The header contains a logo and navigation menu.
* The `header h1` selector sets the styles for the logo. The logo is positioned on the left side of the header and has a font size of 24px.
* The `header nav` selector creates the navigation menu. The navigation menu is positioned on the right side of the header and contains a list of links.
* The `header nav ul` selector sets the styles for the navigation menu. The navigation menu is displayed as a flexbox, which means that the links are arranged horizontally.
* The `header nav ul li` selector sets the styles for the individual links in the navigation menu. Each link has a margin of 0 10px and a padding of 0 10px.
* The `header nav ul li a` selector sets the styles for the text of the links in the navigation menu. The text of the links is black and has a text decoration of none.
* The `header nav ul li a:hover` selector sets the styles for the links when they are hovered over. The background color of the links changes to #f5f5f5 when they are hovered over.
* The `main` selector creates the main content area of the website. The main content area is located below the header and has a padding of 20px.
* The `article` selector creates the articles and posts in the main content area. Articles and posts have a margin-bottom of 20px.
* The `article h2` selector sets the styles for the headings of the articles and posts. The headings have a font size of 20px and a line height of 1.2.
* The `article p` selector sets the styles for the text of the articles and posts. The text has a font size of 16px and a line height of 1.5.
* The `aside` selector creates the sidebar of the website. The sidebar is positioned on the right side of the main content area and has a width of 200px.
* The `aside h3` selector sets the styles for the headings of the sidebar. The headings have a font size of 18px and a line height of 1.2.
* The `aside ul` selector creates the list of links in the sidebar. The list of links is displayed as a list, which means that the links are arranged vertically.
* The `aside ul li` selector sets the styles for the individual links in the sidebar. Each link has a margin-bottom of 10px.
* The `aside ul li a` selector sets the styles for the text of the links in the sidebar. The text of the links is black and has a text decoration of none.
* The `aside ul li a:hover` selector sets the styles for the links when they are hovered over. The background color of the links changes to #f5f5f5 when they are hovered over.
* The `footer` selector creates the footer of the website. The footer is positioned at the bottom of the page and has a height of 60px. The footer contains a copyright notice.
* The `footer p` selector sets the styles for the copyright notice. The copyright notice is positioned on the left side of the footer and has a line height of 60px.