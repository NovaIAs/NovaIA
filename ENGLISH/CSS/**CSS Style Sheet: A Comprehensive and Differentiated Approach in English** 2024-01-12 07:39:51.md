```css
/* A complex and differentiated CSS code in English */

/* 1. Variable Declarations */

:root {
  --primary-color: #FF69B4;
  --secondary-color: #FFD600;
  --text-color: #323232;
  --font-family: 'Helvetica', 'Arial', sans-serif;
}

/* 2. Global Styles */

body {
  font-family: var(--font-family);
  font-size: 16px;
  color: var(--text-color);
  background-color: #F5F5F5;
}

/* 3. Header Styles */

header {
  background-color: var(--primary-color);
  color: #FFFFFF;
  padding: 20px;
}

header h1 {
  font-size: 36px;
  font-weight: bold;
  margin-bottom: 10px;
}

header nav {
  float: right;
}

header nav ul {
  list-style-type: none;
  display: flex;
}

header nav li {
  margin-right: 20px;
}

header nav a {
  text-decoration: none;
  color: #FFFFFF;
  font-size: 18px;
}

/* 4. Main Content Styles */

main {
  padding: 20px;
}

main h2 {
  font-size: 24px;
  font-weight: bold;
  margin-bottom: 10px;
}

main p {
  margin-bottom: 10px;
}

/* 5. Sidebar Styles */

aside {
  float: right;
  width: 20%;
  background-color: #F0F0F0;
  padding: 20px;
}

aside h3 {
  font-size: 18px;
  font-weight: bold;
  margin-bottom: 10px;
}

aside ul {
  list-style-type: none;
}

aside li {
  margin-bottom: 10px;
}

/* 6. Footer Styles */

footer {
  background-color: var(--secondary-color);
  color: #FFFFFF;
  padding: 20px;
}

footer p {
  margin-bottom: 10px;
}

/* 7. Media Queries */

@media (max-width: 768px) {
  aside {
    width: 100%;
    float: none;
  }
}

/* 8. Hover States */

header nav a:hover {
  color: #FF4500;
}

footer a:hover {
  color: #FFE633;
}

/* 9. Additional Styles */

.button {
  background-color: var(--primary-color);
  color: #FFFFFF;
  padding: 10px 20px;
  border: none;
  border-radius: 5px;
  cursor: pointer;
}

.button:hover {
  background-color: #FF4500;
}

.highlight {
  background-color: #FFFF00;
  padding: 5px;
}
```

**Explanation:**

This CSS code demonstrates a complex and differentiated style sheet in English. It includes variable declarations, global styles, header, main content, sidebar, footer, media queries, hover states, and additional styles.

1. **Variable Declarations:**
   - Custom CSS variables are defined, such as colors and fonts, to provide a consistent and maintainable style sheet.

2. **Global Styles:**
   - The global styles apply to the entire document, defining the base font family, font size, color, and background color.

3. **Header Styles:**
   - The header has a background color, text color, padding, and includes an h1 and a navigation bar.

4. **Main Content Styles:**
   - The main content area has padding, h2 headings, paragraphs, and a sidebar.

5. **Sidebar Styles:**
   - The sidebar floats to the right, has a background color, padding, h3 headings, and an unordered list.

6. **Footer Styles:**
   - The footer has a background color, text color, padding, and paragraphs.

7. **Media Queries:**
   - A media query is used to make the sidebar full-width on screens smaller than 768px.

8. **Hover States:**
   - Hover states are applied to links in the header and footer to change their color on hover.

9. **Additional Styles:**
   - Additional styles include a button class and a highlight class for specific elements.

This code is comprehensive, organized, and illustrates the various features and capabilities of CSS. It is a good example of a complex and differentiated style sheet that demonstrates the power of CSS.