```css
/*
  -------------------------------------------------------------------------------------------------------------------------------------------------
  |                                                                                                                               |
  |    ______              _   _             _       _____    ____  _____        _   _                _               _                 |
  |   /  __  \            | | | |           | |     /  __  \  / __ \|  __ \      | | | |              | |             | |                |
  |  |  |  | | __ _  ___ | |_| |  ___  ___| | __ | /  \/  \/ / / _` | |__) | ___ | |_| | ___ _ __   | |_ ___  _ __ | |_ ___ _ __ ___  |
  |  |  |  | |/ _` |/ _ \| __| | / __|/ _ \ |/ / | |     / / / / /| |  _  |/ _ \| __| |/ _ \ '__|  | __/ _ \| '__|| __/ _ \ '__/ __| |
  |  |  `--' / (_| | (_) | |_| | \__ \  __/   <  | |    / / / /_| | | | | (_) | |_| |  __/ |     | || (_) | |   | ||  __/ |  \__ \ |
  |   \______/ \__,_|\___/ \__|_| |___/\___|_|\_\ |_/    /_/ /____/|_| |_|\___/ \__|\__\___|_|     \__\___/|_|    \__\___|_|  |___/ |
  |                                                                                                                               |
  -------------------------------------------------------------------------------------------------------------------------------------------------
*/

/*
  1. Reset Styles
*/

*,
*::before,
*::after {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/*
  2. Global Styles
*/

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: #333;
  background-color: #fff;
}

a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
  text-decoration: underline;
}

/*
  3. Header Styles
*/

header {
  background-color: #f8f9fa;
  padding: 20px 0;
}

header h1 {
  font-size: 24px;
  font-weight: bold;
  color: #333;
  margin-bottom: 10px;
}

header nav {
  display: flex;
  justify-content: space-between;
}

header nav a {
  margin-right: 10px;
}

/*
  4. Main Content Styles
*/

main {
  margin-top: 20px;
}

main h2 {
  font-size: 20px;
  font-weight: bold;
  color: #333;
  margin-bottom: 10px;
}

main p {
  margin-bottom: 10px;
}

/*
  5. Footer Styles
*/

footer {
  background-color: #f8f9fa;
  padding: 20px 0;
}

footer p {
  margin-bottom: 0;
}

/*
  6. Media Queries
*/

@media (max-width: 768px) {
  header nav {
    flex-direction: column;
    align-items: center;
  }

  header nav a {
    margin-right: 0;
    margin-bottom: 10px;
  }
}
```

This code is a complex and differentiated CSS stylesheet that includes a reset, global styles, header styles, main content styles, footer styles, and media queries. The code is well-commented and organized, making it easy to read and understand.

The reset styles are used to normalize the browser's default styles, ensuring that all elements start with the same base styles. The global styles include common styles that are applied to all elements on the page, such as the font family, font size, line height, and color.

The header styles include the styles for the header section of the page, including the header's background color, padding, and the styles for the header's h1 element and nav element. The main content styles include the styles for the main content section of the page, including the styles for the main content's h2 element and p element.

The footer styles include the styles for the footer section of the page, including the footer's background color, padding, and the styles for the footer's p element. The media queries are used to apply different styles to the page depending on the width of the browser window.

Overall, this code is a well-crafted and comprehensive CSS stylesheet that provides a solid foundation for creating a visually appealing and responsive website.