```css
/* CSS Grid Layout - Complex and Differentiated Code */

/* 1. Grid Container */

.grid-container {
  display: grid;
  grid-template-columns: repeat(12, 1fr);  /* 12 columns with equal width */
  grid-gap: 1em;                            /* 1em spacing between columns and rows */
  height: 100vh;                           /* Full viewport height */
  margin: 0 auto;                          /* Center the container horizontally */
}

/* 2. Grid Items - Different Sizes and Shapes */

.grid-item-1 {
  grid-column: span 6;                    /* Spans 6 columns */
  grid-row: span 3;                      /* Spans 3 rows */
  background-color: #f00;                  /* Red background */
}

.grid-item-2 {
  grid-column: span 4;                    /* Spans 4 columns */
  grid-row: span 2;                      /* Spans 2 rows */
  background-color: #0f0;                  /* Green background */
}

.grid-item-3 {
  grid-column: span 2;                    /* Spans 2 columns */
  grid-row: span 1;                      /* Spans 1 row */
  background-color: #00f;                  /* Blue background */
}

.grid-item-4 {
  grid-column: span 3;                    /* Spans 3 columns */
  grid-row: span 1;                      /* Spans 1 row */
  background-color: #ff0;                  /* Yellow background */
}

.grid-item-5 {
  grid-column: span 1;                    /* Spans 1 column */
  grid-row: span 2;                      /* Spans 2 rows */
  background-color: #f0f;                  /* Cyan background */
}

/* 3. Grid Item Alignment and Positioning */

.grid-item-1 {
  justify-self: center;                   /* Center horizontally within its column */
  align-self: center;                    /* Center vertically within its row */
}

.grid-item-2 {
  justify-self: end;                      /* Align to the right edge of its column */
  align-self: end;                        /* Align to the bottom edge of its row */
}

.grid-item-3 {
  justify-self: start;                    /* Align to the left edge of its column */
  align-self: start;                      /* Align to the top edge of its row */
}

.grid-item-4 {
  justify-self: center;                   /* Center horizontally within its column */
  align-self: start;                      /* Align to the top edge of its row */
}

.grid-item-5 {
  justify-self: end;                      /* Align to the right edge of its column */
  align-self: center;                    /* Center vertically within its row */
}

/* 4. Grid Item Styling */

.grid-item {
  padding: 1em;                             /* 1em padding on all sides */
  border: 1px solid #000;                  /* 1px solid black border */
  border-radius: 0.5em;                   /* Rounded corners with 0.5em radius */
  text-align: center;                      /* Center the text horizontally */
  font-size: 1.5rem;                      /* 1.5rem font size */
  color: #fff;                             /* White text color */
}

/* 5. Media Queries - Responsive Layout */

@media (max-width: 1024px) {
  .grid-container {
    grid-template-columns: repeat(6, 1fr);  /* 6 columns for smaller screens */
  }
}

@media (max-width: 768px) {
  .grid-container {
    grid-template-columns: repeat(4, 1fr);  /* 4 columns for even smaller screens */
  }
}

@media (max-width: 576px) {
  .grid-container {
    grid-template-columns: repeat(2, 1fr);  /* 2 columns for the smallest screens */
  }
}
```

**Explanation:**

1. **Grid Container**: The `.grid-container` establishes the overall grid layout for the page. It defines the number of columns, the spacing between items, and the height of the container.

2. **Grid Items - Different Sizes and Shapes**: The `.grid-item-*` classes create the individual grid items. Each item has its own grid-column and grid-row properties, which define its size and position within the grid.

3. **Grid Item Alignment and Positioning**: The justify-self and align-self properties are used to align and position the grid items within their cells.

4. **Grid Item Styling**: The `.grid-item` class applies common styling to all grid items, including padding, border, border-radius, text alignment, font size, and color.

5. **Media Queries - Responsive Layout**: The media queries are used to adjust the grid layout for different screen sizes, ensuring a responsive design.

This code creates a complex and differentiated grid layout with items of different sizes, shapes, and positions. It also demonstrates the use of media queries for responsive design.