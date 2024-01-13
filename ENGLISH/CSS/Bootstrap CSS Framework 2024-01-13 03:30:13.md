```css
/* Variables */

:root {
  --primary-color: #007bff;
  --secondary-color: #6c757d;
  --accent-color: #ffc107;
  --font-family: 'Helvetica', sans-serif;

  --grid-column-count: 12;
  --grid-gutter-width: 1rem;

  --breakpoint-sm: 576px;
  --breakpoint-md: 768px;
  --breakpoint-lg: 992px;
  --breakpoint-xl: 1200px;
}

/* Typography */

html {
  font-size: 62.5%;
}

body {
  font-family: var(--font-family);
  font-size: 1.6rem;
  line-height: 1.5;
  color: #333;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
  margin-bottom: 1rem;
}

h1 {
  font-size: 2.5rem;
}

h2 {
  font-size: 2rem;
}

h3 {
  font-size: 1.75rem;
}

h4 {
  font-size: 1.5rem;
}

h5 {
  font-size: 1.25rem;
}

h6 {
  font-size: 1rem;
}

p {
  margin-bottom: 1rem;
}

a {
  color: var(--primary-color);
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

/* Layout */

.container {
  max-width: 1140px;
  padding: 0 1rem;
  margin: 0 auto;
}

.row {
  display: flex;
  flex-wrap: wrap;
  margin-right: -var(--grid-gutter-width);
  margin-left: -var(--grid-gutter-width);
}

.col {
  flex: 1 0 auto;
  padding-right: var(--grid-gutter-width);
  padding-left: var(--grid-gutter-width);
}

.col-sm-1 {
  width: 8.333333%;
}

.col-sm-2 {
  width: 16.666667%;
}

.col-sm-3 {
  width: 25%;
}

.col-sm-4 {
  width: 33.333333%;
}

.col-sm-5 {
  width: 41.666667%;
}

.col-sm-6 {
  width: 50%;
}

.col-sm-7 {
  width: 58.333333%;
}

.col-sm-8 {
  width: 66.666667%;
}

.col-sm-9 {
  width: 75%;
}

.col-sm-10 {
  width: 83.333333%;
}

.col-sm-11 {
  width: 91.666667%;
}

.col-sm-12 {
  width: 100%;
}

@media (min-width: var(--breakpoint-md)) {
  .col-md-1 {
    width: 8.333333%;
  }

  .col-md-2 {
    width: 16.666667%;
  }

  .col-md-3 {
    width: 25%;
  }

  .col-md-4 {
    width: 33.333333%;
  }

  .col-md-5 {
    width: 41.666667%;
  }

  .col-md-6 {
    width: 50%;
  }

  .col-md-7 {
    width: 58.333333%;
  }

  .col-md-8 {
    width: 66.666667%;
  }

  .col-md-9 {
    width: 75%;
  }

  .col-md-10 {
    width: 83.333333%;
  }

  .col-md-11 {
    width: 91.666667%;
  }

  .col-md-12 {
    width: 100%;
  }
}

@media (min-width: var(--breakpoint-lg)) {
  .col-lg-1 {
    width: 8.333333%;
  }

  .col-lg-2 {
    width: 16.666667%;
  }

  .col-lg-3 {
    width: 25%;
  }

  .col-lg-4 {
    width: 33.333333%;
  }

  .col-lg-5 {
    width: 41.666667%;
  }

  .col-lg-6 {
    width: 50%;
  }

  .col-lg-7 {
    width: 58.333333%;
  }

  .col-lg-8 {
    width: 66.666667%;
  }

  .col-lg-9 {
    width: 75%;
  }

  .col-lg-10 {
    width: 83.333333%;
  }

  .col-lg-11 {
    width: 91.666667%;
  }

  .col-lg-12 {
    width: 100%;
  }
}

@media (min-width: var(--breakpoint-xl)) {
  .col-xl-1 {
    width: 8.333333%;
  }

  .col-xl-2 {
    width: 16.666667%;
  }

  .col-xl-3 {
    width: 25%;
  }

  .col-xl-4 {
    width: 33.333333%;
  }

  .col-xl-5 {
    width: 41.666667%;
  }

  .col-xl-6 {
    width: 50%;
  }

  .col-xl-7 {
    width: 58.333333%;
  }

  .col-xl-8 {
    width: 66.666667%;
  }

  .col-xl-9 {
    width: 75%;
  }

  .col-xl-10 {
    width: 83.333333%;
  }

  .col-xl-11 {
    width: 91.666667%;
  }

  .col-xl-12 {
    width: 100%;
  }
}

/* Components */

.button {
  display: inline-block;
  padding: 0.5rem 1rem;
  border: 1px solid var(--primary-color);
  border-radius: 0.25rem;
  background-color: var(--primary-color);
  color: #fff;
  font-size: 1rem;
  text-align: center;
  text-decoration: none;
  cursor: pointer;
}

.button:hover {
  background-color: #0069d9;
}

.button:active {
  background-color: #0059b3;
}

.card {
  background-color: #fff;
  border: 1px solid #e3e3e3;
  border-radius: 0.25rem;
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);
  padding: 1rem;
}

.card-title {
  font-size: 1.5rem;
  font-weight: bold;
  margin-bottom: 0.5rem;
}

