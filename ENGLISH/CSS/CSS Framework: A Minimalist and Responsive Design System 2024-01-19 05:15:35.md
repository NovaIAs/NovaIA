```css
/* Global Styles */

body {
  font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
  font-size: 16px;
  line-height: 1.6;
  color: #333;
  background-color: #fff;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

p {
  margin-bottom: 1em;
}

a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
  text-decoration: underline;
}

/* Layout */

.container {
  max-width: 1000px;
  padding: 0 15px;
  margin: 0 auto;
}

.row {
  display: flex;
  flex-wrap: wrap;
  margin-right: -15px;
  margin-left: -15px;
}

.col {
  flex: 1 0 0%;
  padding: 0 15px;
}

.col-2 {
  flex: 0 0 16.666667%;
  max-width: 16.666667%;
}

.col-3 {
  flex: 0 0 25%;
  max-width: 25%;
}

.col-4 {
  flex: 0 0 33.333333%;
  max-width: 33.333333%;
}

.col-5 {
  flex: 0 0 41.666667%;
  max-width: 41.666667%;
}

.col-6 {
  flex: 0 0 50%;
  max-width: 50%;
}

.col-7 {
  flex: 0 0 58.333333%;
  max-width: 58.333333%;
}

.col-8 {
  flex: 0 0 66.666667%;
  max-width: 66.666667%;
}

.col-9 {
  flex: 0 0 75%;
  max-width: 75%;
}

.col-10 {
  flex: 0 0 83.333333%;
  max-width: 83.333333%;
}

.col-11 {
  flex: 0 0 91.666667%;
  max-width: 91.666667%;
}

.col-12 {
  flex: 0 0 100%;
  max-width: 100%;
}

/* Typography */

h1 {
  font-size: 2.25rem;
}

h2 {
  font-size: 1.875rem;
}

h3 {
  font-size: 1.5rem;
}

h4 {
  font-size: 1.25rem;
}

h5 {
  font-size: 1.125rem;
}

h6 {
  font-size: 1rem;
}

.lead {
  font-size: 1.25rem;
  font-weight: 300;
}

.small {
  font-size: 0.875rem;
}

/* Buttons */

.btn {
  display: inline-block;
  font-size: 0.875rem;
  font-weight: 400;
  line-height: 1.5;
  padding: 0.375rem 0.75rem;
  border-radius: 0.25rem;
  text-align: center;
  white-space: nowrap;
  vertical-align: middle;
  cursor: pointer;
}

.btn-primary {
  color: #fff;
  background-color: #007bff;
  border-color: #007bff;
}

.btn-primary:hover {
  color: #fff;
  background-color: #0069d9;
  border-color: #0062cc;
}

.btn-secondary {
  color: #fff;
  background-color: #6c757d;
  border-color: #6c757d;
}

.btn-secondary:hover {
  color: #fff;
  background-color: #5a6268;
  border-color: #545b62;
}

/* Forms */

.form-control {
  display: block;
  width: 100%;
  height: calc(1.5em + 0.75rem + 2px);
  padding: 0.375rem 0.75rem;
  font-size: 1rem;
  line-height: 1.5;
  color: #495057;
  background-color: #fff;
  background-clip: padding-box;
  border: 1px solid #ced4da;
  border-radius: 0.25rem;
}

.form-control:focus {
  color: #495057;
  background-color: #fff;
  border-color: #80bdff;
  outline: 0;
}

.form-group {
  margin-bottom: 1rem;
}

.form-check {
  position: relative;
  display: block;
  padding-left: 1.25rem;
}

.form-check-input {
  position: absolute;
  margin-top: 0.3rem;
  margin-left: -1.25rem;
}

.form-check-label {
  margin-bottom: 0;
}

/* Tables */

table {
  width: 100%;
  margin-bottom: 1rem;
  color: #212529;
}

th, td {
  padding: 0.75rem;
  vertical-align: top;
  border-top: 1px solid #dee2e6;
}

th {
  font-weight: bold;
}

tbody {
  border-top: 2px solid #dee2e6;
}

/* Images */

img {
  max-width: 100%;
  height: auto;
}

/* Media Queries */

@media (min-width: 576px) {
  .col-sm-1, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9, .col-sm-10, .col-sm-11, .col-sm-12 {
    flex: 1 0 0%;
    max-width: 0%;
  }

  .col-sm-1 {
    flex: 0 0 8.333333%;
    max-width: 8.333333%;
  }

  .col-sm-2 {
    flex: 0 0 16.666667%;
    max-width: 16.666667%;
  }

  .col-sm-3 {
    flex: 0 0 25%;
    max-width: 25%;
  }

  .col-sm-4 {
    flex: 0 0 33.333333%;
    max-width: 33.333333%;
  }

  .col-sm-5 {
    flex: 0 0 41.666667%;
    max-width: 41.666667%;
  }

  .col-sm-6 {
    flex: 0 0 50%;
    max-width: 50%;
  }

  .col-sm-7 {
    flex: 0 0 58.333333%;
    max-width: 58.333333%;
  }

  .col-sm-8 {
    flex: 0 0 66.666667%;
    max-width: 66.666667%;
  }

  .col-sm-9 {
    flex: 0 0 75%;
    max-width: 75%;
  }

  .col-sm-10 {
    flex: 0 0 83.333333%;
    max-width: 83.333333%;
  }

  .col-sm-11 {
    flex: 0 0 91.666667%;
    max-width: 91.666667%;
  }

  .col-sm-12 {
    flex: 0 0 100%;
    max-width: 100%;
  }
}

@media (min-width: 768px) {
  .col-md-1, .col-md-2, .col-md-3, .col-md-4, .col-md-5, .col-md-6, .col-md-7, .col-md-8, .col-md-9, .col-md-10, .col-md-11, .col-md-12 {
    flex: 1 0 0%;
    max-width: 0%;
  }

  .col-md-1 {
    flex: 0 0 8.333333%;
    max-width: 8.333333%;
  }

  .col-md-2 {
    flex: 0 0 16.666667%;
    max-width: 16.666667%;
  }

  .col-md-3 {
    flex: 0 0 25%;
    max-width: 25%;
  }

  .col-md-4 {
    flex: 0 0 33.333333%;
    max-width: 33.333