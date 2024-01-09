```javascript
// Import necessary libraries
import React, { useState, useEffect } from "react";
import { BrowserRouter as Router, Switch, Route, Link } from "react-router-dom";
import { useForm } from "react-hook-form";
import axios from "axios";
import { ToastContainer, toast } from "react-toastify";
import "react-toastify/dist/ReactToastify.css";

// Define the main App component
const App = () => {
  // Initialize state variables
  const [products, setProducts] = useState([]);
  const [currentPage, setCurrentPage] = useState(1);
  const [postsPerPage] = useState(5);
  const [searchTerm, setSearchTerm] = useState("");
  const [isSearching, setIsSearching] = useState(false);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState(null);
  const [formData, setFormData] = useState({
    name: "",
    description: "",
    price: "",
  });

  // Use the useForm hook to handle form validation
  const {
    register,
    handleSubmit,
    formState: { errors },
  } = useForm();

  // Fetch products from the API on initial render
  useEffect(() => {
    const fetchProducts = async () => {
      try {
        setIsLoading(true);
        const response = await axios.get("https://fakestoreapi.com/products");
        setProducts(response.data);
        setIsLoading(false);
      } catch (error) {
        setError(error.message);
        setIsLoading(false);
      }
    };

    fetchProducts();
  }, []);

  // Function to handle search input
  const handleSearch = (e) => {
    setSearchTerm(e.target.value);
    setIsSearching(true);
  };

  // Function to handle page change
  const handlePageChange = (pageNumber) => {
    setCurrentPage(pageNumber);
  };

  // Function to handle form submission
  const onSubmit = async (data) => {
    try {
      setIsLoading(true);
      await axios.post("https://fakestoreapi.com/products", data);
      toast.success("Product added successfully");
      setProducts([...products, data]);
      setIsLoading(false);
    } catch (error) {
      setError(error.message);
      setIsLoading(false);
    }
  };

  // Function to handle product deletion
  const handleDelete = async (id) => {
    try {
      setIsLoading(true);
      await axios.delete(`https://fakestoreapi.com/products/${id}`);
      const updatedProducts = products.filter((product) => product.id !== id);
      setProducts(updatedProducts);
      toast.success("Product deleted successfully");
      setIsLoading(false);
    } catch (error) {
      setError(error.message);
      setIsLoading(false);
    }
  };

  // Function to handle product update
  const handleUpdate = async (id, data) => {
    try {
      setIsLoading(true);
      await axios.put(`https://fakestoreapi.com/products/${id}`, data);
      const updatedProducts = products.map((product) => {
        if (product.id === id) {
          return { ...product, ...data };
        }

        return product;
      });

      setProducts(updatedProducts);
      toast.success("Product updated successfully");
      setIsLoading(false);
    } catch (error) {
      setError(error.message);
      setIsLoading(false);
    }
  };

  // Calculate the number of pages
  const pages = Math.ceil(products.length / postsPerPage);

  // Get the current page of products
  const indexOfLastPost = currentPage * postsPerPage;
  const indexOfFirstPost = indexOfLastPost - postsPerPage;
  const currentProducts = products.slice(indexOfFirstPost, indexOfLastPost);

  // Render the application
  return (
    <Router>
      <div className="container">
        <ToastContainer />
        <Switch>
          <Route exact path="/">
            <h1>Product Manager</h1>
            <form onSubmit={handleSubmit(onSubmit)}>
              <div className="form-group">
                <label htmlFor="name">Name</label>
                <input
                  type="text"
                  name="name"
                  id="name"
                  {...register("name", { required: true, maxLength: 20 })}
                />
                {errors.name && <span className="error">Name is required</span>}
              </div>
              <div className="form-group">
                <label htmlFor="description">Description</label>
                <textarea
                  name="description"
                  id="description"
                  {...register("description", { required: true })}
                />
                {errors.description && (
                  <span className="error">Description is required</span>
                )}
              </div>
              <div className="form-group">
                <label htmlFor="price">Price</label>
                <input
                  type="number"
