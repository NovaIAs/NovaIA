```javascript
// Import necessary libraries.
import { useState, useEffect } from 'react';
import axios from 'axios';
import { LineChart, Line, CartesianGrid, XAxis, YAxis, Tooltip } from 'recharts';

// Define the main function.
const App = () => {
  // Declare state variables.
  const [data, setData] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');

  // Fetch data from the API.
  useEffect(() => {
    const fetchData = async () => {
      try {
        const response = await axios.get('https://api.example.com/data');
        setData(response.data);
        setLoading(false);
      } catch (error) {
        setError(error.message);
        setLoading(false);
      }
    };

    fetchData();
  }, []);

  // Render the application.
  if (loading) {
    return <div>Loading...</div>;
  }

  if (error) {
    return <div>Error: {error}</div>;
  }

  return (
    <div>
      <h1>Line Chart</h1>
      <LineChart width={600} height={300} data={data}>
        <Line type="monotone" dataKey="value" stroke="#8884d8" />
        <CartesianGrid stroke="#ccc" />
        <XAxis dataKey="name" />
        <YAxis />
        <Tooltip />
      </LineChart>
    </div>
  );
};

// Export the main function.
export default App;
```

Explanation:

* The code starts by importing the necessary libraries.
* The `App` function is the main function of the application. It declares state variables using the `useState` hook and fetches data from the API using the `useEffect` hook.
* The `fetchData` function is an asynchronous function that makes a GET request to the API and stores the response data in the `data` state variable.
* If the `loading` state variable is `true`, a "Loading..." message is displayed.
* If the `error` state variable is not empty, an error message is displayed.
* If neither of the above conditions are met, a line chart is rendered using the `LineChart` component from the `recharts` library.
* The line chart displays the data fetched from the API and allows the user to hover over data points to see more information.
* The `App` function is exported so that it can be used in other parts of the application.