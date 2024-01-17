```typescript
import React, { useState, useEffect } from 'react';
import { Row, Col, Button, InputGroup, Form, FormControl } from 'react-bootstrap';
import './App.css';

const API_URL = 'https://api.github.com/users/';

const App = () => {
  const [username, setUsername] = useState('');
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(false);

  const fetchUserData = async () => {
    setLoading(true);
    try {
      const response = await fetch(`${API_URL}${username}`);
      const json = await response.json();
      setData(json);
    } catch (error) {
      console.error(error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchUserData();
  }, [username]);

  return (
    <div className="App">
      <Row>
        <Col xs={12}>
          <h1>GitHub User Data</h1>
        </Col>
      </Row>
      <Row>
        <Col xs={12} md={6}>
          <Form.Group>
            <Form.Label>Username:</Form.Label>
            <InputGroup>
              <FormControl
                type="text"
                placeholder="Enter your GitHub username"
                value={username}
                onChange={(e) => setUsername(e.target.value)}
              />
              <InputGroup.Append>
                <Button variant="primary" onClick={fetchUserData}>
                  {loading ? 'Loading...' : 'Search'}
                </Button>
              </InputGroup.Append>
            </InputGroup>
          </Form.Group>
        </Col>
      </Row>
      <Row>
        <Col xs={12} md={6}>
          {data && (
            <>
              <img src={data.avatar_url} alt={data.name} className="profile-image" />
              <h2>{data.name}</h2>
              <p>Bio: {data.bio}</p>
              <p>Public repos: {data.public_repos}</p>
              <p>Followers: {data.followers}</p>
              <p>Following: {data.following}</p>
              <a href={data.html_url} target="_blank" rel="noreferrer">
                Visit Profile
              </a>
            </>
          )}
        </Col>
      </Row>
    </div>
  );
};

export default App;
```

Explanation:

* This code is a React application that fetches user data from the GitHub API and displays it in a user-friendly manner.
* The `fetchUserData()` function is used to fetch the user data from the GitHub API. It takes the username as a parameter and makes an API call to the GitHub API to get the user data.
* The `useEffect()` hook is used to call the `fetchUserData()` function when the `username` state changes. This ensures that the user data is fetched whenever the username is updated.
* The `data` state is used to store the user data fetched from the API.
* The `loading` state is used to indicate whether the user data is being fetched or not.
* The `App` component renders the user interface. It includes a form for the user to enter their GitHub username, a button to trigger the API call, and a section to display the user data.

When the user enters their GitHub username and clicks the "Search" button, the `fetchUserData()` function is called. This function makes an API call to the GitHub API to get the user data. Once the API call is successful, the `data` state is updated with the user data and the `loading` state is set to `false`. This causes the `App` component to re-render and display the user data.