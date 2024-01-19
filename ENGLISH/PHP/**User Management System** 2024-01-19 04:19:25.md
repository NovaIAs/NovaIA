```php
<?php

// Define the namespace
namespace App\Http\Controllers;

// Import the necessary classes
use Illuminate\Http\Request;
use Illuminate\Support\Facades\DB;
use Illuminate\Support\Facades\Validator;

/**
 * Class UserController
 *
 * This class handles all user-related requests
 */
class UserController extends Controller
{
    /**
     * Display a listing of the users.
     *
     * @return \Illuminate\Http\Response
     */
    public function index()
    {
        // Fetch all users from the database
        $users = DB::table('users')->get();

        // Return the view with the list of users
        return view('users.index', ['users' => $users]);
    }

    /**
     * Show the form for creating a new user.
     *
     * @return \Illuminate\Http\Response
     */
    public function create()
    {
        // Return the view for creating a new user
        return view('users.create');
    }

    /**
     * Store a newly created user in storage.
     *
     * @param  \Illuminate\Http\Request  $request
     * @return \Illuminate\Http\Response
     */
    public function store(Request $request)
    {
        // Validate the user input
        $validator = Validator::make($request->all(), [
            'name' => 'required|max:255',
            'email' => 'required|email|unique:users',
            'password' => 'required|min:6',
        ]);

        // If the validation fails, redirect back to the create page with errors
        if ($validator->fails()) {
            return redirect()->route('users.create')
                ->withErrors($validator)
                ->withInput();
        }

        // Create a new user
        $user = new User();
        $user->name = $request->name;
        $user->email = $request->email;
        $user->password = bcrypt($request->password);
        $user->save();

        // Redirect to the user list page with a success message
        return redirect()->route('users.index')->with('success', 'User created successfully.');
    }

    /**
     * Display the specified user.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */
    public function show($id)
    {
        // Fetch the user from the database
        $user = DB::table('users')->find($id);

        // Return the view with the user details
        return view('users.show', ['user' => $user]);
    }

    /**
     * Show the form for editing the specified user.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */
    public function edit($id)
    {
        // Fetch the user from the database
        $user = DB::table('users')->find($id);

        // Return the view for editing the user
        return view('users.edit', ['user' => $user]);
    }

    /**
     * Update the specified user in storage.
     *
     * @param  \Illuminate\Http\Request  $request
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */
    public function update(Request $request, $id)
    {
        // Validate the user input
        $validator = Validator::make($request->all(), [
            'name' => 'required|max:255',
            'email' => 'required|email|unique:users,email,' . $id,
            'password' => 'required|min:6',
        ]);

        // If the validation fails, redirect back to the edit page with errors
        if ($validator->fails()) {
            return redirect()->route('users.edit', $id)
                ->withErrors($validator)
                ->withInput();
        }

        // Update the user
        $user = User::find($id);
        $user->name = $request->name;
        $user->email = $request->email;
        $user->password = bcrypt($request->password);
        $user->save();

        // Redirect to the user list page with a success message
        return redirect()->route('users.index')->with('success', 'User updated successfully.');
    }

    /**
     * Remove the specified user from storage.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */
    public function destroy($id)
    {
        // Fetch the user from the database
        $user = DB::table('users')->find($id);

        // Delete the user
        $user->delete();

        // Redirect to the user list page with a success message
        return redirect()->route('users.index')->with('success', 'User deleted successfully.');
    }
}
```

**Explanation:**

The code above is a PHP class that handles user-related requests in a web application. It uses the Laravel framework for building modern web applications. The class contains methods for listing, creating, editing, and deleting users. It also includes validation to ensure that user input is valid before saving it to the database.

Here's a breakdown of the code:

1. **Namespace Declaration:**
   ```php
   namespace App\Http\Controllers;
   ```
   This line declares the namespace for the class. It indicates that the class is located in the `App\Http\Controllers` namespace.

2. **Class Declaration:**
   ```php
   class UserController extends Controller
   ```
   This line declares the `UserController` class. It extends the `Controller` class, which provides common functionality for all controllers in a Laravel application.

3. **Index Method:**
   ```php
   public function index()
   {
       // Fetch all users from the database
       $users = DB::table('users')->get();

       // Return the view with the list of users
       return view('users.index', ['users' => $users]);
   }
   ```
   This method displays a list of all users in the database. It fetches the users from the `users` table using Eloquent ORM. Then, it returns a view named `users.index` with the list of users.

4. **Create Method:**
   ```php
   public function create()
   {
       // Return the view for creating a new user
       return view('users.create');
   }
   ```
   This method displays the form for creating a new user. It returns the view named `users.create`.

5. **Store Method:**
   ```php
   public function store(Request $request)
   {
       // Validate the user input
       $validator = Validator::make($request->all(), [
           'name' => 'required|max:255',
           'email' => 'required|email|unique:users',
           'password' => 'required|min:6',
       ]);

       // If the validation fails, redirect back to the create page with errors
       if ($validator->fails()) {
           return redirect()->route('users.create')
               ->withErrors($validator)
               ->withInput();
       }

       // Create a new user
       $user = new User();
       $user->name = $request->name;
       $user->email = $request->email;
       $user->password = bcrypt($request->password);
       $user->save();

       // Redirect to the user list page with a success message
       return redirect()->route('users.index')->with('success', 'User created successfully.');
   }
   ```
   This method handles the form submission for creating a new user. It validates the user input using the Laravel validator. If the validation fails, it redirects back to the create page with error messages. Otherwise, it creates a new user, saves it to the database, and redirects to the user list page with a success message.

6. **Show Method:**
   ```php
   public function show($id)
   {
       // Fetch the user from the database
       $user = DB::table('users')->find($id);

       // Return the view with the user details
       return view('users.show', ['user' => $user]);
   }
   ```
   This method displays the details of a specific user. It fetches the user from the database by its ID and returns a view named `users.show` with the user details.

7. **Edit Method:**
   ```php
   public function edit($id)
   {
       // Fetch the user from the database
       $user = DB::table('users')->find($id);

       // Return the view for editing the user
       return view('users.edit', ['user' => $user]);
   }
   ```
   This method displays the form for editing a specific user. It fetches the user from the database by its ID and returns a view named `users.edit` with the user details.

8. **Update Method:**
   ```php
   public function update(Request $request, $id)
   {
       // Validate the user input
       $validator = Validator::make($request->all(), [
           'name' => 'required|max:255',
           'email' => 'required|email|unique:users,email,' . $id,
           'password' => 'required|min:6',
       ]);

       // If the validation fails, redirect back to the edit page with errors
       if ($validator->fails()) {
           return redirect()->route('users.edit', $id)
               ->withErrors($validator)
               ->withInput();
       }

       // Update the user
       $user = User::find($id);
       $user->name = $request->name;
       $user->email = $request->email;
       $user->password = bcrypt($request->password);
       $user->save();

       // Redirect to the user list page with a success message
       return redirect()->route('users.index')->with('success', 'User updated successfully.');
   }
   ```
   This method handles the form submission for updating a specific user. It validates the user input using the Laravel validator. If the validation fails, it redirects back to the edit page with error messages. Otherwise, it updates the user in the database and redirects to the user list page with a success message.

9. **Destroy Method:**
   ```php
   public function destroy($id)
   {
       // Fetch the user from the database
       $user = DB::table('users')->find($id);

       // Delete the user
       $user->delete();

       // Redirect to the user list page with a success message
       return redirect()->route('users.index')->with('success', 'User deleted successfully.');
   }
   ```
   This method handles the deletion of a specific user. It fetches the user from the database by its ID, deletes the user, and redirects to the user list page with a success message.

The above code demonstrates the OOP approach in PHP to create a complex and differentiated controller class for handling user-related requests in a web application.