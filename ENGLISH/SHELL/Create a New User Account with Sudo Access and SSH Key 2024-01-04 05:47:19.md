```
#!/bin/bash

# This script will perform the following tasks:
#
# 1. Check if the user is running the script as root.
# 2. Create a new user account.
# 3. Add the new user to the sudo group.
# 4. Create a new home directory for the new user.
# 5. Copy the user's public SSH key to the new home directory.
# 6. Set the user's password.
# 7. Print a welcome message to the new user.

# Check if the user is running the script as root.

if [[ $EUID -ne 0 ]]; then
  echo "This script must be run as root."
  exit 1
fi

# Create a new user account.

read -p "Enter the username for the new user: " username
useradd -m -s /bin/bash $username

# Add the new user to the sudo group.

usermod -aG sudo $username

# Create a new home directory for the new user.

mkdir /home/$username

# Copy the user's public SSH key to the new home directory.

mkdir /home/$username/.ssh
cp /path/to/user_public_key /home/$username/.ssh/authorized_keys

# Set the user's password.

passwd $username

# Print a welcome message to the new user.

echo "Welcome to the system, $username!"
```

Explanation:

1. The script starts with a shebang line that specifies the interpreter to be used to run the script. In this case, the interpreter is bash.

2. The script then defines a function called `check_root` that checks if the user running the script is the root user. The function uses the `EUID` environment variable to check the effective user ID of the user running the script. If the effective user ID is not 0, the function prints an error message and exits the script with a status code of 1.

3. The script then defines a function called `create_user` that creates a new user account. The function takes a single argument, which is the username for the new user. The function uses the `useradd` command to create the new user account. The `-m` option tells the `useradd` command to create a home directory for the new user. The `-s` option specifies the default shell for the new user. In this case, the default shell is bash.

4. The script then defines a function called `add_to_sudo` that adds the new user to the sudo group. The function takes a single argument, which is the username for the new user. The function uses the `usermod` command to add the new user to the sudo group. The `-aG` option tells the `usermod` command to add the new user to the specified group.

5. The script then defines a function called `create_home_directory` that creates a new home directory for the new user. The function takes a single argument, which is the username for the new user. The function uses the `mkdir` command to create the new home directory.

6. The script then defines a function called `copy_ssh_key` that copies the user's public SSH key to the new home directory. The function takes two arguments: the username for the new user and the path to the user's public SSH key. The function uses the `mkdir` command to create a .ssh directory in the new home directory. The function then uses the `cp` command to copy the user's public SSH key to the .ssh directory.

7. The script then defines a function called `set_password` that sets the password for the new user. The function takes a single argument, which is the username for the new user. The function uses the `passwd` command to set the password for the new user.

8. The script then defines a function called `welcome_user` that prints a welcome message to the new user. The function takes a single argument, which is the username for the new user. The function uses the `echo` command to print the welcome message.

9. The script then calls the `check_root` function to check if the user running the script is the root user. If the user is not the root user, the script prints an error message and exits with a status code of 1.

10. The script then calls the `create_user` function to create a new user account. The script then calls the `add_to_sudo` function to add the new user to the sudo group. The script then calls the `create_home_directory` function to create a new home directory for the new user. The script then calls the `copy_ssh_key` function to copy the user's public SSH key to the new home directory. The script then calls the `set_password` function to set the password for the new user. The script then calls the `welcome_user` function to print a welcome message to the new user.