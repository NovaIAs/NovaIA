```shell
#!/bin/bash

# This script automates the process of creating a new Git repository, cloning an existing repository,
# and pushing changes to a remote repository.

# Usage:
# ./git-helper.sh [command] [arguments]

# Commands:
# - init: Initialize a new Git repository in the current directory.
# - clone: Clone an existing Git repository to the current directory.
# - push: Push changes to a remote Git repository.

# Arguments:
# For the 'init' command, no arguments are required.
# For the 'clone' command, the argument is the URL of the repository to clone.
# For the 'push' command, the argument is the name of the remote repository to push to.

# Get the command and arguments from the command line.
command=$1
args=${@:2}

# Execute the appropriate command.
case $command in
  "init")
    git init
    ;;
  "clone")
    git clone $args
    ;;
  "push")
    git push $args
    ;;
  *)
    echo "Invalid command. Usage: ./git-helper.sh [command] [arguments]"
    ;;
esac
```

**Explanation:**

This script is a command-line tool that automates common Git operations such as initializing a new repository, cloning an existing repository, and pushing changes to a remote repository. It takes a command and arguments as input and executes the appropriate Git command based on the provided input.

The script starts by getting the command and arguments from the command line using the `$1` and `$@` variables. The `$1` variable contains the command, and the `$@` variable contains all the arguments passed to the script.

Next, the script uses a `case` statement to execute the appropriate Git command based on the provided command. The `case` statement checks the value of the `command` variable and executes the corresponding Git command.

For the `init` command, the script simply calls the `git init` command to initialize a new Git repository in the current directory.

For the `clone` command, the script calls the `git clone` command with the provided repository URL as an argument to clone the repository to the current directory.

For the `push` command, the script calls the `git push` command with the provided remote repository name as an argument to push changes to the remote repository.

If an invalid command is provided, the script prints an error message and displays the usage information.

This script is a useful tool for automating common Git operations and can help streamline the development workflow.