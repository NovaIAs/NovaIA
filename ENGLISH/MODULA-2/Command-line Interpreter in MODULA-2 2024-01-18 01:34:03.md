```modula-2

MODULE ComplexCode;

IMPORT Files, Strings, Terminal;

TYPE
  FileName = ARRAY[1..80] OF CHAR;

  Command = RECORD
    Name: ARRAY[1..80] OF CHAR;
    Args: ARRAY[1..128] OF CHAR;
  END;

  Result = RECORD
    Success: BOOLEAN;
    Output: ARRAY[1..1024] OF CHAR;
  END;

PROCEDURE ReadCommand(fileName: FileName): Command;

VAR
  cmd: Command;
BEGIN
  Files.Open(fileName, Files.Input, Files.Normal);
  Strings.Line(Files.CurrentInput, cmd.Name, 80);
  Strings.Line(Files.CurrentInput, cmd.Args, 128);
  Files.Close(Files.CurrentInput);
  RETURN cmd;
END ReadCommand;

PROCEDURE ExecuteCommand(cmd: Command): Result;

VAR
  result: Result;
  child: INTEGER;
  status: INTEGER;
BEGIN
  result.Success := TRUE;
  result.Output := "";
  child := Terminal.Fork(*NORETURN*);
  IF child = 0 THEN
  BEGIN
    Terminal.SetIn(Files.Input);
    Terminal.SetOut(Files.Output);
    Terminal.SetErr(Files.Output);
    Terminal.Exec(cmd.Name, cmd.Args,Terminal.Env);
  END;
  result.Success := Terminal.Wait(child, status) = 0;
  IF result.Success THEN
  BEGIN
    Files.Open(Terminal.Out, Files.Input, Files.Normal);
    LOOP
      Strings.Line(Files.CurrentInput, result.Output, 1024);
    EXIT WHEN result.Output = "";
    END;
    Files.Close(Files.CurrentInput);
  END;
  RETURN result;
END ExecuteCommand;

PROCEDURE Main;

VAR
  cmd: Command;
  result: Result;
BEGIN
  WHILE TRUE DO
  BEGIN
    Write("Enter command file: ");
    Terminal.ReadLn(fileName);
    cmd := ReadCommand(fileName);
    result := ExecuteCommand(cmd);
    IF result.Success THEN
      Write("Command succeeded\n")
    ELSE
      Write("Command failed\n");
    Write(result.Output);
  END;
END Main.

```

This code defines a MODULA-2 program that reads a command from a file, executes it, and prints the output. The program first reads the command file name from the terminal, then calls the ReadCommand procedure to read the command from the file. The ReadCommand procedure opens the file, reads the command name and arguments, and closes the file.

The program then calls the ExecuteCommand procedure to execute the command. The ExecuteCommand procedure forks a new process, sets the input, output, and error streams for the new process, and executes the command. The ExecuteCommand procedure waits for the new process to finish, and if the command succeeded, it reads the output from the new process and prints it.

The program then loops back to the beginning and waits for the user to enter another command file name.