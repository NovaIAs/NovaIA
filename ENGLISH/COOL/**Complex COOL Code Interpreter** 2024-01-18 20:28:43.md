**Complex COOL Code**

```cool
class Interpreter {

    // Initialize the interpreter with a given program
    Interpreter(String program) {
        this.program = program;
    }

    // Interpret the program
    void interpret() {
        Lexer lexer = new Lexer(program);
        Parser parser = new Parser(lexer);
        Program ast = parser.parse();

        // Create an environment for the program
        Environment env = new Environment();

        // Evaluate the program in the environment
        ast.eval(env);
    }
}

class Lexer {

    // The program to be lexed
    private String program;

    // The current position in the program
    private int pos;

    // The current character being processed
    private char ch;

    // Constructor
    Lexer(String program) {
        this.program = program;
        pos = 0;
        ch = program.charAt(pos);
    }

    // Get the next token from the program
    Token nextToken() {
        // Skip whitespace
        while (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
            nextChar();
        }

        // If we're at the end of the program, return an EOF token
        if (ch == '\0') {
            return new Token(TokenType.EOF, "");
        }

        // Otherwise, get the next token
        switch (ch) {
            case '(':
                nextChar();
                return new Token(TokenType.LPAREN, "(");
            case ')':
                nextChar();
                return new Token(TokenType.RPAREN, ")");
            case '+':
                nextChar();
                return new Token(TokenType.PLUS, "+");
            case '-':
                nextChar();
                return new Token(TokenType.MINUS, "-");
            case '*':
                nextChar();
                return new Token(TokenType.STAR, "*");
            case '/':
                nextChar();
                return new Token(TokenType.SLASH, "/");
            case '%':
                nextChar();
                return new Token(TokenType.MOD, "%");
            case '=':
                nextChar();
                return new Token(TokenType.EQ, "=");
            case '<':
                nextChar();
                return new Token(TokenType.LT, "<");
            case '>':
                nextChar();
                return new Token(TokenType.GT, ">");
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                return getNumber();
            case '"':
                return getString();
            case ';':
                nextChar();
                return new Token(TokenType.SEMI, ";");
            case ',':
                nextChar();
                return new Token(TokenType.COMMA, ",");
            default:
                throw new RuntimeException("Unexpected character: " + ch);
        }
    }

    // Get the next character from the program
    private void nextChar() {
        pos++;
        if (pos >= program.length()) {
            ch = '\0';
        } else {
            ch = program.charAt(pos);
        }
    }

    // Get the next number from the program
    private Token getNumber() {
        StringBuilder sb = new StringBuilder();
        while (ch >= '0' && ch <= '9') {
            sb.append(ch);
            nextChar();
        }
        return new Token(TokenType.NUM, sb.toString());
    }

    // Get the next string from the program
    private Token getString() {
        StringBuilder sb = new StringBuilder();
        nextChar(); // Skip the opening quote
        while (ch != '"' && ch != '\0') {
            sb.append(ch);
            nextChar();
        }
        nextChar(); // Skip the closing quote
        return new Token(TokenType.STR, sb.toString());
    }
}

class Parser {

    // The lexer to use for parsing
    private Lexer lexer;

    // The current token being processed
    private Token currentToken;

    // Constructor
    Parser(Lexer lexer) {
        this.lexer = lexer;
        currentToken = lexer.nextToken();
    }

    // Parse the program
    Program parse() {
        Program program = new Program();
        while (currentToken.type != TokenType.EOF) {
            program.addStatement(parseStatement());
        }
        return program;
    }

    // Parse a statement
    Statement parseStatement() {
        switch (currentToken.type) {
            case TokenType.