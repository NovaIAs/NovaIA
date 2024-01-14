```d
import std.stdio, std.algorithm, std.range, std.string, std::foreach, std::traits;

enum TokenKind {
    TK_EOF, TK_EOL, TK_IDENT, TK_NUMBER, TK_OPERATOR, TK_KEYWORD,
    TK_LPAREN, TK_RPAREN, TK_LBRACE, TK_RBRACE, TK_LSQUARE, TK_RSQUARE,
    TK_COMMA, TK_SEMICOLON, TK_COLON, TK_DOT, TK_ARROW, TK_QUESTMARK,
    TK_COLONCOLON, TK_ASSIGN, TK_EQ, TK_NE, TK_LE, TK_GE, TK_LT, TK_GT,
    TK_PLUS, TK_MINUS, TK_STAR, TK_SLASH, TK_PERCENT, TK_PLUSPLUS,
    TK_MINUSMINUS, TK_AND, TK_OR, TK_XOR, TK_NOT, TK_TILDE, TK_ANDAND,
    TK_OROR, TK_ELLIPSIS, TK_STRING, TK_CHAR, TK_COMMENT, TK_PREPROCESSOR
}

struct Token {
    TokenKind kind;
    string text;
    int length;
    int line;
    int column;
}

class Scanner {
    string source;
    int pos;
    int line;
    int column;

public:
    Scanner(string source) {
        this.source = source;
        this.pos = 0;
        this.line = 1;
        this.column = 0;
    }

    Token next() {
        Token token;

        while (pos < source.length && source[pos] == ' ') {
            pos++;
            column++;
        }

        if (pos >= source.length) {
            token.kind = TK_EOF;
            return token;
        }

        if (source[pos] == '\n') {
            pos++;
            line++;
            column = 0;
            token.kind = TK_EOL;
            return token;
        }

        if (isLetter(source[pos])) {
            int start = pos;
            while (pos < source.length && (isLetter(source[pos]) || isDigit(source[pos]) || source[pos] == '_')) {
                pos++;
                column++;
            }
            token.text = source.substring(start, pos);
            token.length = pos - start;
            token.kind = TK_IDENT;
            return token;
        }

        if (isDigit(source[pos])) {
            int start = pos;
            while (pos < source.length && isDigit(source[pos])) {
                pos++;
                column++;
            }
            if (source[pos] == '.') {
                pos++;
                column++;
                while (pos < source.length && isDigit(source[pos])) {
                    pos++;
                    column++;
                }
            }
            token.text = source.substring(start, pos);
            token.length = pos - start;
