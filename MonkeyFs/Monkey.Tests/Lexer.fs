module Monkey.Lexer

type TokenType = 
    | Illegal    // Unknown token/character
    | Eof        // End of File stops parsing

    // Identifiers and literals
    | Ident      // add, foobar, x, y
    | Int        // 123456
    | String     // "foobar"

    // Operators
    | Assign     // =
    | Plus       // +
    | Minus      // -
    | Bang       // !
    | Asterisk   // *
    | Slash      // /
    | Lt         // <
    | Gt         // >
    | Eq         // ==
    | NotEq      // !=

    // Delimiters
    | Comma      // ,
    | Semicolon  // ;
    | LParen     // (
    | RParen     // )
    | LBrace     // {
    | RBrace     // }
    | LBracket   // [
    | RBracket   // ]
    | Colon      // :

    // Keywords
    | Function
    | Let
    | True
    | False
    | If
    | Else
    | Return

type Token = Token of TokenType * string

type Lexer(input:string) = 
    member this.NextToken() = 
        Token(Illegal, "")