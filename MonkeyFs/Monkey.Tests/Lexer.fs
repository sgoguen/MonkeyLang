module Monkey.Lexer
open System.Text

let add x y = 
    if x > 5 then 
        y
    else 
        x + y

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

module Patterns = 

    let isLetter ch = 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch = '_'
    let isDigit(ch) = '0' <= ch && ch <= '9'
    let isWhiteSpace(ch) = ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r'
    let readChars test (chars:char list) = 
        let sb = StringBuilder()
        let rec readCharsInternal = function 
            | [] -> sb.ToString(), []
            | (c:char)::rest -> 
                if test c then
                    sb.Append(c) |> ignore
                    readCharsInternal rest
                else
                    (sb.ToString()), rest
        match readCharsInternal chars with
        | "", _ -> None
        | s, rest -> Some(s, rest)

    let rec readString = function
        | '\"'::rest -> startRead (StringBuilder()) rest
        | _ -> None
    and startRead sb = function
        | '\"'::rest -> Some(sb.ToString(), rest)
        | c::rest -> startRead (sb.Append(c)) rest
        | [] -> raise(exn("String unexpectedly completed"))

    let (|Numeric|_|) = readChars isDigit
    let (|Literal|_|) = readChars isLetter
    let (|StringLiteral|_|) = readString
    let (|WhiteSpace|_|) = readChars isWhiteSpace

    

    let keyWords = Map.ofList [
        ("fn", Function)
        ("let", Let)
        ("true", True)
        ("false", False)
        ("if", If)
        ("else", Else)
        ("return", Return) 
    ]   

    let (|Keyword|_|) = keyWords.TryFind
    

    let toCharList(s:string) = s.ToCharArray() |> List.ofArray

    let (|CharList|) = toCharList

    let rec getToken = function
        | WhiteSpace(rs, rest) -> getToken(rest)
        | StringLiteral(s, rest) -> Token(String, s), rest
        | '='::'='::rest -> Token(Eq, "=="), rest
        | '='::rest -> Token(Assign, "="), rest
        | '+'::rest -> Token(Plus, "+"), rest
        | '-'::rest -> Token(Minus, "-"), rest
        | '/'::rest -> Token(Slash, "/"), rest
        | '*'::rest -> Token(Asterisk, "*"), rest
        | '<'::rest -> Token(Lt, "<"), rest
        | '>'::rest -> Token(Gt, ">"), rest
        | ';'::rest -> Token(Semicolon, ";"), rest
        | ','::rest -> Token(Comma, ","), rest
        | '('::rest -> Token(LParen, "("), rest
        | ')'::rest -> Token(RParen, ")"), rest
        | '{'::rest -> Token(LBrace, "{"), rest
        | '}'::rest -> Token(RBrace, "}"), rest
        | '['::rest -> Token(LBracket, "["), rest
        | ']'::rest -> Token(RBracket, "]"), rest
        | ':'::rest -> Token(Colon, ":"), rest
        | '\000'::rest -> Token(Slash, ""), rest
        | Literal(Keyword(keyword) as s, rest) -> Token(keyword, s), rest
        | Numeric(n, rest) -> Token(Int, n), rest
        | c::rest -> Token(Illegal, c.ToString()), rest
        | [] -> Token(Eof, ""), []

    let (|Token|) = getToken

    let rec tokenSeq input =
        seq { 
            match input with
            | Token(t, []) -> yield t
            | Token(t, rest) -> 
                yield t
                yield! tokenSeq rest
        }

    let parseTokenSeq = toCharList >> tokenSeq


    // let rec parseTokens = function
    //     | Token(t, []) -> [t]
    //     | Token(t, rest) -> t::parseTokens rest

    // let (|Tokens|) = toCharList >> parseTokens

type Lexer(input:string) as this = 
    let mutable position = 0
    let mutable readPosition = 0
    let mutable ch = char(0)

    let keyWords = Map.ofList [
        ("fn", Function)
        ("let", Let)
        ("true", True)
        ("false", False)
        ("if", If)
        ("else", Else)
        ("return", Return) 
    ]    

    do this.ReadChar()

    member this.EOF = position >= input.Length
    member this.NextToken() = 
        let inline returnToken(tokenType, tokenValue) = 
            let tok = Token(tokenType, tokenValue)
            this.ReadChar()
            tok
        
        this.SkipWhitespace()

        match ch with
        | '=' when this.PeekChar() = '=' -> 
                this.ReadChar()
                returnToken(Eq, "==")
        | '=' -> returnToken(Assign, "=")
        | '+' -> returnToken(Plus, "+")
        | '-' -> returnToken(Minus, "-")
        | '!' when this.PeekChar() = '=' ->
                this.ReadChar()
                returnToken(NotEq, "!=")
        | '/' -> returnToken(Slash, "/")
        | '*' -> returnToken(Asterisk, "*")
        | '<' -> returnToken(Lt, "<")
        | '>' -> returnToken(Gt, ">")
        | ';' -> returnToken(Semicolon, ";")
        | ',' -> returnToken(Comma, ",")
        | '(' -> returnToken(LParen, "(")
        | ')' -> returnToken(RParen, ")")
        | '(' -> returnToken(LParen, "(")
        | ')' -> returnToken(RParen, ")")
        | '"' -> returnToken(String, this.ReadString())
        | '[' -> returnToken(LBracket, "[")
        | ']' -> returnToken(RBracket, "]")
        | ':' -> returnToken(Colon, ":")
        | '\000' -> returnToken(Slash, "")
        | c when this.IsLetter(c) ->
            let ident = this.ReadIdentifier()
            let tokenType = this.LookupIdent(ident)
            Token(tokenType, ident)
        | c when this.IsDigit(ch) ->
            let literal = this.ReadNumber()
            Token(Int, literal)
        | c -> 
            let tok = Token(Illegal, c.ToString())
            this.ReadChar()
            tok

    member this.LookupIdent(ident) = 
        if keyWords.ContainsKey(ident) then
            keyWords.Item(ident)
        else
            Ident

    member this.PeekChar() = 
        if readPosition > input.Length then 
            char(0) 
        else 
            input.[readPosition]        

    member this.ReadChar() = 
        ch <- if readPosition >= input.Length then 
                char(0) 
              else 
                input.[readPosition]
        readPosition <- readPosition + 1

    member this.ReadIdentifier() =
        let p = position
        while this.IsLetter(ch) do
            this.ReadChar()
        input.Substring(p, position - p)

    member this.ReadNumber() = 
        let p = position
        while this.IsDigit(ch) do
            this.ReadChar()
        input.Substring(p, position - p)
    
    member this.IsLetter(ch) = 
        'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch = '_'

    member this.SkipWhitespace() =
        while ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r' do
            this.ReadChar()

    member this.IsDigit(ch) = '0' <= ch && ch <= '9'

    member this.ReadString() = 
        let pos = position + 1
        let rec doUntil() = 
            this.ReadChar()
            if position >= input.Length then 
                raise(exn("Oh noes!"))
            if ch <> '\"' then doUntil() else ()
        doUntil()
        input.Substring(pos, position - pos)



