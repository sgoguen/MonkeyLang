module Monkey.Tests.Lexer

open System
open Xunit

open Monkey.Lexer
open Monkey.Lexer.Patterns

[<Fact>]
let ``Patterns`` () = 
    let (CharList(Token(t, rest))) = "5 + 2"
    Assert.Equal(Token(Int, "5"), t)
    let (CharList(Token(t, rest))) = "\"Hello World\" + 2"
    Assert.Equal(Token(String, "Hello World"), t)
    

[<Fact>]
let ``Test Next Token`` () =
    let input = @"
                let five = 5;
                let ten = 10;
                let add = fn(x, y) {
                x + y;
                };
                let result = add(five, ten);
                !-/*5;
                5 < 10 > 5;

                if (5 < 10) {
                    return true;    
                } else {
                    return false;
                }

                10 == 10;
                10 != 9;
                ""foobar""
                ""foo bar""
                [1, 2];
                {""foo"": ""bar""}";

    let tokens = [
        Token(TokenType.Let, "let")
        Token(TokenType.Ident, "five")
        Token(TokenType.Assign, "=")
        Token(TokenType.Int, "5")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.Let, "let")
        Token(TokenType.Ident, "ten")
        Token(TokenType.Assign, "=")
        Token(TokenType.Int, "10")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.Let, "let")
        Token(TokenType.Ident, "add")
        Token(TokenType.Assign, "=")
        Token(TokenType.Function, "fn")
        Token(TokenType.LParen, "(")
        Token(TokenType.Ident, "x")
        Token(TokenType.Comma, ",")
        Token(TokenType.Ident, "y")
        Token(TokenType.RParen, ")")
        Token(TokenType.LBrace, "{")
        Token(TokenType.Ident, "x")
        Token(TokenType.Plus, "+")
        Token(TokenType.Ident, "y")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.RBrace, "}")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.Let, "let")
        Token(TokenType.Ident, "result")
        Token(TokenType.Assign, "=")
        Token(TokenType.Ident, "add")
        Token(TokenType.LParen, "(")
        Token(TokenType.Ident, "five")
        Token(TokenType.Comma, ",")
        Token(TokenType.Ident, "ten")
        Token(TokenType.RParen, ")")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.Bang, "!")
        Token(TokenType.Minus, "-")
        Token(TokenType.Slash, "/")
        Token(TokenType.Asterisk, "*")
        Token(TokenType.Int, "5")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.Int, "5")
        Token(TokenType.Lt, "<")
        Token(TokenType.Int, "10")
        Token(TokenType.Gt, ">")
        Token(TokenType.Int, "5")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.If, "if")
        Token(TokenType.LParen, "(")
        Token(TokenType.Int, "5")
        Token(TokenType.Lt, "<")
        Token(TokenType.Int, "10")
        Token(TokenType.RParen, ")")
        Token(TokenType.LBrace, "{")
        Token(TokenType.Return, "return")
        Token(TokenType.True, "true")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.RBrace, "}")
        Token(TokenType.Else, "else")
        Token(TokenType.LBrace, "{")
        Token(TokenType.Return, "return")
        Token(TokenType.False, "false")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.RBrace, "}")
        Token(TokenType.Int, "10")
        Token(TokenType.Eq, "==")
        Token(TokenType.Int, "10")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.Int, "10")
        Token(TokenType.NotEq, "!=")
        Token(TokenType.Int, "9")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.String, "foobar")
        Token(TokenType.String, "foo bar")
        Token(TokenType.LBracket, "[")
        Token(TokenType.Int, "1")
        Token(TokenType.Comma, ",")
        Token(TokenType.Int, "2")
        Token(TokenType.RBracket, "]")
        Token(TokenType.Semicolon, ";")
        Token(TokenType.LBrace, "{")
        Token(TokenType.String, "foo")
        Token(TokenType.Colon, ":")
        Token(TokenType.String, "bar")
        Token(TokenType.RBrace, "}")
        Token(TokenType.Eof, "")
    ]

    // ()
    // let lexer = Lexer(input)
    // let (Tokens(output)) = input
    let output = parseTokenSeq input

    
    // printfn "tokens: %A" tokens
    output |> Seq.mapi (fun i o -> printfn "%i %A" i o) |> ignore
    //printfn "output: %A" (output |> List.toArray)
    

    // let both = List.zip tokens output

    // printfn "%A" both

    // Assert.Equal(tokens.Length, both.Length)

    // lexer.NextToken()
    // let output = 
    //     [ let mutable count = 0
    //       let mutable keepReading = true
    //       while not lexer.EOF && count < 200 do
    //         let token = lexer.NextToken()
    //         count <- count + 1
    //         yield token
    //         let (Token(t, v)) = token
    //         keepReading <- t <> Eof
    //     ]

    // Assert.Equal(add 2 2, 4)
    
    // Assert.StrictEqual(tokens, output)
