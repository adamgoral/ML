namespace ML.Console

module ExcelParser =
    open FParsec
    open System

    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "%s parse success: %A" str result
        | Failure(errorMsg, _, _) -> printfn "%s parse failure: %s" str errorMsg
    
    //let between pBegin pEnd p = pBegin >>. p .>> pEnd
    let betweenStrings s1 s2 = between (pstring s1) (pstring s2)

    let betweenBrackets = betweenStrings "[" "]"
    let betweenDoubleBrackets = betweenStrings "[[" "]]"
    let floatBetweenBrackets =  betweenBrackets pfloat
    let floatBetweenDoubleBrackets = betweenDoubleBrackets pfloat
    let comaSep p = sepBy p (pstring ",")
    let floatList = pstring "[" >>. comaSep pfloat .>> pstring "]"
    let str_ws s = pstring s .>> spaces
    let float_ws = pfloat .>> spaces
    let numberList = between (str_ws "[") (str_ws "]") (sepBy float_ws (str_ws ","))
    let identifier =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> spaces

    let stringLiteral =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        between (pstring "\"") (pstring "\"")
                (manyChars (normalChar <|> escapedChar))

    let tests() =
        test pfloat "1.0"
        test pfloat "2.1E-4"
        test floatBetweenBrackets "[1.99999]"
        test floatBetweenBrackets "[1 ]"
        test floatBetweenDoubleBrackets "[[1]]"
        test floatBetweenDoubleBrackets "[[1]"
        test (many floatBetweenBrackets) "[1][2][2.0E5]"
        test floatList "[1,2.0,4.,3.9E-5]"
        test (spaces >>. numberList) "    [ 2, 4 , 5.3 ]"
        test numberList "[ 2, 4 , 5.3 ]"
        test identifier "_abc1 "
        test identifier "6abc1 "
        test identifier "abc1=0"
        test stringLiteral "\"\\\\ab\\\"c\""
        ()

    type ExcelExpression =
        | EString of string
        | EBool of bool
        | ENumber of float
        | EOp of string * ExcelExpression * ExcelExpression
        | EFunc of string * ExcelExpression list
        | ERef of string

    let pNumber = pfloat |>> ENumber
    let pBool = (stringReturn "TRUE" (EBool true)) <|> (stringReturn "FALSE" (EBool false))
    //let pOpSymbol = (pstring ">=" <|> pstring "<=" <|> (anyOf "&*/-+^=" |>> string)) .>> spaces
    let pStringLiteral =
        let normalChar = many1Satisfy (fun c -> c <> '"')
        between (pstring "\"") (pstring "\"") (manyStrings normalChar) |>> EString

    //let pExpression = (pNumber <|> pBool <|> pStringLiteral <|> pOp) .>> spaces
    //let pOp = pipe3 pExpression pOpSymbol pExpression (fun a b c -> EOp(b,a,c))
    let pValue = pNumber <|> pBool <|> pStringLiteral
    let pStringAsciiU = many1Chars (satisfy isAsciiUpper)
    let pStringNumber = many1Chars (satisfy isDigit)
    let pAddressA = (pstring "$" <|> pstring "") >>. pStringAsciiU
    let pAddressN = (pstring "$" <|> pstring "") >>. pStringNumber
    let pCellRefAbs = pipe2 pAddressA pAddressN (fun c n -> ERef (c + n))
    let pCellRefRange = pipe5 pAddressA pAddressN (pstring ":") pAddressA pAddressN (fun bc br _ ec er -> ERef (bc + br + ":" + ec + er))
    let pCellRef = pCellRefRange <|> pCellRefAbs

    let pOp =
        let opp = new OperatorPrecedenceParser<ExcelExpression, unit, unit>()
        opp.AddOperator(InfixOperator("=", spaces, 1, Associativity.Left, fun a b -> EOp("=", a, b)))
        opp.AddOperator(InfixOperator("=>", spaces, 1, Associativity.Left, fun a b -> EOp("=>", a, b)))
        opp.AddOperator(InfixOperator("=<", spaces, 1, Associativity.Left, fun a b -> EOp("=<", a, b)))
        opp.AddOperator(InfixOperator("<>", spaces, 1, Associativity.Left, fun a b -> EOp("<>", a, b)))
        opp.AddOperator(InfixOperator(">", spaces, 1, Associativity.Left, fun a b -> EOp(">", a, b)))
        opp.AddOperator(InfixOperator("<", spaces, 1, Associativity.Left, fun a b -> EOp("<", a, b)))
        opp.AddOperator(InfixOperator("+", spaces, 2, Associativity.Left, fun a b -> EOp("+", a, b)))
        opp.AddOperator(InfixOperator("-", spaces, 2, Associativity.Left, fun a b -> EOp("-", a, b)))
        opp.AddOperator(InfixOperator("*", spaces, 3, Associativity.Right, fun a b -> EOp("*", a, b)))
        opp.AddOperator(InfixOperator("/", spaces, 3, Associativity.Right, fun a b -> EOp("/", a, b)))
        opp.AddOperator(InfixOperator("^", spaces, 4, Associativity.Right, fun a b -> EOp("^", a, b)))
        opp.AddOperator(PrefixOperator("+", spaces, 2, true, (fun a -> EFunc("+", [a]))))
        opp.AddOperator(PrefixOperator("-", spaces, 2, true, (fun a -> EFunc("-", [a]))))
        let expr = opp.ExpressionParser
        let pExpr = pValue <|> pCellRef .>> spaces
        let term = pExpr <|> between (str_ws "(") (str_ws ")") expr <|> expr
        opp.TermParser <- term
        expr

    let pExpression = pOp <|> pCellRef <|> pValue .>> spaces

    let eTests() =
        test pNumber "1"
        test pNumber "2."
        test pNumber "-2."
        test pNumber "--2."
        test pBool "TRUE"
        test pStringLiteral "\"abc\""
        test pExpression "-2"
        test pExpression "+2"
        test pExpression "--2"
        test pExpression "1 + 2 * 3"
        test pExpression "(1 + 2) *3/4"
        test pCellRef "$A3"
        test pExpression "A3:$R4"

        //test pOp "TRUE >= 3 "
        ()

