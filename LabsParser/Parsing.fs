namespace Buzz

namespace Buzz
open FParsec
open Types

    module Parsing = 
        type Parser<'t> = Parser<'t, unit>

        let ws = spaces // Whitespace

        let betweenBrackets = between (pchar '[') (pchar ']') (manySatisfy (isNoneOf "]"))
        let interfaceKey = (pchar 'I') >>. betweenBrackets
        let lstigKey = (pchar 'L') >>. betweenBrackets

        /// Parser for values
        let pval : Parser<_> = 
            let pintval = pint32 |>> Int
            let pstringval = 
                let pquotes = (pchar '"')
                between pquotes pquotes (manySatisfy (isNoneOf "\""))
                |>> Val.String
            let ppointval =
                between (pchar '(') (pchar ')') 
                    (tuple2 pint32 (between ws ws (pchar ',') >>. pint32))
                    |>> P
            choice [pintval; pstringval; ppointval]
        
        // Example of a recursive parser
        // http://hestia.typepad.com/flatlander/2011/07/recursive-parsers-in-fparsec.html
        let pexpr, pexprRef = createParserForwardedToRef()

        let pexprTerm = 
            let pexprL = lstigKey |>> Expr.L
            let pexprI = interfaceKey |>> Expr.I
            let pexprConst = pval |>> Const
            choice [pexprConst; pexprL; pexprI]

        let private pexprSum = tuple2 pexprTerm (between ws ws (pchar '+') >>. pexpr) |>> Sum

        // assign to "pexprRef" the choice of the above parsers
        do pexprRef := choice [(attempt pexprSum); pexprTerm]


        let pbop : Parser<_> =
            choice[
                (stringReturn "<" Less);
                (stringReturn "=" Equal);
                (stringReturn ">" Greater);
            ]

        let pbexpr, pbexprRef = createParserForwardedToRef()

        let pbexprTerm : Parser<_> = 
            let pbexprNeg = (pchar '!') >>. pbexpr |>> Neg
            let pbexprCompare =
                tuple3 pexpr (between ws ws pbop) pexpr |>> Compare
            choice [
                attempt pbexprNeg;
                attempt pbexprCompare;
                stringReturn "true" True;
                stringReturn "false" False;
            ]
        
        let private pbexprConj =
            tuple2 pbexprTerm (between ws ws (pchar '&') >>. pbexpr) |>> Conj


        do pbexprRef := 
            choice [attempt pbexprConj; pbexprTerm]

        let paction : Parser<_> =
            let pactionAttr = 
                tuple2 interfaceKey (between ws ws (pstring ":=") >>. pexpr) |>> AttrUpdate
            let pactionLstig =
                tuple2 lstigKey (between ws ws (pstring ":=") >>. pexpr) |>> LStigUpdate
            let pactionAwait =
                pbexpr .>> followedBy (pchar '?') |>> Await
            choice [pactionAwait; pactionAttr; pactionLstig]

        let pproc, pprocRef = createParserForwardedToRef()
        let pprocTerm, pprocTermRef = createParserForwardedToRef()

        let pprocTermImpl : Parser<_>  =
            let pprocNil = stringReturn "0" Nil
            let pprocParen = between (pchar '(') (pchar ')') pproc
            let pprocSeq =
                tuple2 paction (pchar '.' >>. pprocTerm) |>> Process.Seq
            choice [pprocSeq; pprocParen; pprocNil]
        
        do pprocTermRef := pprocTermImpl

        let private pprocChoice : Parser<_> =
            tuple2 pprocTerm (opt (between ws ws (pchar '+') >>. pproc)) 
            |>> function
            | a, Some(b) -> Process.Choice(a, b)
            | a, None -> a
       
        
        do pprocRef := choice [pprocChoice; pprocTerm]