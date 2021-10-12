module Day18 =

    type private Operator = Add | Multiply

    type private Expression =
        | Number of uint64
        | CompoundExpression of Expression array * Operator array

    let private emptyExpression = CompoundExpression (Array.empty, Array.empty)

    type private ExpressionBuilder = {
        Expression : Expression
        Input : char array
    }

    let private evaluateOperation evaluationFunc left right = function
        | Add -> (evaluationFunc left) + (evaluationFunc right)
        | Multiply -> (evaluationFunc left) * (evaluationFunc right)

    let rec private evaluateLeftToRight = function
        | Number n -> uint64 n
        | CompoundExpression (expressions, operations) ->
            match (expressions, operations) with
            | [| e |], [| |] -> evaluateLeftToRight e
            | es, os ->
                let leftmostResult = os.[0] |> evaluateOperation evaluateLeftToRight es.[0] es.[1]
                let newEs = Array.append [| Number leftmostResult |] es.[2..]
                evaluateLeftToRight (CompoundExpression (newEs, os.[1..]))

    let rec private evaluateOperationsWithPrecedence numbers operators =
        match operators |> Array.tryFindIndex ((=) Add) with
        | None -> numbers |> Array.fold (*) 1UL
        | Some i ->
            let sum = numbers.[i] + numbers.[i + 1]
            let newNumbers = Array.concat [| numbers.[..i - 1]; [| sum |]; numbers.[i + 2..]; |]
            let newOperators = operators |> Array.indexed |> Array.filter (fun (j, _) -> j <> i) |> Array.map snd
            evaluateOperationsWithPrecedence newNumbers newOperators

    let rec private evaluateAdditionThenMultiplication = function
        | Number n -> n
        | CompoundExpression (expressions, operators) ->
            let numbers = expressions |> Array.map (fun e ->
                match e with
                | Number n -> n
                | CompoundExpression (es, os) -> (es, os) |> CompoundExpression |> evaluateAdditionThenMultiplication
            )
            evaluateOperationsWithPrecedence numbers operators

    let private appendSubexpression subexpression expression =
        match expression with
        | CompoundExpression (subexpressions, operators) ->
            CompoundExpression (Array.append subexpressions [| subexpression |], operators)
        | Number n -> failwith "Cannot append a subexpression to a number"

    let private appendOperator operator expression =
        match expression with
        | CompoundExpression (subexpressions, operators) ->
            CompoundExpression (subexpressions, Array.append operators [| operator |])
        | Number n -> failwith "Cannot append an operator to a number"

    let private addSubexpression (subexpression, builder) =
        {
            Expression = appendSubexpression subexpression builder.Expression;
            Input = builder.Input.[1..]
        }

    let private addOperator (operator, builder) =
        {
            Expression = appendOperator operator builder.Expression;
            Input = builder.Input.[1..]
        }

    let private parseExpression (input : string) : Expression =
        let rec impl (builder : ExpressionBuilder) : ExpressionBuilder =
            match builder.Input |> Array.tryHead with
            | None -> builder
            | Some (' ') -> impl { builder with Input = builder.Input.[1..] }
            | Some ('0') -> (Number 0UL, builder) |> addSubexpression |> impl
            | Some ('1') -> (Number 1UL, builder) |> addSubexpression |> impl
            | Some ('2') -> (Number 2UL, builder) |> addSubexpression |> impl
            | Some ('3') -> (Number 3UL, builder) |> addSubexpression |> impl
            | Some ('4') -> (Number 4UL, builder) |> addSubexpression |> impl
            | Some ('5') -> (Number 5UL, builder) |> addSubexpression |> impl
            | Some ('6') -> (Number 6UL, builder) |> addSubexpression |> impl
            | Some ('7') -> (Number 7UL, builder) |> addSubexpression |> impl
            | Some ('8') -> (Number 8UL, builder) |> addSubexpression |> impl
            | Some ('9') -> (Number 9UL, builder) |> addSubexpression |> impl
            | Some ('+') -> (Add, builder) |> addOperator |> impl
            | Some ('*') -> (Multiply, builder) |> addOperator |> impl
            | Some (')') -> { builder with Input = builder.Input.[1..] }
            | Some ('(') ->
                let subBuilder = impl { Expression = emptyExpression; Input = builder.Input.[1..] }
                { subBuilder with Expression = appendSubexpression subBuilder.Expression builder.Expression } |> impl
            | Some (c) -> failwithf "Invalid character '%c' in input string" c
        let result = impl { Expression = emptyExpression; Input = input.ToCharArray() }
        result.Expression

    let private parse = Array.map parseExpression

    let solve input =
        let expressions = parse input
        let partOne = expressions |> Array.sumBy evaluateLeftToRight
        let partTwo = expressions |> Array.sumBy evaluateAdditionThenMultiplication
        string partOne, string partTwo
