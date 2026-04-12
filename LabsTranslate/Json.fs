module LabsTranslate.Json
open System.Text.Json
open System.Text.Json.Serialization
open Frontend
open LabsCore.ExprTypes
open LabsCore.Grammar

let writeOp (writer:Utf8JsonWriter) (options:JsonSerializerOptions) (op: string) terms =
    JsonSerializer.Serialize({| Op = op; Terms = terms |}, options)
    |> writer.WriteRawValue

type WriteOnlyConverter<'a>() =
    inherit JsonConverter<'a>()
    override this.Read(reader, _, _) = failwith "Not Implemented"
    override this.Write(_, _, _) = failwith "Not Implemented"

type StmtConverter<'a>() =
    inherit WriteOnlyConverter<Stmt<'a>>()
    override this.Write(writer, value, options) =
        match value with
        | Act a -> JsonSerializer.Serialize(a, options) |> writer.WriteRawValue
        | Block actions ->
            List.map Act actions |> fun x -> JsonSerializer.Serialize(x, options) |> writer.WriteRawValue
        | Name name -> writer.WriteStringValue name
        | Nil -> writer.WriteStringValue "Nil"
        | Skip -> writer.WriteStringValue "Skip"

type NodeStmtConverter(table: SymbolTable) =
    inherit WriteOnlyConverter<Node<Stmt<Var<int>*int>>>()
    override this.Write(writer, value, options) =
        let stmt =
            // Small trick so that "Stmt" is a list even with single assignments
            match value.Def with
            | Act a -> Block [a]
            | x -> x
        {|Name = value.Name; Pos = value.Pos; Guard = table.Guards.TryFind(value); Stmt = stmt|}
        |> fun x -> JsonSerializer.Serialize (x, options) |> writer.WriteRawValue 

type ProcessConverter<'a>() =
    inherit WriteOnlyConverter<Process<'a>>()
    override this.Write(writer, value, options) =
        let myWriteOp = writeOp writer options
        match value with
        | Comp(composition, processes) -> myWriteOp (string composition) processes
        | BaseProcess(node) ->
            JsonSerializer.Serialize(node, options) |> writer.WriteRawValue
        | _ -> writer.WriteNullValue()
            

type VarTypeConverter() =
    inherit WriteOnlyConverter<VarType<int>>()
    override this.Write(writer, value, options) =
        match value with
        | C1Ref -> writer.WriteStringValue "c1"
        | C2Ref -> writer.WriteStringValue "c2"
        | Scalar -> writer.WriteNumberValue(0)
        | Array dims -> JsonSerializer.Serialize(dims, options) |> writer.WriteRawValue

type BExprConverter<'a, 'b>() =
    inherit WriteOnlyConverter<BExpr<'a, 'b>>()
    override this.Write(writer, value, options) =
        let myWriteOp1 = writeOp writer options
        let myWriteOp2 = writeOp writer options
        match value with
        | BLeaf x -> writer.WriteBooleanValue x
        | Neg (BLeaf x) -> writer.WriteBooleanValue <| not x
        | Neg b -> myWriteOp1 "not" [b]
        | Compare (e1, op, e2) -> myWriteOp2 (string op) [e1; e2]
        | Compound(bop, bExprs) -> myWriteOp1 (string bop) bExprs
        | _ -> writer.WriteNullValue()

type ExprConverter<'a, 'b>() =
    inherit WriteOnlyConverter<Expr<'a, 'b>>()
    override this.Write(writer, value, options) =
        let myWriteOp = writeOp writer options
        match value with
        | Leaf (Id _) -> writer.WriteStringValue("id")
        | Leaf (Const n) -> writer.WriteNumberValue(n)
        | Leaf (Extern x) -> writer.WriteStringValue(string x)
        | Arithm (e1, op, e2) -> myWriteOp (string op) [e1; e2]
        | Unary(UnaryMinus, Leaf (Const n)) -> writer.WriteNumberValue(-n)
        | Unary(Abs, Leaf (Const n)) -> writer.WriteNumberValue(abs(n))
        | Unary(op, x) -> myWriteOp (string op) [x]
        | Nondet(e1, e2, _) -> myWriteOp "Nondet" [e1; e2]
        | Ref x -> JsonSerializer.Serialize(x, options) |> writer.WriteRawValue
        | Count(typ, name, bExpr) ->
            {| Count = typ; Name = name; Pred = bExpr |}
            |> fun x -> JsonSerializer.Serialize(x, options) |> writer.WriteRawValue
        | QB(stringMap, expr) ->
            Map.toSeq stringMap
            |> (Seq.map <| fun (var, (typ, quant)) -> [string quant; typ; var])
            |> fun s -> {| Quantifiers = s; Pred = expr |}
            |> fun x -> JsonSerializer.Serialize(x, options) |> writer.WriteRawValue
        | IfElse(bExpr, ifTrue, ifFalse) ->
            {| IfElse = bExpr; IfTrue = ifTrue; IfFalse = ifFalse |}
            |> fun x -> JsonSerializer.Serialize(x, options) |> writer.WriteRawValue
        | RawCall(name, exprs) ->
            {| RawCall = name; Terms = exprs |}
            |> fun x -> JsonSerializer.Serialize(x, options) |> writer.WriteRawValue
type ConvertToString<'a>() =
    inherit WriteOnlyConverter<'a>()
    override this.Write(writer, value, _) = writer.WriteStringValue(string value)