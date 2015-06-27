namespace ML
module Data =
    type wordCat = {word: string; category: string}
//    let mapValuesToWordCounts (xs: Series<'a, string seq>) = Series.mapValues (fun (vs: string seq) -> Seq.countBy id vs |> series) xs

//    type MailSimplified = {id: string; words: string list; category: string}
//    let testFrame () =
//        let records = [{words = ["abc";"def";"ghi"]; categories = ["cat1"; "cat2"]}
//                       {words = ["abc";"def";"def";"jkl";"jkl"]; categories = ["cat1"; "cat3"]}
//                       {words = ["ghi"]; categories = ["cat2"]}]
//        let data = seq { for r in records do
//                            for c in r.categories do
//                                yield { id = r.id; words = r.words; category = c} }
//                   |> Frame.ofRecords
//        Frame.aggregateRowsBy ["category"] ["id"] (fun ser -> Series.countKeys ser) data
//        data.GetColumn<string list>("words") |> Series.groupBy (fun k v -> v)
//        let data = seq { for r in records do
//                            for c in r.categories do
//                                for w in r.words do
//                                    yield { word = w; category = c} }
//                   |> Frame.ofRecords
//                   |> Frame.pivotTable (fun r c -> c.GetAs<string>("word")) (fun r c -> c.GetAs<string>("category")) Frame.countRows
    
    //    let dummyCol = series ["id1" => series["abc" => 1; "def" => 1]]
    
    //    data.ReplaceColumn ("words", (data.GetColumn "words" |> mapValuesToWordCounts))
    //    let data = data.ExpandColumns ["words"]
    //               |> Frame.fillMissingWith 0
    //               |> Frame.transpose
//        data

    open ML
    open ML.Data.Outlook
    open Deedle

    type Mail = {words: (string * int) list; categories: string list}

    let testLoadFromInbox () =
        let samples = Inbox.GetProcessedMailFolders() |> Seq.collect (fun f -> Seq.truncate 10 f.Mail)
        let trainingSet = samples |> Seq.map Classification.mailToInput
                          |> Seq.map (fun (ws, cs) -> {words=((Seq.countBy (fun v -> v) ws) |> Seq.toList); categories=cs})
        let sourceFrame = Frame.ofRecords trainingSet
        
        let words = sourceFrame.GetColumn<(string * int) list>("words")
        Series.mapValues series words |> Frame.ofRows |> Frame.fillMissingWith 0
//        let trainingFrame = seq { for r in trainingSet do
//                                    for c in r.categories do
//                                        for w in r.words do
//                                            yield { word = w; category = c} }
//                            |> Frame.ofRecords
//                            |> Frame.pivotTable (fun r c -> c.GetAs<string>("word")) (fun r c -> c.GetAs<string>("category")) Frame.countRows
//                            |> Frame.fillMissingWith 0

    //    let trainingFrame = Seq.mapi (fun i (id, subject, ws, cs) -> {id = i.ToString(); words=ws; categories=cs}) trainingSet
    //                        |> Frame.ofRecords
    //                        |> Frame.indexRowsString "id"
    //    trainingFrame.ReplaceColumn ("words", (trainingFrame.GetColumn "words" |> mapValuesToWordCounts))
    //    trainingFrame.ReplaceColumn ("categories", (trainingFrame.GetColumn "categories" |> mapValuesToWordCounts))
    //    let trainingFrame = Frame.expandCols ["words"; "categories"] trainingFrame
    //                        |> Frame.fillMissingWith 0
//        trainingFrame.Print()
    open MathNet.Numerics

    type ConstantExpression =
        | Pi
        | Exp

    type NumericExpression =
        | Num of bigint
        | Const of ConstantExpression
        | Exp of NumericExpression
        | Pow of NumericExpression * NumericExpression
        | Sum of NumericExpression list
        | Mul of NumericExpression list
    
    open System.Numerics

    let bi (x: int) =
        BigInteger(x)

    let biexp (x: int) =
        Num (bi x)

    let sum a b =
        match (a, b) with
        | (Num a, Num b) -> Num(a + b)
        | (a, b) when a = b -> Mul(biexp 2::a::[])
        | (Sum a, b) -> Sum (b::a)
        | (a, Sum b) -> Sum (a::b)
        | (a, b) -> Sum (a::b::[])

    let mul a b =
        match (a, b) with
        | (Num a, Num b) -> Num(a * b)
        | (a, b) when a = b -> Pow(a, biexp 2)
        | (Pow(a, x), Pow(b, y)) when a = b -> Pow(a, sum x y)
        | (Mul a, b) -> Mul (b::a)
        | (a, Mul b) -> Mul (a::b)
        | (a, b) -> Mul (a::b::[])
        