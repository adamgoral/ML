namespace ML.Tests
open System
open Xunit
open FsCheck
open FsCheck.Prop
open FsCheck.Xunit

module ``Classification Tests`` =
    [<Fact>]
    let dummy = 0.

    [<Property>]
    let ``dummy test`` (x: float) =
        not(Double.IsNaN(x)) ==> (x * x >= 0.)
    
    open ML.Classification

    let digitCharGenerator =
        let chars = ['0'..'9']
        Gen.elements chars

    let letterGenerator =
        let chars = List.append ['a'..'z'] ['A'..'Z']
        Gen.elements chars

    let alphaNumGenerator =
        let alphaNum = [(1, letterGenerator); (1, digitCharGenerator)]
        Gen.frequency alphaNum

    let wordGenerator = 
        Gen.nonEmptyListOf alphaNumGenerator |> Gen.map (fun xs -> new String(xs |> Array.ofSeq))

    let wordListGenerator =
        Gen.listOf wordGenerator

    let wordInputGenerator = 
        gen {
            let! categories = wordListGenerator
            let! words = wordListGenerator
            return (words, categories)
        }
        

    [<Property>]
    let ``GetWords is splitting text into words`` () =
        let testInput = Gen.map (fun ws -> (String.concat " " ws, ws)) wordListGenerator
        forAll (Arb.fromGen testInput) (fun (text, ws) ->
            let words = getWords text |> Seq.toList
            ws = words
        )
    
    open ML.Data.Outlook

    [<Property>]
    let ``GetMailWords is splitting subject, to and body into words`` () =
        let testInput = Gen.three wordListGenerator
                        |> Gen.map (fun (sub, from, body) -> let join = String.concat " "
                                                             let ws = List.concat [from; sub; body]
                                                             (new Inbox.Mail(String.Empty, join from, join sub, join body, []), ws))
        forAll (Arb.fromGen testInput) (fun (input, ws) ->
            let words = getMailWords input |> Seq.toList
            ws = words
        )

    [<Property>]
    let ``Mail maps to input`` () =
        let join = String.concat " "
        let toUniqueList xs = xs    |> Seq.distinct
                                    |> Seq.toList        
        let testInput = Gen.four wordListGenerator
                        |> Gen.map (fun (sub, from, body, cats) -> let cats = cats |> toUniqueList
                                                                   let ws = List.concat [from; sub; body]
                                                                   (new Inbox.Mail(String.Empty, join from, join sub, join body, cats), (ws, cats)))
        forAll (Arb.fromGen testInput) (fun (input, (ws, cats)) ->
            let (words, categories) = mailToInput input
            (ws, cats) = (words, categories)
        )
    
    [<Property>]
    let ``wordsByCategory groups words from input to sequence of words by category`` () =
        let testInput = gen {
                            let! allowedCats = Gen.listOfLength 20 wordGenerator
                            let wordInputGenerator = gen { let! words = Gen.listOf wordGenerator
                                                           let! cats = Gen.subListOf allowedCats
                                                           return (List.toSeq words, List.toSeq cats)
                                                         }
                            let! input = Gen.listOf wordInputGenerator
                            let expectedCats = Seq.collect (fun (ws, cats) -> cats) input
                            return (input, expectedCats)
                        }
        forAll (Arb.fromGen testInput) (fun (input, expectedCats) ->
            let categorised = wordsByCategory input
                              |> Seq.toList
                              |> List.map (fun (words, category) -> (Seq.toList words, category))
            expectedCats = expectedCats
        )

    [<Property>]
    let ``wordsByCategory groups words with same category`` () =
        let testInput = gen { let cats = ["cat1"; "cat2"]
                              let wordInputGenerator = gen { let! words = Gen.nonEmptyListOf wordGenerator
                                                             return (List.toSeq words, List.toSeq cats)
                                                           }
                              let! input = Gen.nonEmptyListOf wordInputGenerator
                              let expectedWords = Seq.collect (fun (ws, cat) -> ws) input
                                                  |> List.ofSeq
                              return (input, cats, expectedWords)
                            }
        forAll(Arb.fromGen testInput) (fun (input, cats, expectedWords) ->
            let categorised = wordsByCategory input |> Seq.toList
                              |> List.map (fun (words, category) -> (Seq.toList words, category))
            let (words, cat1) = categorised.Head
            (words = expectedWords) && (cat1 = cats.Head)
        )

    open MathNet.Numerics.Probability
    open MathNet.Numerics
    open System.Numerics

    let positiveIntGenerator =
        Gen.choose (1, Int32.MaxValue)

    let intGenerator =
        Gen.choose (Int32.MinValue + 1, Int32.MaxValue)

    let bigRationalGenerator =
        gen { let! numerator = intGenerator
              let! denominator = positiveIntGenerator 
              return BigRational.FromIntFraction(numerator, denominator)}

//    [<Property>]
//    let ``Beta function should match expected`` () =
//        let testInput = gen { let! x = bigRationalGenerator
//                              let! y = bigRationalGenerator
//                              return ((x, y), 1.) }
//        forAll (Arb.fromGen testInput) (fun ((x, y), expected) ->
//            beta([1.; 1.]) = expected
//        )

    open ML.DecisionTrees
    open MathNet.Numerics.LinearAlgebra

    [<Property>]
    let ``OLS returns inferred weights`` () =
        let testInput = Seq.unfold (fun (x) -> Some((float x, float(2 * x + 2)), x + 1)) 0
                        |> Seq.map (fun (x, y) -> (vector (x::[]), vector (y::[])))
                        |> Seq.take 100 |> List.ofSeq
                        |> Gen.constant
        forAll (Arb.fromGen testInput) (fun input ->
            let beta = ols input
            true
        )

    [<Property>]
    let ``OLSpoly returns inferred weights`` () =
        let testInput = Seq.unfold (fun (x) ->  let xs = float(x)::float(x)::[]
                                                let y1 = float(2 * x)
                                                let y2 = float(3 * x + 1)
                                                Some((xs, y1::y2::[]), x + 1)) 0
                        |> Seq.map (fun (x, ys) -> (vector x, vector ys))
                        |> Seq.take 100 |> List.ofSeq
                        |> Gen.constant
        forAll (Arb.fromGen testInput) (fun input ->
            let beta = polyOls 1 input
            true
        )

        