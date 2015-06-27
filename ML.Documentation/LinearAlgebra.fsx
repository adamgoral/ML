

// F# interactive initialisation
// #r @"C:\Users\Adam\Work\Experiments\ML\ML.Console\bin\Debug\FSharp.Charting.dll"
// #r @"C:\Users\Adam\Work\Experiments\ML\packages\MathNet.Numerics.2.6.0\lib\net40\MathNet.Numerics.dll"
// #r @"C:\Users\Adam\Work\Experiments\ML\packages\MathNet.Numerics.FSharp.2.6.0\lib\net40\MathNet.Numerics.FSharp.dll"
// #r @"C:\Users\Adam\Work\Experiments\ML\packages\MathNet.Numerics.2.6.0\lib\net40\MathNet.Numerics.IO.dll"

//open System
//open System.Diagnostics.Contracts
//open FSharp.Charting
//open MathNet.Numerics.LinearAlgebra.Double
//open System.Threading.Tasks
//
//type Side =
//     | Heads
//     | Tails
//
//let displayUniformDistr() =
//    let u = uniformPdf 1. 10.
//    let chart = Chart.Line [for x in 1..10 -> (x, u (float x))]
//    chart.ShowChart()
//
//let testFunc = fun x -> Math.Sin (2. * Math.PI * x)
//
//let withNoise scale f =
//    let rand = new System.Random()
//    let sampler () = rand.NextDouble() - 0.5
//    (fun x -> (sampler() * scale) + f x)
//
//let plotLine f range =
//    let chart = Chart.Line [for x in range -> (x, f x)]
//    chart.ShowChart()
//
//let plotList xs ys =
//    let chart = Chart.Line (Seq.zip xs ys)
//    chart.ShowChart()
//
//let plotScatter f range =
//    let chart = Chart.Point [for x in range -> (x, f x)]
//    chart.ShowChart()
//
//let polynomialMatrix order xs =
//    let width = order
//    let length = Array.length xs
//    DenseMatrix.init length width (fun i j -> pown xs.[i] j)  
//
//let estimateWeightsWithLeastSquares (xs : Matrix) (ys : Matrix) =
//    let xst = xs.Transpose()
//    (xst * xs).Inverse() * xst * ys |> DenseMatrix.OfMatrix
//
//let estimateWeightsWithLeastSquaresAndRegression (xs : Matrix) (ys: Matrix) reg=
//    let xst = xs.Transpose()
//    let regMatrix = DenseMatrix.init xs.ColumnCount xs.ColumnCount (fun i j -> if i = j then reg 
//                                                                               else 0.)
//    (xst * xs + regMatrix).Inverse() * xst * ys |> DenseMatrix.OfMatrix
//
//let sumOfSquaredErrors (xs : Matrix) (ys: Matrix) =
//    let diff = xs - ys
//    let prod = diff.Transpose() * diff
//    Matrix.sum(prod)
//
//let getColumns (matrix: Matrix) =
//    Seq.init matrix.ColumnCount (fun i -> matrix.Column(i))
//
//let testLeastSquares () =
//    let xs = [for x in (0.)..(0.1)..(1.) -> x] |> Array.ofList
//    let observed =
//        let temp = Array.map (withNoise 2. testFunc) xs
//        DenseMatrix.init temp.Length 1 (fun i _ -> temp.[i])
//    let poly = polynomialMatrix 5 xs
//
//    let est1 = estimateWeightsWithLeastSquares poly observed
//    let est2 = estimateWeightsWithLeastSquaresAndRegression poly observed 0.01
//
//    let pred1 = poly * est1
//    let pred2 = poly * est2
//    let err1 = sumOfSquaredErrors pred1 observed
//    let err2 = sumOfSquaredErrors pred2 observed
//    plotList xs (getColumns pred1|> Seq.head)
//    plotList xs (getColumns pred2|> Seq.head)

//System.Environment.CurrentDirectory <- "C:\Users\Adam\Work\Experiments\ML"
//#I "..\packages\Deedle.1.0.1"
//#I "..\packages\Deedle.1.0.1\lib\net40"
//#load "Deedle.fsx"

//let flipped = coinFlip 0.5 (always Heads) (always Tails)
//let isHeads = function Heads -> 1. | _ -> 0.
//let expectation = flipped.Expectation(isHeads)
//let sampler = uniformSampler 0. 0.
