namespace ML
module DecisionTrees =
    open Deedle

    let dataDir = "..\..\..\Work\Experiments\ML\Data\\"

    let loadTitanic () =
        Frame.ReadCsv (dataDir + "Titanic\\train.csv")
        |> Frame.indexRowsInt "PassengerId"
    
    type Node<'Tkey> =
        | Leaf
        | GreaterThan of 'Tkey * float * Node<'Tkey> * Node<'Tkey>
        | EqualTo of 'Tkey * int * Node<'Tkey> * Node<'Tkey>

//    let growTree features classes =
//        
    open MathNet.Numerics.LinearAlgebra

    let trans = Matrix.transpose
    let inv = Matrix.inverse
    
    module DenseMatrix =
        let ofVectorSeq xs = List.ofSeq xs |> DenseMatrix.ofRows

    let ols observations =
        let X = Seq.map fst observations |> DenseMatrix.ofVectorSeq
        let Y = Seq.map snd observations |> DenseMatrix.ofVectorSeq
        let Xtrans = trans X
        inv(Xtrans * X) * Xtrans * Y

    let rec polyList (xs: float list) order =
        if order < 0 then []
        else let front = polyList xs (order - 1)
             let tail = List.map (fun x -> pown x order) xs
             front @ tail

    let polyOls order observations =
        let observations = seq { for (x, y) in observations do
                                  let xs = Vector.toList x
                                  yield (polyList xs order |> vector, y)}
                           |> List.ofSeq
        ols observations

    let rec insert xs x =
        match xs with
        | [] -> x::[]
        | h::tail -> if h < x then x::h::tail
                     else h::x::tail

    let withinBoundary x y matrix =
        (Array2D.base1 matrix <= x) && (Array2D.base2 matrix <= y) && (Array2D.length1 matrix > x) && (Array2D.length2 matrix > y)

    let rec markIsland x y matrix id =
        if withinBoundary x y matrix && matrix.[x, y] = 1 then
            matrix.[x , y] <- !id
            markIsland (x-1) (y-1) matrix id |> ignore
            markIsland (x-1) (y+1) matrix id |> ignore
            markIsland (x+1) (y-1) matrix id |> ignore
            markIsland (x+1) (y+1) matrix id |> ignore
            !id + 1
        else !id

    let countIslands matrix =
        let id = ref 2
        Array2D.iteri (fun x y _ -> id := (markIsland x y matrix id)) matrix
        !id - 2