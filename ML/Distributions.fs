namespace ML
module Distributions =
    open System

    type ParserResult<'a> =
        | Success of 'a * char list
        | Failure

    type Parser<'a> = char list -> ParserResult<'a>

    let Return (x: 'a): Parser<'a> =
        let p stream = Success(x, stream)
        in p

    type Distribution<'a when 'a : comparison > =
        abstract Sample : 'a
        abstract Support : Set<'a>
        abstract Expectation : ('a -> float) -> float

    let always x =
        { 
        new Distribution<'a> with
            member d.Sample = x
            member d.Support = Set.singleton x
            member d.Expectation(H) = H x
        } 

    let coinFlip p (d1 : Distribution<'a>) (d2 : Distribution<'a>) =
        let rnd = Random()
        if p < 0. || p > 1. then failwith "invalid probability in coinflip"
        {
            new Distribution<'a> with
                member d.Sample = if rnd.NextDouble() < p then d1.Sample else d2.Sample
                member d.Support = Set.union d1.Support d2.Support
                member d.Expectation(H) =
                    p * d1.Expectation(H) + (1.-p) * d2.Expectation(H)
        }

    let uniformPdf a b =
        if a >= b then failwith "a must be less than b"
        fun x ->
            if x < a || x > b then failwith "x must be a <= x <= b"
            1. / (b - a)

    let uniformCdf a b =
        if a >= b then failwith "a must be less than b"
        let integral (x: float) =
            if x < a || x > b then failwith "x must be a <= x <= b"
            x / (b - a)
        fun x -> (integral x) - (integral a)

    let uniformCdfInverse a b =
        if a >= b then failwith "a must be less than b"
        fun (y: float) ->
            if y < 0. || y > 1. then failwith "x must be 0 <= x <= 1"
            (y + (a / (b - a))) * (b - a)

    let uniformSampler a b =
        System.Diagnostics.Contracts.Contract.Requires(a < b)
        if a >= b then failwith "a must be less than b"
        let rand = System.Random()
        let cdfInverse = uniformCdfInverse a b
        fun () -> cdfInverse (rand.NextDouble())

    let (|Positive|_|) x =
        if x > 0. then Some x
        else None

    let pow x y = Math.Pow (x, y)

    open MathNet.Numerics

    let binomialPdf k n =
        if k <= n then
            Some (fun x -> (pow x k) * (pow (1. - x) (n - k)))
        else None

    let betaPdf a b =
        match (a, b) with
        | (Positive(a), Positive(b)) ->
            let betaFunction = SpecialFunctions.Gamma(a) * SpecialFunctions.Gamma(b) / SpecialFunctions.Gamma(a + b)
            let normalizer = 1. / betaFunction
            Some (fun x -> normalizer * (pow x (a - 1.)) * (pow (1. - x) (b - 1.)))
        | _ -> None

    let naiveBayesClassifierLikelihood classprob thetas xs =
        Seq.zip xs thetas |> Seq.map (fun (x, theta) -> classprob x theta) |> Seq.reduce (*)

    let dirichletPdf alphas =
        Some (fun thetas -> Seq.zip thetas alphas |> Seq.map (fun (theta, alpha) -> pow theta alpha) |> Seq.reduce (*))

    let multinomialPdf thetas =
        if Seq.isEmpty thetas then None
        else
            Some (fun xs -> Seq.zip thetas xs |> Seq.map (fun (theta, x) -> pow theta x) |> Seq.reduce (*))