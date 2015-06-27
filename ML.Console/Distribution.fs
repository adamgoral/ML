namespace ML.Console
module Distribution =
    open System

    type Sample =
        | Real of float
        | Integer of int
        | Boolean of bool

    type Cache<'k when 'k : comparison> (values : ('k *  'v) seq) =
        let mutable cache = new Map<'k, _>(values)
        member this.GetOrAdd(key: ' k, f: (unit -> 'v)) =
            match cache.TryFind key with
            | Some(v) -> v
            | None -> let v = f()
                      cache <- cache.Add(key, v)
                      v
        member this.AddOrUpdate(key : 'k, v: 'v) =
            match cache.TryFind key with
            | Some(v) -> cache <- cache.Remove(key)
            | _ -> ()
            cache <- cache.Add(key, v)
            v
        member this.Items with get() = Map.toList cache
        member this.Count with get() = cache.Count
        member this.Remove(key) =
            cache <- cache.Remove key

    type DistributionType =
        | Uniform
        | UniformDraw of int * int
        | Binomial of float
        | Beta of float * float
        | Gamma of float * float
        | Gaussian of float * float
    
    [<AbstractClass>]
    type Context() =
        abstract observe : (DistributionType * Sample -> unit)
        abstract yieldPrevious : (unit -> bool)
        abstract uniformSample : (string -> float)
        member this.uniformLikelihood (v) = MathNet.Numerics.Distributions.ContinuousUniform.PDFLn(0.,1.,v)
        abstract binomialSample : (string * float -> bool)
        member this.binomialLikelihood(p, v) = 
            let vint = if v then 1 else 0
            MathNet.Numerics.Distributions.Binomial.PMFLn(p, 1, vint)
        abstract betaSample : (string * float * float -> float)
        member this.betaLikelihood = MathNet.Numerics.Distributions.Beta.PDFLn
        abstract gammaSample : (string * float * float -> float)
        member this.gammaLikelihood = MathNet.Numerics.Distributions.Gamma.PDFLn
        abstract gaussianSample : (string * float * float -> float)
        member this.gaussianLikelihood = MathNet.Numerics.Distributions.Normal.PDFLn
        abstract uniformDrawSample : (string * int * int -> int)
        member this.uniformDrawLikelihood = MathNet.Numerics.Distributions.DiscreteUniform.PMFLn

    type StubContext() =
        inherit Context()
        override this.observe = fun(dist, sample) -> ()
        override this.yieldPrevious = fun() -> false
        override this.uniformSample = fun key -> Samplers.uniform()
        override this.binomialSample = (fun (_, p) -> Samplers.binomial p)
        override this.betaSample = fun (_ , a, b) -> MathNet.Numerics.Distributions.Beta.Sample(a, b)
        override this.gammaSample = fun (_, shape, rate) -> MathNet.Numerics.Distributions.Gamma.Sample(shape, rate)
        override this.gaussianSample = fun (_, mean, stddev) -> MathNet.Numerics.Distributions.Normal.Sample(mean, stddev)
        override this.uniformDrawSample = fun (_, min, max) -> MathNet.Numerics.Distributions.DiscreteUniform.Sample(min, max)

    type MHAlgorithm(cachedSamples, likelihood) =
        inherit Context()
        let originalCache = Cache<string>(cachedSamples)
        let mutable originalLikelihood = likelihood
        let mutable likelihood = Double.NegativeInfinity
        let mutable cache = Cache<string>(cachedSamples)
        new () = MHAlgorithm(Seq.empty, Double.NegativeInfinity)
        override this.uniformSample = fun key -> match cache.GetOrAdd(key, fun () -> Real(Samplers.uniform())) with | Real v -> v
        override this.binomialSample = fun (key, p) -> match cache.GetOrAdd(key, fun () -> Boolean(Samplers.binomial p)) with Boolean v -> v
        override this.betaSample = fun (key , a, b) -> match cache.GetOrAdd(key, fun() -> Real(MathNet.Numerics.Distributions.Beta.Sample(a, b))) with Real v -> v
        override this.gammaSample = fun (key, shape, rate) -> match cache.GetOrAdd(key, fun() -> Real(MathNet.Numerics.Distributions.Gamma.Sample(shape, rate))) with Real v -> v
        override this.gaussianSample = fun (key, mean, stddev) -> match cache.GetOrAdd(key, fun() -> Real(MathNet.Numerics.Distributions.Normal.Sample(mean, stddev))) with Real v -> v
        override this.uniformDrawSample = fun (key, min, max) -> match cache.GetOrAdd(key, fun() -> Integer(MathNet.Numerics.Distributions.DiscreteUniform.Sample(min, max))) with Integer v -> v

        override this.observe = fun (dist, sample) ->
            match (dist, sample) with
            | (Uniform, Real v) -> this.updateLikelihood(this.uniformLikelihood(v))
            | (Binomial p, Boolean v) -> this.updateLikelihood(this.binomialLikelihood(p, v))
            | (Beta(a,b) , Real v) -> this.updateLikelihood(this.betaLikelihood(a, b, v))
            | (Gamma(shape, size), Real v) -> this.updateLikelihood(this.gammaLikelihood(shape, size, v))
            | (Gaussian(mean, stddev), Real v) -> this.updateLikelihood(this.gaussianLikelihood(mean, stddev, v))
        override this.yieldPrevious = (fun () -> 
            if originalLikelihood > likelihood then
                cache <- Cache(originalCache.Items)
                likelihood <- Double.NegativeInfinity
                true
            else
                originalLikelihood <- likelihood
                likelihood <- Double.NegativeInfinity
                let itemCount = cache.Count
                if itemCount > 0 then
                    let itemToRemove = Samplers.uniformDraw 0 itemCount
                    let keys = cache.Items |> List.map fst |> List.toArray;
                    let keyToRemove = keys.[itemToRemove]
                    cache.Remove keyToRemove
                false)
        member this.updateLikelihood(l) =
            if Double.IsNegativeInfinity(likelihood) then
                likelihood <- l
            else
                likelihood <- likelihood + l

    let defaultContext() = MHAlgorithm()

    type Distribution<'v when 'v : equality> = 
        | Distribution of DistributionType * (Context -> 'v) * (Context * 'v -> unit)
    
    type Distribution<'v> with
        member this.observe(ctx : Context, value : 'v) = match this with
                                                         | Distribution (distType, sampler, likelihood) -> likelihood(ctx, value)
        //static member (?=) (dist: Distribution<'v>, value:'v) = dist.observe(ctx, value)
    
    let uniform id =
        let distType = Uniform
        Distribution (distType, (fun ctx -> ctx.uniformSample(id)), (fun (ctx, v) -> ctx.observe(distType, Real v)))

    let binomial id p =
        let distType = Binomial p
        Distribution (distType, (fun ctx -> ctx.binomialSample(id, p)), (fun (ctx, v) -> ctx.observe(distType, Boolean v)))

    let beta id a b =
        let distType = Beta(a, b)
        Distribution (distType, (fun ctx -> ctx.betaSample(id, a, b)), (fun (ctx, v) -> ctx.observe(distType, Real v)))

    let gamma id shape size =
        let distType = Gamma(shape, size)
        Distribution (distType, (fun ctx -> ctx.gammaSample(id, shape, size)), (fun (ctx, v) -> ctx.observe(distType, Real v)))

    let gaussian id mean stddev =
        let distType = Gaussian(mean, stddev)
        Distribution(distType, (fun ctx -> ctx.gaussianSample(id, mean, stddev)), (fun (ctx, v) -> ctx.observe(distType, Real v)))

    let uniformDraw id min max =
        let distType = UniformDraw(min, max)
        Distribution(distType, (fun ctx -> ctx.uniformDrawSample(id, min, max)), (fun (ctx, v) -> ctx.observe(distType, Integer v)))

    type DistributionBuilder<'v>(ctx: Context) =
        let context = ctx
        let mutable previousYield = None
        member this.Zero() =
            Seq.empty
        member this.Bind(m, f) =
            match m with
            | Distribution (distType, sampler, likelihood) -> 
                let v = sampler(context)
                f(v)
        member this.Yield(v : 'v) =
            let result = v
            if context.yieldPrevious() then
                match previousYield with
                | Some pv -> Seq.singleton pv
                | None -> Seq.singleton v
            else
                previousYield <- Some v
                Seq.singleton v
        member this.For(range, f) =
            Seq.collect f range
        member this.While(guard, body) =
            Seq.unfold (fun state -> if state() then Some(body(), state) else None) guard
                |> Seq.concat
        member this.Delay(f) = f
        member this.Run(f) = f()
        member this.Combine(m, f) =
            seq { 
                yield! m
                yield! f()  
            }

    let distribution ctx = new DistributionBuilder<_>(ctx)

    let ctx1 = defaultContext()
    let d1Observations = List.init 100 (fun _ -> Samplers.binomial(0.2))
    let d1 = distribution (ctx1) {
                let pDist = uniform "p"
                while true do
                    let! p = pDist
                    let bDist = binomial "" p
                    for obs in d1Observations do
                        bDist.observe (ctx1, obs)
                    yield p }
    let d1Sample = d1 |> Seq.take 100 |> List.ofSeq
    let d1SAvg = List.average d1Sample
    printfn "d1 p posterior %f" d1SAvg

    let ctx2 = defaultContext()
    let d2Observations = List.init 100 (fun _ -> MathNet.Numerics.Distributions.Beta.Sample(10.,0.5))
    let d2 = distribution (ctx2) {
                let aDist = gamma "a" 1. 10.
                let bDist = gamma "b" 1. 10.
                while true do
                    let! a = aDist
                    let! b = bDist
                    let pDist = beta "p" a b
                    for obs in d2Observations do pDist.observe (ctx2, obs)
                    yield (a, b)}

    let d2Sample = d2 |> Seq.skip 100 |> Seq.take 100 |> List.ofSeq
    let d2SAvga =  d2Sample |> List.averageBy fst
    let d2SAvgb =  d2Sample |> List.averageBy snd
    printfn "d2 a b posterior %f %f" d2SAvga d2SAvgb

    let ctx3 = defaultContext()
    let d3Observations = List.init 1000 (fun _ -> MathNet.Numerics.Distributions.Normal.Sample(2., 5.1))
    let d3 = distribution (ctx3) {
                let meanDist = gaussian "mean" 0. 10.
                let devDist = gamma "stddev" 1. 10.
                while true do
                    let! mean = meanDist
                    let! stdDev = devDist
                    let gDist = gaussian "g" mean stdDev
                    for obs in d3Observations do gDist.observe (ctx3, obs) 
                    yield (mean, stdDev)}
    let d3Sample = d3 |> Seq.skip 100 |> Seq.take 100 |> List.ofSeq
    let d3Smean =  d3Sample |> List.averageBy fst
    let d3SstdDev =  d3Sample |> List.averageBy snd
    printfn "d3 mean stdDev posterior %f %f" d3Smean d3SstdDev

    let ctx4 = defaultContext()
    let d4Observations = List.init 100 (fun i-> (float i, (float i) * 2. + 6. + Samplers.uniform() - 0.5))
    let d4 = distribution ctx4 {
                let interceptDist = gaussian "intercept" 0. 10.
                let slopeDist = gaussian "slope" 0. 10.
                let noiseDist = gamma "noise" 1. 1.
                while true do
                    let! intercept = interceptDist
                    let! slope = slopeDist
                    let! noise = noiseDist
                    let f = (fun x -> x * slope + intercept)
                    for (x, y) in d4Observations do (gaussian "o" (f x) noise).observe(ctx4, y)
                    yield (intercept, slope, noise) }
    let d4Sample = d4 |> Seq.skip 100 |> Seq.take 1000 |> List.ofSeq
    let d4Sintercept =  d4Sample |> List.averageBy (fun (f,s,t) -> f)
    let d4Sslope =  d4Sample |> List.averageBy (fun (f,s,t) -> s)
    let d4Snoise = d4Sample |> List.averageBy (fun (f,s,t) -> t)
    printfn "d4 posteriors intercept=%f slope=%f noise=%f" d4Sintercept  d4Sslope d4Snoise

    let averageByItem s size =
        Array.init size (fun i -> Seq.averageBy (fun item -> Array.get item i) s)

    let ctx5 = defaultContext()
    let d5Observations = List.init 100 (fun i-> (float i, (float i) * (float i) * 3. + (float i) * 2. + 6.))
    let d5 = distribution ctx5 {
                let noDist = uniformDraw "no" 1 10
                let noiseDist = gamma "noise" 1. 1.
                while true do
                    let! no = noDist
                    let! noise = noiseDist
                    let ws = Array.zeroCreate no
                    for n = 0 to (no-1) do
                        let! w = gaussian ("w" + n.ToString()) 0. 10.
                        ws.[n] <- w
                    let f = (fun x -> List.init no (fun n -> (pown x n) * ws.[n]) |> List.sum)
                    for (x, y) in d5Observations do
                        (gaussian "o" (f x) noise).observe(ctx5, y)
                    yield (no, ws) }
    let d5Sample = d5 |> Seq.skip 100 |> Seq.take 1000 |> List.ofSeq
    let d5nAvg = d5Sample |> List.map (fst >> float) |> List.average
    let d5weights = Seq.groupBy fst d5Sample 
                    |> Seq.map (fun (key, items) -> (key, averageByItem (Seq.map snd items) key))
                    |> Map.ofSeq
    printfn "d5 posteriors polynomial order=%f" d5nAvg
    let stop = ()
