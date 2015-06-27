namespace ML.Console

module QueryMonadExperiment =
    
    open System
    open System.Collections.Generic

    let memoise (cache: Dictionary<_,_>) f =
        (fun key arg  -> if cache.ContainsKey key then
                            cache.[key]
                          else
                            let value = f arg
                            cache.[key] <- value
                            value)

    let dictionary () = new Dictionary<_,_>()

    let dicClone (dic: Dictionary<_,_>) =
        let result = dictionary()
        Seq.iter (fun (kvp: KeyValuePair<_,_>) -> result.Add(kvp.Key, kvp.Value)) dic
        result

    type QueryState (uniformCache, binomialCache, uniformDrawCache) =
        let mutable varKey = 0;
        let muniform = (memoise uniformCache Samplers.uniform, uniformCache)
        let mbinomial = (memoise binomialCache Samplers.binomial, binomialCache)
        let muniformDraw = (memoise uniformDrawCache Samplers.uniformDraw, uniformDrawCache)
        let mutable logLikelihood = Double.NegativeInfinity
        new () = QueryState(dictionary(), dictionary(), dictionary())
        member this.getNextVarKey() =
            varKey <- varKey + 1
            varKey
        member this.NextQuery() =
            let uniformCache = dicClone (snd muniform)
            let binomialCache = dicClone (snd mbinomial)
            let uniformDrawCache = dicClone (snd muniformDraw)
            if varKey > 0 then do
                let toggleKey = Samplers.uniformDraw 1 (varKey + 1)
                uniformCache.Remove toggleKey |> ignore
                binomialCache.Remove toggleKey |> ignore
                uniformDrawCache.Remove toggleKey |> ignore
            QueryState(uniformCache, binomialCache, uniformDrawCache)
        
        member this.uniform = fst muniform
        member this.binomial = fst mbinomial
        member this.uniformDraw = fst muniformDraw
        member this.updateLogLikelihood l =
            if Double.IsNegativeInfinity logLikelihood then
                logLikelihood <- l
            else
                logLikelihood <- logLikelihood + l
        member this.LogLikelihoodScore () = logLikelihood
        member this.ObserveBinomial p obs =
            let iobs = if obs then 1 else 0
            let logLikelihoodScore = MathNet.Numerics.Distributions.Binomial.PMFLn (p,1, iobs)
            this.updateLogLikelihood logLikelihoodScore
        
    let getNextVarKey (s : QueryState) = s.getNextVarKey()
    let nextQuery (s: QueryState) =
        s.NextQuery()

    type QueryContext<'v, 'l, 'c> = 
        | Context of ('c -> ('v * 'l * 'c))

    let setContext v l ctx = Context(fun _ -> (v, l, ctx))
    let runWithContext (Context f) ctx = f ctx
    let getContext () = Context(fun c -> (c, c, c))

    type QueryBuilder() =
        member this.Return(v) = 
            Context(fun ctx -> (v, (), ctx))
        member this.Bind(m, f) =
            Context (fun ctx -> let (v, l, ctx') = runWithContext m ctx
                                runWithContext (f v) ctx')
        member this.ReturnFrom (m) = m

    let query = new QueryBuilder()

    let lift f = query {
            let! ctx = getContext()
            let key =  getNextVarKey ctx
            let v, l = f ctx key
            return! setContext v l ctx
        }

    let uniform () = 
        lift (fun ctx key -> (ctx.uniform key (), fun v -> 1.))

    let binomial p =
        lift (fun ctx key -> (ctx.binomial key p, fun v -> ctx.ObserveBinomial p v))

    let uniformDraw min max =
        lift (fun ctx key -> (ctx.uniformDraw key min max, fun v -> 1.))

    let sample q =
        let initialState = QueryState()
        Seq.unfold (fun state -> let v, _, candidateState = runWithContext q (nextQuery state)
                                 let currL = state.LogLikelihoodScore()
                                 let nextL = candidateState.LogLikelihoodScore()
                                 if nextL > nextL then
                                    Some(v, candidateState)
                                 else
                                    Some(v, state)) initialState

    let observe f obs =
        query {
            let! ctx = getContext()
            let v, l, ctx' = runWithContext f ctx
            do l(obs)
            return! setContext () () ctx
        }

    let q1 =    query { let! p = uniform()

                        let x = binomial p
                        do! observe x false
                        do! observe x false
                        do! observe x false
                        do! observe x false
                        do! observe x false
                        do! observe x false
                        return p }

    let q1Sampler () = sample q1 |> Seq.skip 100 |> Seq.take 1000 |> List.ofSeq
