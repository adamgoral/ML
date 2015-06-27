namespace ML.Console
module Sampler =
    
    type UpdateMonad<'TState, 'TUpdate, 'T> = UM of ('TState -> 'TUpdate * 'T)

    let inline unit< ^S when ^S : (static member Unit : ^S)> () : ^S =
        (^S : (static member Unit : ^S) ()) 

    let inline (++)< ^S when ^S : (static member Combine : ^S * ^S -> ^S )> a b : ^S =
        (^S : (static member Combine : ^S * ^S -> ^S) (a, b)) 

    let inline apply< ^S, ^U when ^U : (static member Apply : ^S * ^U -> ^S )> s a : ^S =
        (^U : (static member Apply : ^S * ^U -> ^S) (s, a)) 

    type SamplerBuilder() = 
      /// Returns the specified value, together
      /// with empty update obtained using 'unit'
      member inline x.Return(v) : UpdateMonad<'S, 'U, 'T> = 
        UM (fun s -> (unit(),v))

      /// Compose two update monad computations
      member inline x.Bind(UM u1, f:'T -> UpdateMonad<'S, 'U, 'R>) =  
        UM (fun s -> 
          // Run the first computation to get first update
          // 'u1', then run 'f' to get second computation
          let (u1, x) = u1 s
          let (UM u2) = f x
          // Apply 'u1' to original state & run second computation
          // then return result with combined state updates
          let (u2, y) = u2 (apply s u1)
          (u1 ++ u2, y))

//      member inline x.Combine(c1, c2) = x.Bind(c1, fun () -> c2)

      [<CustomOperation("observe", MaintainsVariableSpaceUsingBind = true)>]
      member inline x.Observe(vars, [<ProjectionParameter>] body) =
        UM(fun s -> (s, body(vars)))

      
    type Sample =
        | Real of float
        | Bool of bool

    open System

    [<AbstractClass>]
    type DistributionContext() =
        let rnd' = Random()
        member this.rnd with get() = rnd'
        abstract sample : string * (unit -> Sample) -> Sample 
        abstract observe : Sample * float -> unit
        abstract Likelihood : float with get
        abstract yieldPrevious : unit -> bool

    type MHAlgorithm(varCache : Map<string, Sample>, likelihood) =
        inherit DistributionContext()
        let mutable originalLikelihood = likelihood
        let mutable currLikelihood = Double.NegativeInfinity
        let mutable originalCache = varCache
        let mutable cache = varCache
        new() = MHAlgorithm(Map.empty, Double.NegativeInfinity)
        override this.Likelihood with get() = currLikelihood
        override this.sample(key, f) =
            match cache.TryFind(key) with
            | Some v -> v
            | None -> let v = f()
                      cache <- cache.Add(key, v)
                      v
        override this.observe(value, likelihood) = 
            if Double.IsNegativeInfinity(currLikelihood) then
                currLikelihood <- likelihood
            else
                currLikelihood <- currLikelihood + likelihood
        override this.yieldPrevious () = 
            if originalLikelihood > currLikelihood then
                cache <- originalCache
                currLikelihood <- Double.NegativeInfinity
                true
            else
                originalLikelihood <- currLikelihood
                currLikelihood <- Double.NegativeInfinity
                let itemCount = cache.Count
                if itemCount > 0 then
                    let itemToRemove = this.rnd.Next(0, itemCount)
                    let keys = cache |> Seq.map (fun kvp -> kvp.Key) |> Seq.toArray;
                    let keyToRemove = keys.[itemToRemove]
                    cache <- cache.Remove keyToRemove
                false

    /// Wraps a state of type 'T
    type StateState<'T> = State of 'T

    /// Represents updates on state of type 'T
    type StateUpdate<'T> = 
      | Set of 'T | SetNop
      /// Empty update - do not change the state
      static member Unit = SetNop
      /// Combine updates - return the latest (rightmost) 'Set' update
      static member Combine(a, b) = 
        match a, b with 
        | SetNop, v | v, SetNop -> v 
        | Set a, Set b -> Set b
      /// Apply update to a state - the 'Set' update changes the state
      static member Apply(s, p) = 
        match p with SetNop -> s | Set s -> State s

    /// Set the state to the specified value
    let set s = UM (fun _ -> (Set s,()))
    /// Get the current state 
    let get = UM (fun (State s) -> (SetNop, s))
    /// Run a computation using a specified initial state
    let run s (UM f) = f (State s) |> snd

    let sampler = new SamplerBuilder()

    type DistributionType =
        | Binomial of float
    
    [<AbstractClass>]
    type Distribution<'v>() =
        abstract Sample : DistributionContext * string -> 'v
        abstract Observe : DistributionContext * 'v -> unit
    
    open MathNet.Numerics.Distributions

    type binomial(p) =
        inherit Distribution<bool>()
        let p = p
        let boolToInt v = if v then 1 else 0
        let intToBool b = if b = 0 then false else true
        override this.Sample(ctx, id) =
            let (Bool v) = ctx.sample(id, fun() -> Bool (intToBool(Binomial.Sample(ctx.rnd, p, 1))))
            v
        override this.Observe(ctx, v) =
            let likelihood = Binomial.PMFLn(p, 1,(boolToInt v))
            ctx.observe(Bool v, likelihood)

    type beta(a, b) =
        inherit Distribution<float>()
        let a, b = a, b
        override this.Sample(ctx, id) =
            let (Real v) = ctx.sample(id, fun() -> Real ( Beta.Sample(ctx.rnd, a, b)))
            v
        override this.Observe(ctx, v) =
            let likelihood = Beta.PDFLn(a, b, v)
            ctx.observe(Real v, likelihood)

    let rvar id (dist: Distribution<'v>) =
        sampler {
            let! ctx = get
            return dist.Sample(ctx, id)
        }

    let betaVar id =
        fun a b ->
            rvar id (beta (a, b))

    let binomialVar id =
        fun p -> rvar id (binomial p)

    let observe (dist: Distribution<'v>) (v:'v) =
        sampler {
            let! ctx = get
            do dist.Observe(ctx, v)
            return ()
        }

    let ctxLikelihood (ctx : DistributionContext) =
        ctx.Likelihood

    let samples (ctx: DistributionContext) sampler =
        Seq.unfold (fun (previous, ctx' : DistributionContext) -> 
            let v = run ctx' sampler
            match (previous, ctx.yieldPrevious()) with
            | (None, _) | (Some _, false) -> Some(v, (Some v, ctx'))
            | (Some pv, _) -> Some(pv, (previous, ctx')) ) (None, ctx)

    let d1 = sampler {
                let! p = betaVar "p" 1. 1.
                let bdist = binomial(p)
                //observe (p <> 0.)
                return p
             }

    let d1result = samples (MHAlgorithm()) d1 |> Seq.take 100 |> Seq.toList
    let stop = ()