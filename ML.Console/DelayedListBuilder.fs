namespace ML.Console
module DelayedListBuilder =
    type ListBuilder() =
        member this.Bind(m, f) = 
            m |> Seq.collect f

        member this.Zero() = 
            printfn "Zero"
            Seq.empty
        
        member this.Yield(x) = 
            printfn "Yield an unwrapped %A as a sequence" x
            Seq.singleton x

        member this.YieldFrom(m) = 
            printfn "Yield a list (%A) directly" m
            m

        member this.For(m,f) =
            printfn "For %A" m
            this.Bind(m,f)
        
        member this.Combine (a,f) = 
            printfn "combining %A and delayed yields" a
            let result = seq {
                yield! a
                yield! f()
            }
            result

        member this.Delay(f) = 
            printfn "Delay"
            (fun _ -> 
                printfn "running delayed function"
                f())

    // make an instance of the workflow                
    let listbuilder = new ListBuilder()
    (listbuilder { 
        yield 1
        yield 2
        yield 3
        yield 4
        for x = 0 to 10 do
            yield x
        })() |> Seq.take 12 |> List.ofSeq |> printfn "Result for yield then yield: %A" 

    type State<'a, 's> = State of ('s -> 'a * 's)

    let runState (State s) a = s a
    let getState = State (fun s -> (s,s))
    let putState s = State (fun _ -> ((),s))

    type StateBuilder() =
        member this.Return(a) = 
            State (fun s -> (a,s))
        member this.Bind(m,k) =
            State (fun s -> 
                let (a,s') = runState m s 
                runState (k a) s')
        member this.ReturnFrom (m) = m

    let state = new StateBuilder()

    let DoSomething counter = 
        printfn "DoSomething. Counter=%i " counter
        counter + 1

    let FinalResult counter = 
        printfn "FinalResult. Counter=%i " counter
        counter

    // convert old functions to "state-aware" functions
    let lift f = state {
        let! s = getState 
        return! putState (f s)
        }

    // new functions
    let DoSomething' = lift DoSomething
    let FinalResult' = lift FinalResult

    let counterWorkflow = 
        let s = state {
            do! DoSomething'
            do! DoSomething'
            do! DoSomething'
            do! FinalResult'
            } 
        runState s 0

    /// Represents an update monad - given a state, produce 
    /// value and an update that can be applied to the state
    type UpdateMonad<'TState, 'TUpdate, 'T> = 
      UM of ('TState -> 'TUpdate * 'T)

    /// Returns the value of 'Unit' property on the ^S type
    let inline unit< ^S when ^S : 
        (static member Unit : ^S)> () : ^S =
      (^S : (static member Unit : ^S) ()) 

    /// Invokes Combine operation on a pair of ^S values
    let inline (++)< ^S when ^S : 
        (static member Combine : ^S * ^S -> ^S )> a b : ^S =
      (^S : (static member Combine : ^S * ^S -> ^S) (a, b)) 

    /// Invokes Apply operation on state and update ^S * ^U
    let inline apply< ^S, ^U when ^U : 
        (static member Apply : ^S * ^U -> ^S )> s a : ^S =
      (^U : (static member Apply : ^S * ^U -> ^S) (s, a)) 

    type UpdateBuilder() = 
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

    /// Instance of the computation builder
    /// that defines the update { .. } block
    let update = UpdateBuilder()

    /// The state of the reader is 'int'
    type ReaderState = int
    /// Trivial monoid of updates 
    type ReaderUpdate = 
      | NoUpdate
      static member Unit = NoUpdate
      static member Combine(NoUpdate, NoUpdate) = NoUpdate
      static member Apply(s, NoUpdate) = s

    /// Read the current state (int) and return it as 'int'
    let read = UM (fun (s:ReaderState) -> 
      (NoUpdate, s))
    /// Run computation and return the result 
    let readRun (s:ReaderState) (UM f) = f s |> snd

    /// Returns state + 1
    let demo1 = update { 
      let! v = read
      return v + 1 }
    /// Returns the result of demo1 + 1
    let demo2 = update { 
      let! v = demo1
      return v + 1 }

    // Run it with state 40 
    let d2Result = demo2 |> readRun 40

    /// Writer monad has no readable state
    type WriterState = NoState

    /// Updates of writer monad form a list
    type WriterUpdate<'TLog> = 
      | Log of list<'TLog>
      /// Returns the empty log (monoid unit)
      static member Unit = Log []
      /// Combines two logs (operation of the monoid)
      static member Combine(Log a, Log b) = Log(List.append a b)
      /// Applying updates to state does not affect the state
      static member Apply(NoState, _) = NoState

    /// Writes the specified value to the log 
    let write v = UM (fun s -> (Log [v], ()))
    /// Runs a "writer monad computation" and returns 
    /// the log, together with the final result
    let writeRun (UM f) = let (Log l, v) = f NoState in l, v

    /// Writes '20' to the log and returns "world"
    let demo3 = update {
      do! write 20
      return "world" }
    /// Calls 'demo3' and then writes 10 to the log
    let demo4 = update {
      let! w = demo3
      do! write 10
      return "Hello " + w }

    /// Returns "Hello world" with 20 and 10 in the log
    let d4result = demo4 |> writeRun

    /// Extends UpdateBuilder to support additional syntax
    type UpdateBuilder with
      /// Represents monadic computation that returns unit
      /// (e.g. we can now omit 'else' branch in 'if' computation)
      member inline x.Zero() = x.Return(())

      /// Delays a computation with (uncontrolled) side effects
      member inline x.Delay(f) = x.Bind(x.Zero(), f)

      /// Sequential composition of two computations where the
      /// first one has no result (returns a unit value)
      member inline x.Combine(c1, c2) = x.Bind(c1, fun () -> c2)

      /// Enable the 'return!' keyword to return another computation
      member inline x.ReturnFrom(m : UpdateMonad<'S, 'P, 'T>) = m

      /// Ensure that resource 'r' is disposed of at the end of the
      /// computation specified by the function 'f'
      member inline x.Using(r,f) = UM(fun s -> 
        use rr = r in let (UM g) = f rr in g s)

      /// Support 'for' loop - runs body 'f' for each element in 'sq'
      member inline x.For(sq:seq<'V>, f:'V -> UpdateMonad<'S, 'P, unit>) = 
        let rec loop (en:System.Collections.Generic.IEnumerator<_>) = 
          if en.MoveNext() then x.Bind(f en.Current, fun _ -> loop en)
          else x.Zero()
        x.Using(sq.GetEnumerator(), loop)

      /// Supports 'while' loop - run body 'f' until condition 't' holds
      member inline x.While(t, f:unit -> UpdateMonad<'S, 'P, unit>) =
        let rec loop () = 
          if t() then x.Bind(f(), loop)
          else x.Zero()
        loop()

    /// Logs numbers from 1 to 10
    let logNumbers = update {
      for i in 1 .. 10 do 
        do! write i }
    let logNumbersResult = writeRun logNumbers

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
    let setRun s (UM f) = f (State s) |> snd

    /// Increments the state by one
    let demo5 = update { 
      let! v = get
      do! set (v + 1) }
    /// Call 'demo5' repeatedly in a loop
    /// and then return the final state
    let demo6 = update {
      for i in 1 .. 10 do 
        do! demo5
      return! get }
    // Run the sample with initial state 0
    let d6result = demo6 |> setRun 0



    let st = ()