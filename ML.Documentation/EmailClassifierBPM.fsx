(*** hide ***)
//#I ".."
#load "ML.Documentation.fsx"
#load @"..\ML.Data.Outlook\bin\debug\ML.Data.Outlook.dll"
//#r "ML.dll"

(**
Email classifier using Bernoulli Product Model (BPM)
================

In previous post I have provided detailed description for BPM.

Now is the time to put it so some use.

The idea is to assign most likely category to each e-mail based on model trained using existing Outlook folder structure.

As an example:

Inbox (120 messages)

    Invoices (10 messages)

    Subscriptions (13 messages)

        News (40 messages)

        Google (15 messages)

Junk (1000 messages)


In above setup, all messages in news folder are considered to have both "News" and "Subscriptions" categories.

I will be using helper library ML.Data.Outlook to interact with Outlook. Source can be found here: ML.Data.Outlook.zip
*)

open ML.Data.Outlook
open System.Text.RegularExpressions

let getWords text =
    if text = null then Seq.empty
    else
        let wordRegEx = new Regex(@"\w+")
        seq { for m in wordRegEx.Matches(text) do
                yield m }
            |> Seq.map (fun m -> m.Value)
    
let getMailWords (mail : Inbox.Mail) =
    let words = getWords mail.Body       
                |> Seq.append (getWords mail.Subject)
                |> Seq.append (getWords mail.From)
    words

let mailToInput (mail : Inbox.Mail) =
    let words = getMailWords mail    |> Seq.toList
    let categories = mail.Categories |> Seq.distinct
                                     |> Seq.toList
    (words, categories)

(**
Inbox.Mail type already provides categories based on folder structure. I am including words identified in 'from', 'subject' and 'body' properties.


I have expanded the model from previous post to utilise mutual information. It will allow me to only use features with sensibly low entropy.
*)
let predictY (x: seq<'a>) = 
    let rankedFeatures = x |> Seq.filter (fun k -> mmik k > 0.1)
    let fset = Set rankedFeatures
(**
In this case I have hardcoded the filter to have MI > 0.1 for each feature. It should reduce irrelevant features greatly.

Having too many features will result in a loss of numerical stability. (product of large number of low probability values will end up being 0)

To avoid this, log-sum-exp trick can be used:
*)

let logSumExp (xs: float seq) =
    let logXs = Seq.map log xs
    let result = Seq.sum logXs
    exp result

open MathNet.Numerics
open System.Numerics

let findOrDefault (map: Map<_,_>) key defaultValue =
    match map.TryFind key with
    | None -> defaultValue
    | Some value -> value

let rec updateCount (stats: Map<_,_>) data =
    match data with
    | [] -> stats
    | d::ds -> match stats.TryFind d with
                | None -> updateCount (stats.Add(d, 1)) ds
                | Some n -> updateCount (stats.Add(d, n + 1)) ds

open System.Collections.Concurrent

let memoize<'a,'b> f =
    let cache = new ConcurrentDictionary<'a,'b>()
    (fun k -> cache.GetOrAdd(k, fun k -> f k))

type ClassificationBPM<'a, 'b> when 'a: comparison and 'b: comparison (N, Nc, Nk, Nck : Map<_,_>) =
    let Cs = Map.toSeq Nc |> Seq.map fst |> List.ofSeq
    let Ks = Map.toSeq Nk |> Seq.map fst |> List.ofSeq
    let Thetack (c, k) = (float(findOrDefault (Nck.[c]) k 0) + 1.) / (float Nc.[c] + 2.)
    let mThetack = memoize Thetack
    let Alpha0 = Cs.Length |> float
    let Pic c = (float(findOrDefault Nc c 0) + 1.) / (float N + Alpha0)
    let mPic = memoize Pic
    let Thetak k = (float(findOrDefault Nk k 0) + 1.) / (float N + 2.)
    let mThetak = memoize Thetak
    let mi (c, k) =
        let thetaCk = mThetack (c, k) |> float
        let thetak = mThetak k |> float
        let pic = mPic c |> float
        pic * (thetaCk * (log(thetaCk/thetak)) + (1. - thetaCk) * (log((1.-thetaCk)/(1.-thetak))))
    let mmi = memoize mi
    let mik k =
        Cs |> List.map (fun c -> mmi(c, k)) |> List.sum
    let mmik = memoize mik
    let predictY (x: seq<'a>) = 
        let rankedFeatures = x |> Seq.filter (fun k -> mmik k > 0.1)
        let fset = Set rankedFeatures
        let calcBeta c k = if (fset.Contains(k)) then mThetack (c, k) |> float
                           else (1. - (mThetack (c, k) |> float))
        let ys = Cs |> Seq.map (fun c -> (c, (mPic c) * (Ks |> Seq.map (fun k -> calcBeta c k) |> logSumExp)))
                    |> Seq.sortBy snd
        Seq.last ys |> fst
    new() = ClassificationBPM(0, Map.empty<_,_>, Map.empty<_,_>, Map.empty<_,_>)
    member this.Observe(data: List<_>) =
        let Nc = updateCount Nc (Seq.collect snd data |> Seq.toList)
        let Nk = updateCount Nk (Seq.collect fst data |> Seq.toList)
        let unfolded = Seq.collect (fun (fs, cs) -> let fslst = Seq.toList fs
                                                    Seq.map (fun c -> (fslst, c)) cs) data
        let Nck = Seq.groupBy snd unfolded
                    |> Seq.map (fun (key, xs) -> let ds = Seq.map fst xs
                                                 let stats = Seq.fold (fun state ds -> updateCount state ds) Map.empty ds
                                                 (key, stats))
                    |> Map.ofSeq
        new ClassificationBPM<_, _>(N + data.Length, Nc, Nk, Nck)
    member this.InferLabel(features) = predictY features

(**
Training it becomes simple matter of:
*)

let processedMail = Inbox.GetProcessedMail() |> Seq.take 100 |> Seq.map mailToInput |> Seq.toList
let bpm = ClassificationBPM().Observe(processedMail)

(**
Using defined classifier and utility function, all e-mails can now be assigned most likely category based on above observations.
(OVERWRITING any categories already assigned!!!)
*)
let categoriser = mailToInput >> fst >> (bpm.InferLabel)
//Inbox.CategoriseAllMail categoriser
