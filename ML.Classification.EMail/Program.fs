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

let logSumExp (xs: float seq) =
    let logXs = Seq.map log xs
    let result = Seq.sum logXs
    exp result

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

open ML.Data.Outlook
open System.Text.RegularExpressions

let unknownWord = "[unknown]"

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

[<EntryPoint>]
let main argv = 
    let processedMail = Inbox.GetProcessedMail() |> Seq.map mailToInput |> Seq.toList
    let bpm = ClassificationBPM().Observe(processedMail)
    let unprocessedEmail = Inbox.GetUnprocessedMail() |> Seq.map mailToInput |> Seq.map fst
    let categorised = unprocessedEmail |> Seq.map (bpm.InferLabel) |> Seq.toList
    //let categoriser = mailToInput >> fst >> (bpm.InferLabel)
    //Inbox.CategoriseAllMail categoriser
    printfn "%A" argv
    0 // return an integer exit code
