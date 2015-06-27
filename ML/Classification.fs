namespace ML
module Classification =
    open System
    open ML.Data.Outlook
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open System.Linq

    let unknownWord = "[unknown]"

    let getWords text =
        let wordRegEx = new Regex(@"\w+")
        seq { for m in wordRegEx.Matches(text) do
                yield m }
            |> Seq.map (fun m -> m.Value)
    
    let getMailWords (mail : Inbox.Mail) =
        let words = getWords mail.Body       |> Seq.append (getWords mail.Subject)
                                             |> Seq.append (getWords mail.From)
        words

    let mailToInput (mail : Inbox.Mail) =
        let words = getMailWords mail    |> Seq.toList
        let categories = mail.Categories |> Seq.distinct
                                         |> Seq.toList
        (words, categories)

    let wordCount ws =
        Seq.countBy id ws

    let wordsByCategory (xs : (string seq * string seq) seq) =
        let singleCategory = seq { for (words, categories) in xs do
                                    for category in categories do
                                        yield (words, category)}
        let grouped = Seq.groupBy snd singleCategory
                      |> Seq.map (fun (category, xs) -> (Seq.map fst xs, category))
        Seq.map (fun (xs, category) -> ((Seq.collect id xs), category)) grouped

    let gamma = MathNet.Numerics.SpecialFunctions.Gamma

    let lngamma = MathNet.Numerics.SpecialFunctions.GammaLn

    let logSumExp (xs: float seq) =
        let logXs = Seq.map log xs
        let result = Seq.sum logXs
        exp result

    let beta xs =
        let xs = Seq.toList xs
        let nominator = (Seq.map lngamma xs |> Seq.sum)
        let denominator = (lngamma (List.sum xs))
        let logresult = nominator - denominator
        exp logresult

    module Seq =
        let toDic (xs : ('a * 'b) seq) =
            let result = new Dictionary<'a, 'b>()
            Seq.iter (fun (k, v) -> result.Add(k, v)) xs
            result

        let areSame xs ys =
            let lxs = Seq.toList xs
            let lys = Seq.toList ys
            if not(lxs.Length = lys.Length) then false
            else if lxs.Length = 0 then true
                 else List.zip lxs lys |> List.forall (fun (x, y) -> x = y)

    let nonKeyValues (dic: IDictionary<'a,'b>) (key: 'a) =
        let filtered = Seq.filter (fun (kvp: KeyValuePair<'a,'b>) -> not(kvp.Key = key)) dic
        Seq.map (fun (kvp: KeyValuePair<'a,'b>) -> kvp.Value) filtered

    let calculatePhis n (nc: IDictionary<string, int>) =
        let denominator = float n + float nc.Count
        let phiSeq = Seq.map (fun key -> (key, (float nc.[key] + 1.) / denominator)) nc.Keys
        phiSeq

    let calculateThetas n (nk: IDictionary<string, int>) =
        let denominator = float n + 2.
        let thetas = Seq.map (fun key -> (key, ((float nk.[key]) + 1.) / denominator)) nk.Keys
        thetas

    let getValOrDefault (lookup: IDictionary<'a, 'b>) key defaultValue =
        if not (lookup.ContainsKey key) then defaultValue
        else lookup.[key]

    let calculateCatThetas (nc: IDictionary<string, int>) (nck: IDictionary<string, Dictionary<string, int>>) features =
        let calculateCatTheta cat (nck : IDictionary<string, int>) =
            let catThetas = Seq.map (fun key -> (key, (float (getValOrDefault nck key 0) + 1.) / (float (nc.[cat]) + 2.))) features
            Seq.toDic catThetas
        let thetas = Seq.map (fun key -> (key, calculateCatTheta key nck.[key])) nck.Keys
        thetas

    let calculateCatAlphas (nc: IDictionary<string, int>) (nck: IDictionary<string, Dictionary<string, int>>) (nk: IDictionary<string, int>) features =
        let priorAlpha = 1.
        let features = Seq.toList features
        let alpha0 = nc.Count + 1
        let calculateCatAlpha cat (nck : IDictionary<string, int>) =
            let catAlphas = Seq.map (fun key -> (key, (float (getValOrDefault nck key 0) + priorAlpha) / (float (nk.[key] + alpha0)))) features
                            |> Seq.toList
            Seq.toDic (List.toSeq catAlphas)
        let alphas = Seq.map (fun key -> (key, calculateCatAlpha key nck.[key])) nck.Keys
        alphas

    let calculateMutualInformation keys (phis: IDictionary<string, float>) (catthetas: IDictionary<string, Dictionary<string, float>>) (thetas: IDictionary<string, float>) =
        let calcMI key =
            Seq.map (fun cat -> let catTheta = catthetas.[cat].[key]
                                let theta = thetas.[key]
                                let phi = phis.[cat]
                                let catMI = phi * (catTheta * (log(catTheta) - log(theta)) + (1. - catTheta) * (log(1. - catTheta) - log(1. - theta)))
                                catMI ) phis.Keys
        Seq.map (fun key -> (key, Seq.sum (calcMI key))) keys

    let calculateMutualInformationBer keys cat (phis: IDictionary<string, float>) (catthetas: IDictionary<string, Dictionary<string, float>>) (ocatthetas: IDictionary<string, Dictionary<string, float>>) (thetas: IDictionary<string, float>) =
        let calcMI key =
            let catTheta = catthetas.[cat].[key]
            let ocatTheta = ocatthetas.[cat].[key]
            let theta = thetas.[key]
            let phi = phis.[cat]
            let ophi = 1. - phi
            let catMI = phi * (catTheta * (log(catTheta) - log(theta)) + (1. - catTheta) * (log(1. - catTheta) - log(1. - theta)))
            let ocatMI = ophi * (ocatTheta * (log(ocatTheta) - log(theta)) + (1. - ocatTheta) * (log(1. - ocatTheta) - log(1. - theta)))
            catMI + ocatMI
        Seq.map (fun key -> (key, calcMI key)) keys



    let calculateCategoriesProbability (words: string seq) features (phis : IDictionary<string, float>) (catThetas: IDictionary<string, Dictionary<string, float>>) =
        let words = new HashSet<string>(words)
        let calculateCategoryProbability cat =
            let phi = phis.[cat]
            let catTheta = catThetas.[cat]
            let featureProb = Seq.map (fun word -> let theta = catTheta.[word]
                                                   if words.Contains(word) then theta
                                                   else 1. - theta) features |> Seq.toList
            let result = logSumExp (phi::featureProb)
            result
        let catProbabilities = Seq.map (fun cat -> (cat, calculateCategoryProbability cat)) phis.Keys |> Seq.toList
        let logTotal = log(List.sumBy snd catProbabilities)
        Seq.map (fun (cat, prob) -> (cat, exp(log(prob) - logTotal))) catProbabilities

    let factorial (n: float) = [(1.)..n] |> List.fold (*) 1.
    
    let calculateCategoriesProbabilityBCM words (phis : IDictionary<string, float>) (catAlphas: IDictionary<string, Dictionary<string, float>>) (nck: IDictionary<string, int>) =
        let words = words |> Seq.toList
        let n = float words.Length
        let calculateCategoryProbability cat =
            let phi = phis.[cat]
            if words.Length = 0 then phi
            else
                let catAlpha = catAlphas.[cat]
                let featuresProb = (words |> Seq.map (fun feature -> (float nck.[feature]) + (catAlpha.[feature])) |> beta) / (words |> Seq.map (fun feature -> catAlpha.[feature]) |> beta)
                let result = featuresProb * phi
                result
        let catProbabilities = Seq.map (fun cat -> (cat, calculateCategoryProbability cat)) phis.Keys |> Seq.toList
        let logTotal = log(List.sumBy snd catProbabilities)
        Seq.map (fun (cat, prob) -> (cat, exp(log(prob) - logTotal))) catProbabilities

    let calculateCategoriesProbabilityBCMBer cat words (phis : IDictionary<string, float>) (catAlphas: IDictionary<string, Dictionary<string, float>>) (ocatAlphas: IDictionary<string, Dictionary<string, float>>) (nck: IDictionary<string, int>) =
        let words = words |> Seq.toList
        let n = float words.Length
        let calculateCategoryProbability cat =
            let phi = phis.[cat]
            let oPhi = (1. - phi)
            if words.Length = 0 then phi
            else
                let catAlpha = catAlphas.[cat]
                let ocatAlpha = ocatAlphas.[cat]
                let featuresProb = (words |> Seq.map (fun feature -> (float nck.[feature]) + (catAlpha.[feature])) |> beta) / (words |> Seq.map (fun feature -> catAlpha.[feature]) |> beta)
                let ofeaturesProb = (words |> Seq.map (fun feature -> (float nck.[feature]) + (ocatAlpha.[feature])) |> beta) / (words |> Seq.map (fun feature -> ocatAlpha.[feature]) |> beta)
                let logresult = log(featuresProb) + log(phi) - log(exp(log(featuresProb) + log(phi)) + exp(log(ofeaturesProb) + log(oPhi))) 
                exp logresult
        let probability = calculateCategoryProbability cat
        probability

    let loadWordStats () =
        let nck = Inbox.Deserialize<Dictionary<string, Dictionary<string, int>>>("wordsByCategory.dat").Result
        let nc = Inbox.Deserialize<Dictionary<string, int>>("categories.dat").Result
        let nk = Inbox.Deserialize<Dictionary<string, int>>("words.dat").Result
        let n = Inbox.Deserialize<int>("summary.dat").Result
        (nck, nc, nk, n)

    let saveWordStats (nck, nc, nk, n) =
        Inbox.Serialize(nck, "wordsByCategory.dat").Wait() |> ignore
        Inbox.Serialize(nc, "categories.dat").Wait() |> ignore
        Inbox.Serialize(nk, "words.dat").Wait() |> ignore
        Inbox.Serialize(n, "summary.dat").Wait() |> ignore
        (nck, nc, nk, n)

    let calculateWordStats () =
        let mailItems = Inbox.GetProcessedMail() |> Seq.map mailToInput |> Seq.toList
        let nk = Inbox.GetWordCounts (mailItems |> Seq.map (fun (ws, _) -> Seq.ofList ws) )
        let nc = Inbox.GetWordCounts (mailItems |> Seq.map (fun (_, cs) -> Seq.ofList cs) )
        let nck = Inbox.GetWordCountsByCategory (Seq.map (fun (ws, cs) -> (Seq.ofList ws, Seq.ofList cs)) mailItems)
        let n = mailItems.Length
        (nck, nc, nk, n)

    let mulModel (nck, nc, (nk: Dictionary<string, int>), n) =
        let Phis = calculatePhis n nc |> Seq.toDic
        let CatThetas = calculateCatThetas nc nck nk.Keys |> Seq.toDic
        let Thetas = calculateThetas n nk |> Seq.toDic
        let features = Seq.toList nk.Keys
        let MutualInformation = calculateMutualInformation features Phis CatThetas Thetas |> Seq.filter (fun (_, v) -> not (Double.IsNaN v) && v > 0.1)
                                                                                          |> Seq.sortBy (fun (_, v) -> -v)
                                                                                          |> Seq.toDic
        let categoriseItem (ws: string list) =
            let catProb = calculateCategoriesProbability ws (MutualInformation.Keys) Phis CatThetas
            catProb
        let categoriseMail mail =
            let (ws, cs) = mailToInput mail
            categoriseItem ws
        categoriseMail

    let dcmModel (nck, nc, (nk: Dictionary<string, int>), n) =
        let Phis = calculatePhis n nc |> Seq.toDic
        let CatThetas = calculateCatThetas nc nck nk.Keys |> Seq.toDic
        let CatAlphas = calculateCatAlphas nc nck nk nk.Keys |> Seq.toDic
        let Thetas = calculateThetas n nk |> Seq.toDic
        let features = Seq.toList nk.Keys
        let MutualInformation = calculateMutualInformation features Phis CatThetas Thetas |> Seq.filter (fun (_, v) -> not (Double.IsNaN v) && v > 0.1)
                                                                                          |> Seq.sortBy (fun (_, v) -> -v)
                                                                                          |> Seq.toDic
        let categoriseItem (ws: string list) =
            let wordFilter = new HashSet<string>(MutualInformation.Keys)
            let catProb = calculateCategoriesProbabilityBCM (ws.Distinct().Where(wordFilter.Contains)) Phis CatAlphas (Inbox.GetWordCounts ws)
            catProb
        let categoriseMail mail =
            let (ws, cs) = mailToInput mail
            categoriseItem ws
        categoriseMail

    let dcmMultiLabelModel (nck, nc, (nk: Dictionary<string, int>), n) =
        let oNc = Seq.map (fun (kvp: KeyValuePair<string, int>) -> (kvp.Key, n - kvp.Value)) nc |> Seq.toDic
        let oNck = Seq.map (fun (nck: KeyValuePair<string, Dictionary<string, int>>) -> (nck.Key, (Seq.map (fun k -> (k, nk.[k] - nck.Value.[k])) nck.Value.Keys |> Seq.toDic))) nck |> Seq.toDic
        let Phis = calculatePhis n nc |> Seq.toDic
        let oPhis = calculatePhis n oNc |> Seq.toDic
        let CatThetas = calculateCatThetas nc nck nk.Keys |> Seq.toDic
        let oCatThetas = calculateCatThetas oNc oNck nk.Keys |> Seq.toDic
        let CatAlphas = calculateCatAlphas nc nck nk nk.Keys |> Seq.toDic
        let oCatAlphas = calculateCatAlphas oNc oNck nk nk.Keys |> Seq.toDic
        let Thetas = calculateThetas n nk |> Seq.toDic
        let features = Seq.toList nk.Keys
        let MutualInformation = Seq.map (fun cat -> let catMI = calculateMutualInformationBer features cat Phis CatThetas oCatThetas Thetas |> Seq.filter (fun (_, v) -> not (Double.IsNaN v) && v > 0.1)
                                                                                                                                            |> Seq.sortBy (fun (_, v) -> -v)
                                                                                                                                            |> Seq.toDic
                                                    (cat, catMI)) nc.Keys |> Seq.toDic
        let categoriseItem (ws : string list) =
            let calcCatProb cat =
                let wordFilter = new HashSet<string>(MutualInformation.[cat].Keys)
                let catProb = calculateCategoriesProbabilityBCMBer cat (ws.Distinct().Where(wordFilter.Contains)) Phis CatAlphas oCatAlphas (Inbox.GetWordCounts ws)
                catProb
            let cs = Seq.map (fun cat -> (cat, calcCatProb cat)) nc.Keys
            cs
        let categoriseMail mail =
            let (ws, cs) = mailToInput mail
            categoriseItem ws
        categoriseMail

    let runCategorisation trainingSetSource trainingModel (mailCategoriser: (Inbox.Mail -> (string * float) seq) -> unit) =
        Console.Write("Loading training set...") |> ignore
        let (nck, nc, nk, n) = trainingSetSource()
        Console.WriteLine("done") |> ignore
        Console.Write("Training model...") |> ignore
        let categoriser = trainingModel (nck, nc, nk, n)
        Console.WriteLine("done") |> ignore
        Console.Write("Categorising mail...") |> ignore
        mailCategoriser categoriser |> ignore
        Console.WriteLine("done") |> ignore

    let categoriseAllEMails () =
        let mailCategoriser (categoriser : (Inbox.Mail -> (string * float) seq)) =
            let categoriser = (fun mail -> categoriser mail |> Seq.filter (fun (c, prob) -> prob > 0.95)
                                                            |> Seq.map fst)
            Inbox.CategoriseAllMail((fun mail -> categoriser mail)).Wait()
        //calculateWordStats() |> saveWordStats |> ignore
        runCategorisation loadWordStats dcmMultiLabelModel mailCategoriser |> ignore