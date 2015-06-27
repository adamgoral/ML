#light
namespace ML.Console
module Program =
    open System

    open System.Collections.Generic

    let withTiming text f =
        let stopWatch = System.Diagnostics.Stopwatch()
        fun x ->
            stopWatch.Start()
            let result = f x
            stopWatch.Stop()
            printfn "%s operation took %O" text stopWatch.Elapsed
            result

    let rec mapTest map count =
        match count with
        | 0 -> map
        | n -> mapTest (Map.add n n map) (count - 1)

    let rec dicTest (dic: Dictionary<_,_>) count =
        match count with
        | 0 -> dic
        | n ->  dic.Add(n,n)
                dicTest dic (n - 1)

    type InstrumentPair() =
        member val Independent = "" with get, set
        member val Dependent = "" with get, set
    
    type InstrumentTriplet() =
        member val First = "" with get, set
        member val Second = "" with get, set
        member val Third = "" with get, set

    open Deedle
    open SmartTrader.Domain
    open System.Linq
    type String with
        member x.asStock() = Stock(x, String.Empty, String.Empty, String.Empty)

    let runKTests pairsToTest testName =
        let symbols = 
            List.concat [List.map fst pairsToTest; List.map snd pairsToTest]
            |> Seq.distinct
            |> Seq.map (fun (s:string) -> s.asStock())
            |> Seq.toList
        printfn "Loading prices"
        let pricesFrame = Reversal.closePricesWithMissing (new DateTime(2006,1,1)) (new DateTime(2011,12,31)) 200 symbols
        printfn "Prices loaded"
        let symbolsLookup = Set.ofSeq(pricesFrame.ColumnKeys)
        let logger = new ML.Common.SynchronisedCsvLogger(@"c:\temp\" + testName + @"\Test\kpairtestresults.csv")
        let loggerV = new ML.Common.SynchronisedCsvLogger(@"c:\temp\" + testName + @"\Validation\kpairtestresults.csv")
        let runBacktest (first:string , second: string) =
            if symbolsLookup.Contains first && symbolsLookup.Contains second then
                let dataFrame = Frame.ofColumns [first => pricesFrame.Columns.[first]; second => pricesFrame.Columns.[second]]
                                    |> Frame.dropSparseRows
                if dataFrame.ColumnCount = 2 then
                    let parameters = seq { for delta in [0.00001..(-0.000001)..0.] do
                                            for r in [0.001] do
                                                yield (delta, r) }
                    let (bapr, bsharpe, bdraw, bdrawdur, bdelta, br, bshortCount, blongCount, baprS, baprL, bframe) = 
                        Seq.map (fun (delta, r) -> Reversal.pairTradingKalmanFilter (dataFrame.Clone()) first second delta r) parameters
                        |> Seq.maxBy (fun (_, sharpe, _, _, _, _, _, _, _, _, _) -> sharpe)
                    logger.Write [|first; second; bapr.ToString(); bsharpe.ToString(); bdraw.ToString(); bdrawdur.ToString(); bdelta.ToString(); br.ToString(); bshortCount.ToString(); blongCount.ToString(); baprS.ToString(); baprL.ToString()|]
                    printfn "K Test(%s-%s) APR: %f Sharpe: %f maxdrawdown: %f maxdrawdowndur: %i delta: %f r: %f shortCount: %i longcount: %i aprS: %f aprL: %f" first second bapr bsharpe bdraw bdrawdur bdelta br bshortCount blongCount baprS baprL
                    if bsharpe > 2. then
                        bframe.SaveCsv (@"c:\temp\" + testName + @"\Test\K_" + first + "-" + second + ".csv", includeRowKeys = true)
                        let dataFrame = Reversal.closePrices (new DateTime(2012,1,1)) (new DateTime(2014,12,31)) 200 [(first.asStock());(second.asStock())]
                        let (bapr, bsharpe, bdraw, bdrawdur, bdelta, br, bshortCount, blongCount, baprS, baprL, bframe) = 
                            Reversal.pairTradingKalmanFilter (dataFrame.Clone()) first second bdelta br
                        loggerV.Write [|first; second; bapr.ToString(); bsharpe.ToString(); bdraw.ToString(); bdrawdur.ToString(); bdelta.ToString(); br.ToString(); bshortCount.ToString(); blongCount.ToString(); baprS.ToString(); baprL.ToString()|]
                        printfn "K Validation(%s-%s) APR: %f Sharpe: %f maxdrawdown: %f maxdrawdowndur: %i delta: %f r: %f shortCount: %i longcount: %i aprS: %f aprL: %f" first second bapr bsharpe bdraw bdrawdur bdelta br bshortCount blongCount baprS baprL
                        bframe.SaveCsv (@"c:\temp\" + testName + @"\Validation\K_" + first + "-" + second + ".csv", includeRowKeys = true)
        let tests = pairsToTest.Select(fun (f, s) -> runBacktest(f, s))
        tests.ToArray() |> ignore

    let runMATests pairsToTest testName =
        let symbols = 
            List.concat [List.map fst pairsToTest; List.map snd pairsToTest]
            |> Seq.distinct
            |> Seq.map (fun (s:string) -> s.asStock())
            |> Seq.toList
        printfn "Loading prices"
        let pricesFrame = Reversal.closePricesWithMissing (new DateTime(2006,1,1)) (new DateTime(2011,12,31)) 200 symbols
        printfn "Prices loaded"
        let symbolsLookup = Set.ofSeq(pricesFrame.ColumnKeys)
        let logger = new ML.Common.SynchronisedCsvLogger(@"c:\temp\" + testName + @"\Test\pairtestresults.csv")
        let loggerV = new ML.Common.SynchronisedCsvLogger(@"c:\temp\" + testName + @"\Validation\pairtestresults.csv")
        let runBacktest (first:string , second: string) =
            if symbolsLookup.Contains first && symbolsLookup.Contains second then
                let dataFrame = Frame.ofColumns [first => pricesFrame.Columns.[first]; second => pricesFrame.Columns.[second]]
                                    |> Frame.dropSparseRows
                if dataFrame.ColumnCount = 2 then
                    let parameters = seq { for lag in [2..1..100] do
                                                yield lag }
                    //(apr, sharpeRatio, lag, (Stats.count dataFrame?totalret), dataFrame)
                    let (bapr, bsharpe, blag, bcount, bframe) = 
                        Seq.map (fun (lag) -> Reversal.pairTradingMovAvg (dataFrame.Clone()) first second lag) parameters
                        |> Seq.maxBy (fun (_, sharpe, _, _, _) -> sharpe)
                    logger.Write [|first; second; bapr.ToString(); bsharpe.ToString(); blag.ToString()|]
                    printfn "MA Test(%s-%s) APR: %f Sharpe: %f Lag: %i" first second bapr bsharpe blag
                    if bsharpe > 2. then
                        bframe.SaveCsv (@"c:\temp\" + testName + @"\Test\MA_" + first + "-" + second + ".csv", includeRowKeys = true)
                        let dataFrame = Reversal.closePrices (new DateTime(2012,1,1)) (new DateTime(2014,12,31)) 200 [(first.asStock());(second.asStock())]
                        let (bapr, bsharpe, blag, bcount, bframe) = 
                            Reversal.pairTradingMovAvg (dataFrame.Clone()) first second blag
                        loggerV.Write [|first; second; bapr.ToString(); bsharpe.ToString(); blag.ToString()|]
                        printfn "MA Test(%s-%s) APR: %f Sharpe: %f Lag: %i" first second bapr bsharpe blag
                        bframe.SaveCsv (@"c:\temp\" + testName + @"\Validation\MA_" + first + "-" + second + ".csv", includeRowKeys = true)
        let tests = pairsToTest.Select(fun (f, s) -> runBacktest(f, s))
        tests.ToArray() |> ignore


    let runKTest3() =
        let pairsToTest = 
            let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"..\Data\cointegrated etf.csv"))
            seq { for t in reader.GetRecords<InstrumentTriplet>() do
                    yield (t.First, t.Second, t.Third)
                    yield (t.Second, t.First, t.Third)
                    yield (t.Third, t.Second, t.First)
                    yield (t.First, t.Third, t.Second)
                    yield (t.Second, t.Third, t.First)
                    yield (t.Third, t.First, t.Second)
                }|> Seq.toList

        let logger = new ML.Common.SynchronisedCsvLogger(@"c:\temp\K3CointegratedTest\ktriplettestresults.csv")
        let runBacktest (first:string , second: string, third : string) =
            let dataFrame = Reversal.closePrices (new DateTime(2006,4,10)) (new DateTime(2012,4,10)) 200 [first.asStock() ; second.asStock(); third.asStock()]
            if dataFrame.ColumnCount = 3 then
                let (bapr, bsharpe, bdraw, bdrawdur, bdelta, br, bshortCount, blongCount, baprS, baprL, bframe) = 
                    Seq.map (fun r -> Reversal.pairTradingKalmanFilter3 (dataFrame.Clone()) first second third 0.0000001 r) [0.001] //[0.0000001] //[0.0001..(-0.0000001)..0.]
                    |> Seq.maxBy (fun (_, sharpe, _, _, _, _, _, _, _, _, _) -> sharpe)
                logger.Write [|first; second; third; bapr.ToString(); bsharpe.ToString(); bdraw.ToString(); bdrawdur.ToString(); bdelta.ToString(); br.ToString(); bshortCount.ToString(); blongCount.ToString(); baprS.ToString(); baprL.ToString()|]
                printfn "K3(%s-%s-%s) APR: %f Sharpe: %f maxdrawdown: %f maxdrawdowndur: %i delta: %f r: %f shortCount: %i longcount: %i aprS: %f aprL: %f" first second third bapr bsharpe bdraw bdrawdur bdelta br bshortCount blongCount baprS baprL
                bframe.SaveCsv (@"c:\temp\K3CointegratedTest\K_" + first + "-" + second + "-" + third + ".csv", includeRowKeys = true)

        let tests = pairsToTest.Select(fun (f, s, t) -> runBacktest(f, s, t))
        tests.ToArray() |> ignore

    let runKValidation3() =
        let pairsToTest = 
            let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"..\Data\selectedcointegrated etf.csv"))
            reader.GetRecords<InstrumentTriplet>()
                |> Seq.map (fun pair -> (pair.First, pair.Second, pair.Third))
                |> Seq.toList

        let logger = new ML.Common.SynchronisedCsvLogger(@"c:\temp\K3CointegratedValidation\ktriplettestresults.csv")
        let runBacktest (first:string , second: string, third : string) =
            let dataFrame = Reversal.closePrices (new DateTime(2012,4,10)) (new DateTime(2014,12,31)) 200 [first.asStock() ; second.asStock(); third.asStock()]
            if dataFrame.ColumnCount = 3 then
                let (bapr, bsharpe, bdraw, bdrawdur, bdelta, br, bshortCount, blongCount, baprS, baprL, bframe) = 
                    Seq.map (fun r -> Reversal.pairTradingKalmanFilter3 (dataFrame.Clone()) first second third 0.0000001 r) [0.001] //[0.0000001] //[0.0001..(-0.0000001)..0.]
                    |> Seq.maxBy (fun (_, sharpe, _, _, _, _, _, _, _, _, _) -> sharpe)
                logger.Write [|first; second; third; bapr.ToString(); bsharpe.ToString(); bdraw.ToString(); bdrawdur.ToString(); bdelta.ToString(); br.ToString(); bshortCount.ToString(); blongCount.ToString(); baprS.ToString(); baprL.ToString()|]
                printfn "K3(%s-%s-%s) APR: %f Sharpe: %f maxdrawdown: %f maxdrawdowndur: %i delta: %f r: %f shortCount: %i longcount: %i aprS: %f aprL: %f" first second third bapr bsharpe bdraw bdrawdur bdelta br bshortCount blongCount baprS baprL
                bframe.SaveCsv (@"c:\temp\K3CointegratedValidation\K_" + first + "-" + second + "-" + third + ".csv", includeRowKeys = true)

        let tests = pairsToTest.Select(fun (f, s, t) -> runBacktest(f, s, t))
        tests.ToArray() |> ignore


    let runKValidation() =
        let pairsToTest = 
            let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"..\Data\validationpairs.csv"))
            reader.GetRecords<InstrumentPair>()
                |> Seq.map (fun pair -> (pair.Independent, pair.Dependent))
                |> Seq.toList

        let logger = new ML.Common.SynchronisedCsvLogger(@"c:\temp\KCointegratedValidation\kpairtestresults.csv")
        let runBacktest (first:string , second: string) =
            let dataFrame = Reversal.closePrices (new DateTime(2012,4,10)) (new DateTime(2014,12,31)) 200 [first.asStock() ; second.asStock()]
            if dataFrame.ColumnCount = 2 then
                let (bapr, bsharpe, bdraw, bdrawdur, bdelta, br, bshortCount, blongCount, baprS, baprL, bframe) = 
                    Seq.map (fun r -> Reversal.pairTradingKalmanFilter (dataFrame.Clone()) first second 0.0000001 r) [0.001] //[0.0000001] //[0.0001..(-0.0000001)..0.]
                    |> Seq.maxBy (fun (_, sharpe, _, _, _, _, _, _, _, _, _) -> sharpe)
                logger.Write [|first; second; bapr.ToString(); bsharpe.ToString(); bdraw.ToString(); bdrawdur.ToString(); bdelta.ToString(); br.ToString(); bshortCount.ToString(); blongCount.ToString(); baprS.ToString(); baprL.ToString()|]
                printfn "K(%s-%s) APR: %f Sharpe: %f maxdrawdown: %f maxdrawdowndur: %i delta: %f r: %f shortCount: %i longcount: %i aprS: %f aprL: %f" first second bapr bsharpe bdraw bdrawdur bdelta br bshortCount blongCount baprS baprL
                bframe.SaveCsv (@"c:\temp\KCointegratedValidation\K_" + first + "-" + second + ".csv", includeRowKeys = true)

        let tests = pairsToTest.Select(fun (f, s) -> runBacktest(f, s))
        tests.ToArray() |> ignore

    let runBBValidation() =
        let pairsToTest = 
            let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"..\Data\BBValidationPairs.csv"))
            reader.GetRecords<InstrumentPair>()
                |> Seq.map (fun pair -> (pair.Independent, pair.Dependent))
                |> Seq.toList

        let logger = new ML.Common.SynchronisedCsvLogger(@"c:\temp\BBCointegratedValidation\BBpairtestresults.csv")
        let runBacktest (first:string , second: string) =
            let dataFrame = Reversal.closePrices (new DateTime(2012,4,10)) (new DateTime(2014,12,31)) 200 [first.asStock() ; second.asStock()]
            if dataFrame.ColumnCount = 2 then
                let (bapr, bsharpeRatio, bmaxdrawdown, bmaxdrawdownDuration, blag, bshortCount, blongCount, bdataFrame) = 
                    Seq.map (fun lag -> Reversal.pairTradingBollingerBands (dataFrame.Clone()) first second 20) [0.001] //[0.0000001] //[0.0001..(-0.0000001)..0.]
                    |> Seq.maxBy (fun (apr, sharpeRatio, maxdrawdown, maxdrawdownDuration, lag, shortCount, longCount, dataFrame) -> sharpeRatio)
                logger.Write [|first; second; bapr.ToString(); bsharpeRatio.ToString(); bmaxdrawdown.ToString(); bmaxdrawdownDuration.ToString(); blag.ToString(); bshortCount.ToString(); blongCount.ToString() |]
                printfn "BB(%s-%s) APR: %f Sharpe: %f maxdrawdown: %f maxdrawdowndur: %i lag: %i shortCount: %i longcount: %i " first second bapr bsharpeRatio bmaxdrawdown bmaxdrawdownDuration blag bshortCount blongCount
                bdataFrame.SaveCsv (@"c:\temp\BBCointegratedValidation\BB_" + first + "-" + second + ".csv", includeRowKeys = true)

        let tests = pairsToTest.Select(fun (f, s) -> runBacktest(f, s))
        tests.ToArray() |> ignore

    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Statistics

    let calculateF startDate endDate pairs =
        let returns =
            List.map (fun (f: string, s: string) ->  
                          let dataFrame = Reversal.closePrices startDate endDate 200 [f.asStock() ; s.asStock()]
                          let (apr, sharpe, _, _, _, _, _, _, _, _, df) = Reversal.pairTradingKalmanFilter dataFrame f s 0.0000001 0.001
                          (f, s, apr, df?totalret)) pairs
        let aprs = returns |> List.map (fun (_, _, apr, _) -> apr) |> DenseVector.ofList
        let returnsFrame = Frame.ofColumns (List.map (fun (f,s,_,rs)->((f,s), rs)) returns)
        let cols = returnsFrame.ColumnKeys |> Array.ofSeq
        let corr =
            Array2D.init (aprs.Count) (aprs.Count) (fun i j -> let c1 = cols.[i]
                                                               let c2 = cols.[j]
                                                               let rs1 = Frame.mapRows (fun r s -> s.GetAs(c1, 0.)* 252.) returnsFrame |> Series.values
                                                               let rs2 = Frame.mapRows (fun r s -> s.GetAs(c2, 0.)* 252.) returnsFrame |> Series.values
                                                               (Statistics.Covariance (rs1, rs2)) )
                                                               //(Correlation.Pearson (rs1, rs2)))
            |> DenseMatrix.ofArray2
        ((corr.Inverse()) * aprs, aprs, corr, returnsFrame)

    let calculateValidationReturns () =
        let selected = 
            let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"..\Data\selected.csv"))
            reader.GetRecords<InstrumentPair>()
                |> Seq.map (fun pair -> (pair.Independent, pair.Dependent))
                |> Seq.toList
        let (f, aprs, cov, returnsFrame) = calculateF (new DateTime(2006,4,10)) (DateTime.Today) selected 
        returnsFrame.SaveCsv (@"c:\temp\K_selectedReturnsVerification.csv", includeRowKeys = true, culture = Globalization.CultureInfo.CurrentUICulture)

    let estimateFFromRecentHistory () =
        let selected = 
            let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"..\Data\selected.csv"))
            reader.GetRecords<InstrumentPair>()
                |> Seq.map (fun pair -> (pair.Independent, pair.Dependent))
                |> Seq.toList
        let (_, aprs, _, _) = calculateF (new DateTime(2006,4,10)) (new DateTime(2014,12,31)) selected 
        let (_, _, cov, _) = calculateF (new DateTime(2006,4,10)) (new DateTime(2014,12,31)) selected 
        ((cov.Inverse()) * aprs, aprs, cov)

//    let estimateFToMaximiseG () =
//        let selected = 
//            let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"..\Data\selected.csv"))
//            reader.GetRecords<InstrumentPair>()
//                |> Seq.map (fun pair -> (pair.First, pair.Second))
//                |> Seq.toList

    let GetAllPairs() =
        let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"..\Data\Instruments.csv"))
        let instruments = Reversal.instruments
        seq { for instrument1 in instruments do
                for instrument2 in instruments do
                    if not (instrument1 = instrument2) then
                        yield (instrument1.Symbol, instrument2.Symbol) }

    let GetPairs name =
        let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"C:\Temp\" + name + ".csv"))
        let instruments = reader.GetRecords<InstrumentPair>() |> List.ofSeq
        seq { for instrument in instruments do
                  yield (instrument.Independent, instrument.Dependent)
                  yield (instrument.Dependent, instrument.Independent) }

    type OLHC() =
        member val Date = DateTime.MinValue with get, set
        member val Open = 0. with get, set
        member val Low = 0. with get, set
        member val High = 0. with get, set
        member val Close = 0. with get, set


    let loadCcy ccy =
        let reader = CsvHelper.CsvReader(System.IO.File.OpenText(@"..\MarketData\FX\" + ccy + ".csv"))
        let items = reader.GetRecords<OLHC>()
                    |> List.ofSeq
        items
    let testCcy ccy1 ccy2 =
        let minDate = DateTime(2006, 1, 1)
        let maxDate = DateTime(2011, 12, 31)
        let ccy1prices = loadCcy ccy1 
                         |> Seq.map (fun item -> (item.Date, item.Close))
                         |> Seq.filter (fun (d, _) -> d >= minDate && d <= maxDate)
                         |> Series.ofObservations
        let ccy2prices = loadCcy ccy2 
                         |> Seq.map (fun item -> (item.Date, item.Close))
                         |> Seq.filter (fun (d, _) -> d >= minDate && d <= maxDate)
                         |> Series.ofObservations
        let frame = Frame.ofColumns [(ccy1, ccy1prices); (ccy2, ccy2prices)]
                    |> Frame.dropSparseRows
        let (apr, sharpeRatio, maxdrawdown, maxdrawdownDuration, lag, shortCount, longCount, dataFrame) = Reversal.pairTradingBollingerBands (frame.Clone()) ccy1 ccy2 120// 0.0000001 0.001
        dataFrame.SaveCsv(@"c:\temp\Currencies\Test\k_" + ccy1 + "-" + ccy2 + ".csv", includeRowKeys = true, culture = Globalization.CultureInfo.CurrentCulture)
        ()
    
    let split n (xs: float seq) =
        let size = float(Seq.length xs) / (float n)
                   |> round
                   |> int
        seq { for i = 0 to (n-1) do
                let result = Seq.skip (i * size) xs |> Seq.toList
                if result.Length < size then yield (result |> List.toSeq)
                else yield Seq.take size result }      

    let variance xs =
        let avg = Seq.average xs
        Seq.map (fun x -> pown (x - avg) 2) xs
            |> Seq.average

    let rs (xs: float seq) =
        let avg = Seq.average xs
        let demeaned = Seq.map (fun v -> v - avg) xs |> List.ofSeq
        let cumulative = Seq.unfold (fun (acc, ds) -> match ds with
                                                      | [] -> None
                                                      | h::tail -> Some(acc + h, (acc + h, tail))) (0., demeaned)
                         |> List.ofSeq
        let r = (List.max cumulative) - (List.min cumulative)
        let stddev = (variance xs) |> sqrt
        r / stddev

    let hurstExp (xs: float seq) =
        let length = Seq.length xs |> float
        let rss = List.map (fun n -> (float n, Seq.map rs (split n xs) |> Seq.average)) [2..10]
        let logRss = List.map (fun (n, x) -> (log (length / n), log x)) rss
        let indep = List.map fst logRss
        let dep = List.map snd logRss
        let olsRes = Reversal.ordinaryLeastSquares indep dep
        olsRes.Slope

    let hurstForStock (symbol: string) returnPeriod =
        let prices = Reversal.closePrices (DateTime(2004,1,1)) (DateTime(2005,12,31)) 100 [symbol.asStock()]
        if prices.ColumnCount = 0 then None
        else
            let returns = log(prices.[symbol]) - log (Series.shift returnPeriod prices.[symbol])
            let values = returns.Values
                         |> Seq.windowed returnPeriod
                         |> Seq.map (fun win -> win.[0])
                         |> List.ofSeq
            let hurst = hurstExp values
            Some hurst

    let rec hurstForStockForPeriod symbol returnPeriods =
        match returnPeriods with
        | [] -> []
        | period::tail -> match hurstForStock symbol period with
                          | None -> -1.::(hurstForStockForPeriod symbol tail)
                          | Some h -> h::(hurstForStockForPeriod symbol tail)

    let calculateHurstForAllStocks () =
        let returnPeriods = [1;5;10;20]
        let instruments = Reversal.getInstruments() |> Seq.map (fun inst -> inst.Symbol) |> Seq.toList// GetPairs "CointegratedAll" |> Seq.map fst |> Seq.distinct |> List.ofSeq
        let log =
            let logger = ML.Common.SynchronisedCsvLogger(@"c:\temp\hurstAll.csv")
            let headers = "Symbol"::(List.map (fun p -> (string p) + " periods") returnPeriods) |> List.toArray
            logger.Write(headers)
            (fun (entry: string[]) ->
                printfn "%A" entry
                System.Diagnostics.Debug.WriteLine(entry)
                logger.Write(entry))
        let hs = List.map (fun s -> let results = hurstForStockForPeriod s returnPeriods |> List.map string
                                    let logEntry = s::results |> List.toArray
                                    log logEntry) instruments
        ()

    let returnCorrelationForStock lookbackperiod holdingperiod fromDate toDate symbol =
        let prices = Reversal.getPrices (fun ts -> ts.AdjustedClose) fromDate toDate (Instrument(symbol))
                     |> Async.RunSynchronously
                     |> Seq.map (fun kvp -> kvp.Value)
                     |> Seq.toList
        let windowSize = lookbackperiod + holdingperiod + 2
        let windowed = Seq.windowed windowSize prices |> Seq.toList
        let windowRet (w: seq<_>) =
            log(w.Last()) - log(w.First())
        let inputs = windowed
                     |> Seq.mapi (fun i w -> if (i+1) % holdingperiod = 0 then Some w
                                             else None)
                     |> Seq.choose id
                     |> Seq.map (fun w -> (Seq.take (lookbackperiod + 1) w |> Seq.toArray, Seq.skip (lookbackperiod + 1) w |> Seq.toArray))
                     |> Seq.map (fun (rs, hs) -> (windowRet rs, windowRet hs))
                     |> Seq.toList
        (Correlation.Pearson (Seq.map fst inputs, Seq.map snd inputs), inputs.Length)

    let calculateReturnCorrelationsForAllStocks () =
        let fromDate = DateTime(2006, 1, 1)
        let toDate = DateTime(2012, 12, 31)
        let instruments = Reversal.getInstruments() |> Seq.map (fun inst -> inst.Symbol) |> Seq.toList
        let log =
            let logger = ML.Common.SynchronisedCsvLogger(@"c:\temp\CorrelationOfReturns.csv")
            let headers = [|"Symbol"; "Lookback"; "Holding"; "Correlation"; "SamplesNumber"|]
            logger.Write headers
            fun entry -> 
                printfn "%A" entry
                logger.Write entry

        let returnPeriods =
            seq { for i in [5;10;25;60;120;250] do
                    for j in [1;5;10;25;60] do
                        yield (i, j) }
        returnPeriods
            |> Seq.map (fun (lookbackperiod, holdingperiod) -> 
                            let corrForPeriod = fun s -> 
                                                    let (corr, n) = returnCorrelationForStock lookbackperiod holdingperiod fromDate toDate s
                                                    log [|s; string lookbackperiod; string holdingperiod; string corr; string n|]
                            List.map corrForPeriod instruments)
            |> Seq.toList

    [<EntryPoint>]
    let main argv = 
        //calculateReturnCorrelationsForAllStocks() |> ignore
        //testCcy "AUDUSD" "CADUSD"
        //testCcy "CADUSD" "AUDUSD"
        //runKTests (GetPairs "CointegratedETFs" |> List.ofSeq) "CointegratedETFs"
        //runKTests (GetPairs "CointegratedAll" |> List.ofSeq) "CointegratedAll"
        //runMATests (GetCointegratedETFs() |> List.ofSeq) "CointegratedETFs_MA"
        //runKValidation3()
        //runKValidation()
        //runBBValidation()
        //calculateValidationReturns()
//        let (f, aprs, cov) = estimateFFromRecentHistory()
//        let expectedG = ((f.ToRowMatrix()) * cov * f) / 2.
//        let expectedSharpe = sqrt ((f.ToRowMatrix()) * cov * f).[0]
//        printfn "Done"
        ExcelParser.eTests() |> ignore
        Console.ReadLine() |> ignore
        0 // return an integer exit code
