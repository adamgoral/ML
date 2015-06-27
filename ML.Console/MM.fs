namespace ML.Console
module MM =
    open System
    open SmartTrader.Data
    open SmartTrader.Domain
    open System.Threading
    open System.Linq

    let pricesDir = Uri("file://../MarketData/prices/")

    let getPrices instrument fromDate toDate =
        let source = LocalMarketDataStore(pricesDir)
        let result = source.GetAsync(instrument, fromDate, toDate, CancellationToken.None).Result
                        |> Seq.toList
        result

    let getInstruments () =
        LocalInstrumentSource(Uri("file://../MarketData/instruments.csv")).GetInstruments(CancellationToken.None).Result
    
    let getInstrumentsBy f =
        getInstruments().OfType<Stock>() 
            |> Seq.groupBy f
            |> Seq.map (fun (k, i) -> (k, i |> Seq.toList))
            |> Seq.toList

    let instrumentsBySector =
        getInstrumentsBy (fun i -> i.Sector)

    let instrumentsByIndustry =
        getInstrumentsBy (fun i -> i.Industry)

    let instrumentsAll =
        getInstrumentsBy (fun _ -> "All")

    let dailyReturn (item : TimeSeriesPrice) =
        (item.Date, Math.Log((item.Close)/(item.Open)))

    let prices instruments minHistory fromDate toDate =
        instruments 
        |> List.map (fun inst -> (inst, getPrices inst fromDate toDate))
        |> List.filter (fun (_, prices) -> prices.Length > minHistory)
        |> List.filter (fun (_, prices) -> prices.Head.Date <= fromDate.AddDays(20.))

    open Deedle

    let period = 5
    let annualScaling = 250. / (float period)

    let returnsFrame prices =
        //let dailyReturns = List.map (fun (inst, prices) -> (inst, List.map dailyReturn (List.filter (fun p -> (p.Open > 0.) && (p.Close > 0.)) prices))) prices
        let closes = List.map (fun (inst, prices) -> (inst, List.map (fun (item: TimeSeriesPrice) -> (item.Date, item.AdjustedClose)) (List.filter (fun p -> (p.Open > 0.) && (p.Close > 0.) && (p.AdjustedClose > 0.)) prices))) prices
        let series = Seq.map (fun (inst : Stock, returns) -> (inst.Symbol, Series.ofObservations(returns))) closes
        let result = Frame.ofColumns series
                        |> Frame.dropSparseRows
        let result = result / result.Shift(period) |> log
        result |> Frame.dropSparseRows

    let timeSeriesFrame timeSeries =
        let series = Seq.map (fun (key, items) -> (key, Series.ofObservations items)) timeSeries
        let result = Frame.ofColumns series
        result
    
    open MathNet.Numerics.LinearAlgebra

    let lagged frame =
        let x = Frame.toArray2D frame
                |> DenseMatrix.ofArray2
        x.SubMatrix(1, x.RowCount - 1, 0, x.ColumnCount)
            .Append(x.SubMatrix(0, x.RowCount - 1, 0, x.ColumnCount))
//        x.SubMatrix(4,x.RowCount-4, 0, x.ColumnCount)
//            .Append(x.SubMatrix(3, x.RowCount - 4, 0, x.ColumnCount))
//            .Append(x.SubMatrix(2, x.RowCount - 4, 0, x.ColumnCount))
//            .Append(x.SubMatrix(1, x.RowCount - 4, 0, x.ColumnCount))
//            .Append(x.SubMatrix(0, x.RowCount - 4, 0, x.ColumnCount))

    let trainingSize = 800

    let validationSize = 200

    let getTrainingAndVAlidationSets count validationSetSize (data : Matrix<_>) =
        List.init count (fun i ->
            let validationSet = data.SubMatrix(i * validationSetSize, validationSetSize, 0, data.ColumnCount)
            let trainingSet =
                if i = 0 then
                    data.SubMatrix(validationSetSize, (count - 1) * validationSetSize, 0, data.ColumnCount)
                else if i = (count - 1) then
                     data.SubMatrix(0, (count - 1) * validationSetSize, 0, data.ColumnCount)
                     else 
                        let trainingSet1 = data.SubMatrix(0, i * validationSetSize, 0, data.ColumnCount)
                        let trainingSet2 = data.SubMatrix((i + 1) * validationSetSize, (count - i - 1) * validationSetSize, 0, data.ColumnCount)
                        trainingSet1.Transpose().Append(trainingSet2.Transpose()).Transpose()
            (trainingSet, validationSet))

    let dataSetMean data = 
        let colSums = Matrix.foldByCol(fun a b -> a + b) 0. data
        colSums / (float data.RowCount)

    let dataSetCovariance mean data =
        let demeaned = Matrix.mapRows (fun i v -> v - mean) data
        demeaned.TransposeThisAndMultiply(demeaned) / (float data.RowCount)
    
    open MicrosoftResearch.Infer.Maths

    let posDefInverse (m: Matrix<_>) =
        let ps = new PositiveDefiniteMatrix(m.ToArray())
        let inverse = ps.Inverse().ToArray()
        DenseMatrix.ofArray2 inverse

    let conditionalMean (mean: Vector<_>) (covariance: Matrix<_>) size (observed: Vector<_>) =
        let mu_x = mean.SubVector(0, size)
        let sigma_x_z = covariance.SubMatrix(0, size, size, covariance.ColumnCount - size)
        let sigma_z_z = covariance.SubMatrix(size, covariance.RowCount - size, size, covariance.ColumnCount - size)
        let mu_z = mean.SubVector(size, mean.Count - size)
        let cmu = mu_x + sigma_x_z.Multiply(sigma_z_z.Inverse()) * (observed - mu_z)
        cmu

    let conditionalVariance (mean: Vector<_>) (covariance: Matrix<_>) size =
        let sigma_x_x = covariance.SubMatrix(0, size, 0, size)
        let sigma_x_z = covariance.SubMatrix(0, size, size, covariance.ColumnCount - size)
        let sigma_z_x = covariance.SubMatrix(size, covariance.RowCount - size, 0, size)
        let sigma_z_z = covariance.SubMatrix(size, covariance.RowCount - size, size, covariance.ColumnCount - size)
        let csigma = sigma_x_x - sigma_x_z.Multiply(sigma_z_z.Inverse()).Multiply(sigma_z_x)
        csigma

    let optimalAlloc (covariance: Matrix<_>) (mean: Vector<_>) expectedReturn =
        //printfn "%A" (covariance.ToArray())
        let ones = DenseVector.ofArray (Array.init (mean.Count) (fun _ -> 1.))
        //printfn "%A" (ones.ToArray())
        let right = ones.ToColumnMatrix().Append(mean.ToColumnMatrix())
        //printfn "%A" (right.ToArray())
        let bottom = right.Transpose().Append(DenseMatrix.init 2 2 (fun _ _ -> 0.))
        //printfn "%A" (bottom.ToArray())
        let result = (covariance * 2.).Append(right)
        //printfn "%A" (result.ToArray())
        let result = result.Transpose().Append(bottom.Transpose())
        //printfn "%A" (result.ToArray())
        let result = result.Inverse()
        let v = Array.init (mean.Count + 2) (fun i -> if i <= mean.Count then 0.
                                                      else if i = mean.Count + 1 then 1.
                                                           else expectedReturn)
        let allocation = result.Multiply(DenseVector.ofArray v)
        allocation.SubVector(0, mean.Count)

    let randomIntegerGenerator min max =
        let rand = new Random()
        fun () -> rand.Next (min, max)

    let randomIntegerGeneratorWithSeed seed min max =
        let rand = new Random(seed)
        fun () -> rand.Next (min, max)

    // Nonbiased for single test
    let samplingDistributionBootstrapSingle sampleSize sampleCount (source: float array) =
        let rand = randomIntegerGenerator 0 (source.Length - 1)
        let avg = Array.average source
        let sample = fun () -> List.init sampleSize (fun _ -> source.[rand()] - avg)
        let averages = Array.init sampleCount (fun _ -> sample() |> List.average)
        let average = averages |> Array.average
        Array.map (fun v -> v - average) averages

    // Nonbiased for multi test (compensates for data mining bias)
    let samplingDistributionBootstrapMultiple sampleSize sampleCount (sources: float [,]) =
        let matrix = DenseMatrix.ofArray2 sources
        let rand = randomIntegerGenerator 0 (matrix.RowCount - 1)
        let avgs = matrix.ColumnSums() / (float matrix.RowCount)
        let samples =
            seq { for i in 1..sampleCount do
                    let sample = List.init (matrix.ColumnCount) (fun c -> List.init sampleSize (fun _ -> matrix.At(rand(), c) - avgs.[c]) |> List.average)
                                    |> List.max
                    yield sample }
        let res = Seq.toArray samples
        res

    type TradeSummary = 
        {
            Forecast: float;
            Actual: float;
            Volume: float;
            Return: float;
        }

    let validation mean covariance width (validationSet: Matrix<_>) (slippage: float[]) =
        let returns =
            validationSet.EnumerateRows() 
                |> Seq.map (fun v -> (v.SubVector(0, width), conditionalMean mean covariance width (v.SubVector(width, v.Count - width))))
                |> Seq.toList
                |> List.map (fun (a, p) ->  //let cvar = conditionalVariance mean covariance width
                                            //let allocation = optimalAlloc cvar p 1.1
                                            //let total = Seq.fold (fun t v -> abs(v) + t) 0. (allocation.ToArray())
                                            //let palloc = Array.map (fun v -> v / total) (allocation.ToArray())
                                            let palloc = Array.map (fun v -> if v > 0.005 then 1.
                                                                             else if v < -0.005 then -1.
                                                                                  else 0.) (p.ToArray())
                                            [|for i in 0..(p.Count-1) ->
                                              //let predicted = p.[i]
                                              let actual = a.[i]
                                              let r = actual * palloc.[i] - slippage.[i] * Math.Abs(palloc.[i])
                                              { Forecast = p.[i]; Actual = a.[i]; Volume = palloc.[i]; Return = r }
                                            |]
                                           )
        let validationResult = returns |> List.fold (fun sum v -> Array.map2 (fun a b -> a + b.Return) sum v) (Array.zeroCreate width)
        validationResult

    let validationSingle mean covariance width (obs: Matrix<_>) (act: Matrix<_>) (slippage: float[]) =
        let obsItem = obs.EnumerateRows() |> Seq.exactlyOne
        let actItem = act.EnumerateRows() |> Seq.exactlyOne
        let forecast = conditionalMean mean covariance width obsItem
        let palloc = Array.map (fun v -> if v > 0.005 then 1.
                                         else if v < -0.005 then -1.
                                              else 0.) (forecast.ToArray())
        let returns = [|for i in 0..(forecast.Count-1) ->
                            let actual = actItem.[i]
                            let r = actual * palloc.[i] - slippage.[i] * Math.Abs(palloc.[i])
                            { Forecast = forecast.[i]; Actual = actItem.[i]; Volume = palloc.[i]; Return = r }
                      |]
        returns

    let validationSets width (dataSet: Matrix<_>) skipFirst skipLast =
        let setCount = int(Math.Floor((float(dataSet.RowCount)) / 200.)) - skipLast
        getTrainingAndVAlidationSets setCount 200 dataSet
            |> Seq.skip skipFirst |> Seq.toList
            |> List.map (fun (ts, vs) -> 
                let dmean = dataSetMean ts
                let dcov = dataSetCovariance dmean ts
                let v = validation dmean dcov width vs (Array.zeroCreate width)
                //printfn "val return %A" v
                v)
            |> List.fold (fun sum v -> Array.map2 (fun a b -> a + b / (float (setCount - skipFirst))) sum v) (Array.zeroCreate width)

    let getListDateRange ls =
        let first = Seq.take 1 ls |> Seq.exactlyOne
        let last = Seq.last ls
        (first, last)

    let validationRun () =
        let fromDate = DateTime.MinValue
        let toDate = DateTime.Today
        //getInstrumentsBy (fun _ -> "all")
        instrumentsBySector
        //[("AAPL", [Stock("AAPL","","",""); Stock("GOOG","","","")] )]
        |> List.iter (fun (sector, instruments) ->
                      let iprices = prices instruments 3000 fromDate toDate
                      let ireturns = returnsFrame iprices
                      let cols = ireturns.ColumnCount
                      if cols > 1 then
                          let firstDate = ireturns.Rows.FirstKey();
                          let lastDate = ireturns.Rows.LastKey();
                          let lreturns = lagged ireturns
                          let skipLast = int(Math.Floor((float(lreturns.RowCount)) / 200. / 2.))
                          let avgReturn = validationSets cols lreturns 0 skipLast
                          printfn "%s (%i instruments) avg: %f" sector cols (Array.average avgReturn)
                          let instReturns = Array.zip (List.map fst iprices |> Array.ofList) avgReturn
                          Array.iter (fun (a : Stock, r) -> printfn "%s %f - from %s to %s" (a.Symbol) r (firstDate.ToString("dd/MM/yyyy")) (lastDate.ToString("dd/MM/yyyy"))) instReturns
                          let filteredInstruments = Array.filter (fun (a, b) -> b > 0.05) instReturns
                                                    |> Array.map fst |> Array.toList |> List.map (fun s -> s.Symbol)
                                                    |> Set.ofList
                          if filteredInstruments.Count > 0 then
                              let skipFirst = int(Math.Floor((float(lreturns.RowCount)) / 200.)) - skipLast
                              let avgReturn2 = validationSets cols lreturns skipFirst 0
                                               |> Array.zip (List.map (fun (s : Stock,_) -> s.Symbol) iprices |> Array.ofList)
                                               |> Array.filter (fun (s, r) -> filteredInstruments.Contains(s))
                              printfn "%s (%i instruments) avg: %f" sector (filteredInstruments.Count) (Array.average (Array.map snd avgReturn2))
                          )
    
    let slidingWindow skip step length (data: Matrix<_>) =
        seq {
            for i in skip..step..(data.RowCount - length) do
                yield data.SubMatrix(i, length, 0, data.ColumnCount)
        }

    let backTest step trainingWindow ireturns =
        let lreturns = lagged ireturns
        let width = ireturns.ColumnCount
        let length = lreturns.RowCount
        let window = slidingWindow 0 step (trainingWindow + 1) lreturns
                     |> Seq.map (fun (w: Matrix<_>) -> (w.SubMatrix(0, trainingWindow - step, 0, w.ColumnCount), w.SubMatrix(trainingWindow - step, 1, width, w.ColumnCount - width), w.SubMatrix(trainingWindow, 1, 0, width)))
        let avg = lreturns.SubMatrix(trainingWindow, length - trainingWindow, 0, lreturns.ColumnCount).ColumnSums() / (float (length - trainingWindow))
        let avg = avg.SubVector(0, width)
        let avgArray = avg.ToArray()
        let returns = window
                      |> Seq.map (fun (t, obs, act) ->
                                  let mean = dataSetMean t
                                  let covariance = dataSetCovariance mean t
                                  let rs = validationSingle mean covariance width obs act (Array.create width 0.002)
                                  // remove drift bias for the period from returns
                                  //let demeaned = ((DenseVector.ofArray rs) - avg).ToArray()
                                  rs
                                  )
                      |> Seq.toArray
        returns |> Array.mapi (fun i v -> ((ireturns.RowKeys |> Seq.toArray).[i * step + trainingWindow + 1], v) )

    type BackTestItem =
        {
            Group: string;
            Symbol: string;
            AnnualisedReturn: float;
            NumberOfWins: int;
            NumberOfLosses: int;
            MaxLoss : float;
            MaxGain : float;
            SingleTestpValue: float;
            MultiTestpValue: float;
            SharpeRatio: float;
            From: DateTime;
            To: DateTime
        }
    let saveTradeList (trades : (string * TradeSummary) list) (filePath: string) =
        let frame = Frame.ofRecords trades
                    |> Frame.expandAllCols 1
        frame.SaveCsv filePath

    let backTestRun fromDate toDate tradesOutputPath =
        let sampleCount = 5000
        let trainingWindow = 1250
        let groupedInstruments = instrumentsAll //[("CSCO", [Stock("CSCO","","","")] )] //instrumentsBySector
        List.map  (fun (group, instruments) -> 
                   let iprices = prices instruments (trainingWindow + (500 / period)) fromDate toDate
                   let instArray = iprices |> List.map fst |> Array.ofList
                   let ireturns = returnsFrame iprices
                   if ireturns.ColumnCount > 0 then
                       let returns = backTest period trainingWindow ireturns
                                     |> Seq.toList
                       let length = List.length returns
                       let fstDate = ireturns.RowKeys.First()
                       let lstDate = ireturns.RowKeys.Last()
                       let prsingle = 
                           let rArray = Array.ofList (List.map snd returns)
                           let rArray2D = Array2D.init length ((snd returns.Head).Length) (fun r c -> rArray.[r].[c].Return)
                           let Msampled = samplingDistributionBootstrapMultiple (returns.Length) sampleCount rArray2D
                           Array.init ((snd returns.Head).Length)
                                        (fun i ->
                                        let rlist = List.map (fun (t, rs: TradeSummary[]) -> rs.[i].Return) returns |> List.toArray
                                        let tradeList = List.map (fun (t: DateTime, rs: TradeSummary[]) -> (t.ToString("dd/MM/yyyy"), rs.[i])) returns
                                        saveTradeList tradeList (tradesOutputPath + (instArray.[i].Symbol) + "_trades.csv")
                                        let numberOfWins = Array.filter (fun v -> v > 0.) rlist |> Array.length
                                        let numberOfLosses = Array.filter (fun v -> v < 0.) rlist |> Array.length
                                        let rlistavg = Array.average rlist
                                        let rmaxloss = Array.min rlist
                                        let rmaxgain = Array.max rlist
                                        let sampled = rlist |> samplingDistributionBootstrapSingle (returns.Length) sampleCount
                                        let SpValue = (Array.filter (fun s -> s > rlistavg) sampled |> Array.length |> float) / (float (sampled.Length))
                                        let MpValue = (Array.filter (fun s -> s > rlistavg) Msampled |> Array.length |> float) / (float (Msampled.Length))
                                        let sharperatio = rlistavg / (MathNet.Numerics.Statistics.ArrayStatistics.StandardDeviation rlist) * (sqrt annualScaling)
                                        { Group = group; Symbol = instArray.[i].Symbol; AnnualisedReturn = rlistavg * annualScaling; NumberOfWins = numberOfWins; NumberOfLosses = numberOfLosses ; MaxLoss = rmaxloss; MaxGain = rmaxgain; SingleTestpValue = SpValue; MultiTestpValue = MpValue; SharpeRatio = sharperatio; From = fstDate; To = lstDate}
                                      )
                       printfn "Group:%s %s-%s\n%s" group (fstDate.ToString("dd/MM/yyyy")) (lstDate.ToString("dd/MM/yyyy")) (String.Join("\n", ((Array.map (fun item -> "(" + item.Symbol + "," + item.AnnualisedReturn.ToString() + "," + item.NumberOfWins.ToString() + "," + item.MaxGain.ToString() + "," + item.NumberOfLosses.ToString() + "," + item.MaxLoss.ToString() + "," + item.SingleTestpValue.ToString() + "," + item.MultiTestpValue.ToString()  + ","  + item.SharpeRatio.ToString() + ")") prsingle)).AsEnumerable()))
                       (group, prsingle)
                   else (group, Array.empty)
                   ) groupedInstruments
    //validationRun()
    let saveResults (results: (string * BackTestItem[]) list) (filePath : string) =
        let combined = results.SelectMany(fun (g, items) -> items.AsEnumerable())
        let frame = Frame.ofRecords combined
        frame.SaveCsv filePath
        printfn "Results saved to %s" filePath

    saveResults (backTestRun (DateTime(2000, 1, 1)) (DateTime(2010, 12 ,31)) @"c:\temp\testtrades\") @"c:\temp\backtestresults_bysector_test.csv" 
    saveResults (backTestRun (DateTime(2005, 1, 1)) (DateTime(2015, 12 ,31)) @"c:\temp\validationtrades\") @"c:\temp\backtestresults_bysector_validation.csv" 
    
    type ForecastItem =
        {
            Symbol : string;
            Return : float;
        }

    let forecast (date: DateTime) =
        let sampleCount = 5000
        let trainingWindow = 1250
        let groupedInstruments = instrumentsBySector
        List.map  (fun (group, instruments) -> 
                   let iprices = prices instruments trainingWindow (date.AddDays(float(-trainingWindow * 2))) date
                   let instArray = iprices |> List.map fst |> Array.ofList
                   let ireturns = returnsFrame iprices
                   if ireturns.ColumnCount > 0 then
                       let lreturns = lagged ireturns
                       let width = ireturns.ColumnCount
                       let length = lreturns.RowCount
                       let trainingSet = lreturns.SubMatrix(lreturns.RowCount - trainingWindow, trainingWindow, 0, lreturns.ColumnCount)
                       let mean = dataSetMean trainingSet
                       let covariance = dataSetCovariance mean trainingSet
                       let observed = (trainingSet.EnumerateRows().Last().SubVector(width, width))
                       let projection = conditionalMean mean covariance width observed
                       (group, Array.zip instArray (projection.ToArray()) |> Array.map (fun (s, r) -> { Symbol = s.Symbol; Return = r}))
                   else (group, Array.empty)
                   ) groupedInstruments
    
    let tomorrowForecast = (forecast (DateTime(2015,2,20))).SelectMany(fun (g, items) -> items.AsEnumerable()).ToArray()
    
    let saveForecast (data: ForecastItem seq) (filePath: string) =
        let frame = Frame.ofRecords data
        frame.SaveCsv filePath

    saveForecast tomorrowForecast @"c:\temp\forecast20150220.csv"
    let stop = ()