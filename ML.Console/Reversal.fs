namespace ML.Console
module Reversal =
    open System
    open System.Collections.Generic
    open SmartTrader.Data
    open SmartTrader.Domain
    open System.Threading
    open System.Threading.Tasks
    open System.Linq
    open Deedle

    open MathNet.Numerics.LinearAlgebra

    open CsvHelper

    open FSharp.Charting

    let pricesDir = Uri("file://../MarketData/prices/")

    let getInstruments () =
        LocalInstrumentSource(Uri("file://../MarketData/instruments.csv"))
            .GetInstruments(CancellationToken.None).Result

    let getPrices mapper min max instrument =
        let store = LocalMarketDataStore(pricesDir)
        let dateOf (item : TimeSeriesPrice) = item.Date
        store.GetAsync(instrument, min, max, CancellationToken.None)
             .ContinueWith<_>(fun (t: Task<seq<TimeSeriesPrice>>) -> Seq.map (fun item -> KeyValue.Create(dateOf item, mapper item)) t.Result)
             |> Async.AwaitTask

    let priceSeries mapper min max instrument =
        let loader = (fun (min, minb : Indices.BoundaryBehavior) (max, maxb : Indices.BoundaryBehavior) -> getPrices mapper min max instrument)
        DelayedSeries.Create(min, max, loader)

    let pricesFrameWithMissing mapper min max minCount instruments =
        let columnKeyOf (instrument : Instrument) = instrument.Symbol
        let result = Seq.map (fun instrument -> (columnKeyOf instrument, priceSeries mapper min max instrument)) instruments
                     |> Frame.ofColumns
        result.Columns.Where (fun kvp -> kvp.Value.ValueCount > minCount)
            |> Frame.ofColumns

    let pricesFrame mapper min max minCount instruments =
        pricesFrameWithMissing mapper min max minCount instruments
            |> Frame.dropSparseRows

    let closePrices min max =
        let mapper = fun (item : TimeSeriesPrice) -> item.AdjustedClose
        pricesFrame mapper min max

    let closePricesWithMissing min max =
        let mapper = fun (item : TimeSeriesPrice) -> item.AdjustedClose
        pricesFrameWithMissing mapper min max

    let instruments = getInstruments().OfType<Stock>() |> List.ofSeq

    let getInstrumentsBy f =
        getInstruments().OfType<Stock>() 
            |> Seq.groupBy f
            |> Seq.map (fun (k, i) -> (k, i |> Seq.toList))
            |> Seq.toList

    let instrumentsBySector =
        getInstrumentsBy (fun i -> i.Sector)

//    let covariance xs ys =
//        let ex = Seq.average xs
//        let ey = Seq.average ys
//        Seq.map2 (fun x y -> (x - ex) * (y - ey)) xs ys |> Seq.average
//
//    let variance (xs: float seq) =
//        MathNet.Numerics.Statistics.Statistics.Variance xs

    let beta xs ys =
        let ex = Seq.average xs
        let ey = Seq.average ys
        let nom = Seq.map2 (fun x y -> (x - ex) * (y - ey)) xs ys |> Seq.sum
        let denom = Seq.map (fun x -> (x - ex) * (x - ex)) xs |> Seq.sum
        nom / denom

    type OLSResult =
        {
            Intercept : float
            Slope : float
        }

    let ordinaryLeastSquares independent dependent =
        let xsAvg = Seq.average independent
        let ysAvg = Seq.average dependent
        let b = beta independent dependent
        { Intercept = ysAvg - xsAvg * b; Slope = b }

    type String with
        member x.asStock() = Stock(x, String.Empty, String.Empty, String.Empty)

    let depInstrument = "BMC"
    let indepInstrument = "HSP"
    let rec cumulativeReturns state returns =
        match returns with
        | [] -> []
        | r::tail -> let prod = state * (r + 1.)
                     prod::(cumulativeReturns prod tail)

    let rec drawdowns returns (high, low, duration) =
        match returns with
        | [] -> []
        | r::tail when r > high -> (r, r, 0)::(drawdowns tail (r, r, 0))
        | r::tail when r < low ->  (high, r, duration + 1)::(drawdowns tail (high, r, duration + 1))
        | r::tail -> (high, low, duration + 1)::(drawdowns tail (high, low, duration + 1))

    let pairTradingMovAvg (dataFrame: Frame<_,_>) (depsymb: string) (indepsymb: string) lag =
        //let dataFrame = closePrices (new DateTime(2006,4,24)) (new DateTime(2012,5,9)) 500 [depsymb.asStock() ; indepsymb.asStock()]
        let laggedSlope = Frame.windowInto lag (fun fr -> let xs = Series.values (fr.GetColumn(indepsymb))
                                                          let ys = Series.values (fr.GetColumn(depsymb))
                                                          let b = beta xs ys
                                                          b) dataFrame
        dataFrame.AddColumn("slope", laggedSlope)

        let spread = dataFrame.GetColumn(depsymb) - dataFrame?slope * dataFrame.GetColumn(indepsymb)

        dataFrame.AddColumn("spread", spread)

        let spreadMovAvg = Series.windowInto lag (fun sr -> Stats.mean sr) (Series.skip (lag - 1) dataFrame?spread)

        dataFrame.AddColumn("spreadmavg", spreadMovAvg)

        let spreadmovstddev = Series.windowInto lag (fun sr -> Stats.stdDev sr) (Series.skip (lag - 1) dataFrame?spread)

        dataFrame.AddColumn("spreadmovstddev", spreadmovstddev)

        let spreadZScore = -(dataFrame?spread - dataFrame?spreadmavg)/dataFrame?spreadmovstddev

        dataFrame.AddColumn("spreadZScore", spreadZScore)

        let gldpos = dataFrame?spreadZScore * -dataFrame?slope

        dataFrame.AddColumn("gldpos", gldpos)

        dataFrame.AddColumn("usopos", dataFrame?spreadZScore)

        dataFrame.AddColumn("gldret", log((dataFrame.GetColumn(indepsymb)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb)))) * (Series.shift 1 dataFrame?gldpos))
        dataFrame.AddColumn("usoret", log((dataFrame.GetColumn(depsymb)) / (Series.shift 1 (dataFrame.GetColumn(depsymb)))) * (Series.shift 1 dataFrame?usopos))

        dataFrame.AddColumn("totalret", (dataFrame?gldret + dataFrame?usoret) / (abs(Series.shift 1 dataFrame?gldpos) + abs(Series.shift 1 dataFrame?usopos)))

        let apr = (Stats.sum dataFrame?totalret) / (float dataFrame.RowCount) * 252.

        let sharpeRatio = apr / ((Stats.stdDev dataFrame?totalret) * sqrt(252.))
        printfn "MA (%s-%s) lag: %i APR: %f Sharpe: %f poscount: %i" depsymb indepsymb lag apr sharpeRatio (Stats.count dataFrame?totalret)
        (apr, sharpeRatio, lag, (Stats.count dataFrame?totalret), dataFrame)

    //let backtestMA = List.map (pairTradingMovAvg depInstrument indepInstrument) [5..5..100]

    let pairTradingBollingerBands (dataFrame: Frame<_,_>) (depsymb: string) (indepsymb: string) lag =
        let entryZScore = 1.;
        let exitZScore = 0.;
        let laggedSlope = Frame.windowInto lag (fun fr -> let xs = Series.values (fr.GetColumn(indepsymb))
                                                          let ys = Series.values (fr.GetColumn(depsymb))
                                                          let b = beta xs ys
                                                          b) dataFrame
        dataFrame.AddColumn("slope", laggedSlope)

        let spread = dataFrame.GetColumn(depsymb) - dataFrame?slope * dataFrame.GetColumn(indepsymb)

        dataFrame.AddColumn("spread", spread)

        let spreadMovAvg = Series.windowInto lag (fun sr -> Stats.mean sr) (Series.skip (lag - 1) dataFrame?spread)

        dataFrame.AddColumn("spreadmavg", spreadMovAvg)

        let spreadmovstddev = Series.windowInto lag (fun sr -> Stats.stdDev sr) (Series.skip (lag - 1) dataFrame?spread)

        dataFrame.AddColumn("spreadmovstddev", spreadmovstddev)

        let spreadZScore = -(dataFrame?spread - dataFrame?spreadmavg)/dataFrame?spreadmovstddev

        let iif cond truepart falsepart =
            if cond then truepart
            else falsepart
       
        dataFrame.AddColumn("spreadZScore", spreadZScore)
        
        let zscoreLookup = new Dictionary<DateTime, float>()
        let posDict = new Dictionary<DateTime, float>()
        spreadZScore |> Series.map (fun k v -> zscoreLookup.Add(k, v)
                                               if(v > entryZScore) then posDict.Add(k, -1.)
                                               else if (v < -entryZScore) then posDict.Add(k, 1.)
                                                    else posDict.Add(k, 0.)) |> ignore

        let keysArray = Array.ofSeq posDict.Keys
        keysArray 
        |> Array.iteri (fun i k -> if i > 0 then
                                    let keyprev = keysArray.[i-1]
                                    let key = keysArray.[i]
                                    if posDict.[keyprev] > 0. && zscoreLookup.[key] < 0. then
                                         posDict.[key] <- posDict.[keyprev]
                                    else if posDict.[keyprev] < 0. && zscoreLookup.[key] > 0. then
                                         posDict.[key] <- posDict.[keyprev]
                                         else ())
        
        dataFrame.AddColumn("positions", Series.ofObservations (Seq.map (fun (kvp: KeyValuePair<_,_>) -> (kvp.Key, kvp.Value)) posDict))

        let gldpos = dataFrame?positions * dataFrame?slope

        dataFrame.AddColumn("gldpos", gldpos)

        dataFrame.AddColumn("usopos", -dataFrame?positions)

        dataFrame.AddColumn("gldret", log((dataFrame.GetColumn(indepsymb)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb)))) * (Series.shift 1 dataFrame?gldpos))
        dataFrame.AddColumn("usoret", log((dataFrame.GetColumn(depsymb)) / (Series.shift 1 (dataFrame.GetColumn(depsymb)))) * (Series.shift 1 dataFrame?usopos))

        dataFrame.AddColumn("totalret", (dataFrame?gldret + dataFrame?usoret) / (abs(Series.shift 1 dataFrame?gldpos) + abs(Series.shift 1 dataFrame?usopos)))
        let apr = (Stats.sum dataFrame?totalret) / (float dataFrame.RowCount) * 252.

        let sharpeRatio = apr / ((Stats.stdDev dataFrame?totalret) * sqrt(252.))

        let returns = List.ofSeq (Series.values dataFrame?totalret)

        let cReturns = cumulativeReturns 1. returns

        let drList = drawdowns cReturns (1., 1., 0)

        let drList = drawdowns cReturns (1., 1., 0)

        let maxdrawdown = List.map (fun (h, l, _) -> log(l / h)) drList |> List.min
        let maxdrawdownDuration = List.map (fun (_, _, d) -> d) drList |> List.max
        let pos = List.ofSeq (Series.values dataFrame?positions)
        let longCount = List.filter (fun p -> p > 0.) pos |> List.length
        let shortCount = List.filter (fun p -> p < 0.) pos |> List.length
        (apr, sharpeRatio, maxdrawdown, maxdrawdownDuration, lag, shortCount, longCount, dataFrame)

    //let backtestBB = List.map (pairTradingBollingerBands depInstrument indepInstrument) [5..5..100]
    //dataFrame.SaveCsv(@"c:\temp\dataFrame.csv", includeRowKeys = true)

    let mucond (A: Matrix<_>) (mu: Vector<_>) =
        A * mu

    let sigmacond (A: Matrix<_>) (sigma: Matrix<_>) (Q: Matrix<_>) =
        A * sigma * (A.Transpose()) + Q

    let ypred (C: Matrix<_>) (mucond: Vector<_>) =
        C * mucond

    let R (r: Vector<_> list) (C: Matrix<_>) (sigmacond: Matrix<_>) =
        let rsum = List.fold (fun state v -> state + v) (DenseVector.zero (sigmacond.RowCount)) r
        let ravg = rsum / (float r.Length)
        let covsum = Seq.pairwise r 
                    |> Seq.map (fun (x, y) -> (x - ravg) * (y - ravg))
                    |> Seq.fold (fun state v -> state + v) (DenseVector.zero (sigmacond.RowCount))
        let cov = (covsum / (float r.Length)).ToColumnMatrix()
        cov - C * sigmacond * (C.Transpose())

    let kalmangain (sigmacond: Matrix<_>) (C: Matrix<_>) (R: Matrix<_>) =
        let Ctrans = C.Transpose()
        sigmacond * Ctrans * ((C * sigmacond * Ctrans + R).Inverse())

    let mu (mucond: Vector<_>) (K: Matrix<_>) (r: Vector<_>) =
        mucond + K * r

    let sigma (K: Matrix<_>) (C: Matrix<_>) (sigmacond: Matrix<_>) =
        let I = DenseMatrix.identity sigmacond.RowCount
        (I - K * C) * sigmacond

(*
According to Chan:
$\delta=0.0001\\
V_w=\frac{\delta}{1-\delta}I\\
V_e=0.001\\
\beta_0=\begin{pmatrix}0 & 0\end{pmatrix}\\
R_0=\begin{pmatrix}0 & 0 \\ 0 & 0 \end{pmatrix}\\
R_{t|0}=\begin{pmatrix}0 & 0 \\ 0 & 0 \end{pmatrix}\\
R_t=R_{t|t-1}+V_w\\
\hat y_t=x_t\beta_t\\
Q_t=x_tR_tx_t^T+V_e\\
e_t=y_t-\hat y_t\\
K=R_t*x_t^T\\
\beta_{t+1}=\beta_t+Ke_t\\
R_{t+1|t}=R_t-Kx_tR_t$
*)

(*
Chan's translated and simplified:
$A=I\\
\mu_{t|t-1}=A\mu_{t-1}=\mu_{t|t-1}\\
\delta=0.0001\\
Q=\frac{\delta}{1-\delta}I\\
\Sigma_{t|t-1}=A\Sigma_{t-1}A+Q=\Sigma_{t-1}+Q\\
D_t=I,
u_t=\begin{pmatrix}0\\0\end{pmatrix}\\
\hat y_t=C_t\mu_{t|t-1}+D_tu_t=C_t\mu_{t|t-1}\\
r_t=y_t-\hat y_t\\
R=0.001\\
S_t=C_t\Sigma_{t|t-1}C_t^T+R\\
K_t=\Sigma_{t|t-1}C_t^TS^{-1}\\
\mu_t=\mu_{t|t-1}+K_tr_t\\
\Sigma_t=(I-K_tC_t)\Sigma_{t|t-1}\\
\Sigma_0=I,\mu_0=\begin{pmatrix}0\\0\end{pmatrix}$
*)

    let rec evaluateKalman mu_p sigma_p Q R observations =
        match observations with
        | [] -> []
        | (key, o, C_t: Matrix<_>)::tail -> 
            let mucond_t = mu_p
            let sigmacond_t = sigma_p + Q
            let pred = C_t * mucond_t
            let r_t = o - pred
            let C_t_transposed = C_t.Transpose()
            let S_t = C_t * sigmacond_t * C_t_transposed + R
            let S_t_inv = Matrix.inverse S_t
            let K_t = (sigmacond_t * C_t_transposed) * S_t_inv
            let mu_t = mucond_t + K_t * r_t
            let sigma_t = sigmacond_t - K_t * C_t * sigmacond_t
            (key, mu_t, sigma_t, S_t, r_t)::(evaluateKalman mu_t sigma_t Q R tail)

    // Dependent is fst of observations
    let estimateSlopeKalman delta r (observations: (DateTime * float * float) seq) =
        let observationsList = 
            observations
            |> Seq.map (fun (key, dependent, independent) -> (key, (DenseVector.ofList [dependent]), DenseMatrix.ofColumnList [[independent]; [1.]]))
            |> Seq.toList
        let Q = (DenseMatrix.identity 2) * (delta/(1.-delta))
        let R = (DenseMatrix.identity 1) * r
        let sigma_0 = DenseMatrix.identity 2
        let mu_0 = DenseVector.create 2 0.
        let results = evaluateKalman mu_0 sigma_0 Q R observationsList
        results

    // Dependent is fst of observations
    let estimateSlopeKalmanMulti delta r (observations: (DateTime * float * (float list)) seq) =
        let observationsList = 
            observations
            |> Seq.map (fun (key, dependent, independents) -> (key, (DenseVector.ofList [dependent]), DenseMatrix.ofRowList ([List.concat [independents;[1.]]]) ))
            |> Seq.toList
        let Q = (DenseMatrix.identity 3) * (delta/(1.-delta))
        let R = (DenseMatrix.identity 1) * r
        let sigma_0 = DenseMatrix.identity 3
        let mu_0 = DenseVector.create 3 0.
        let results = evaluateKalman mu_0 sigma_0 Q R observationsList
        results

    let pairTradingKalmanFilter (dataFrame: Frame<_, _>) (depsymb: string) (indepsymb: string) (delta: float) (r:float) =
        let observations = Frame.mapRows (fun r s -> (r, s.GetAs(depsymb, 0.), s.GetAs(indepsymb, 0.))) dataFrame
                            |> Series.values
        let kalmanresults = estimateSlopeKalman delta r observations
                         
        let slope = kalmanresults
                        |> List.map (fun (key, mu: Vector<_>, _, _, _) -> (key, mu.[0]))
                        |> Series.ofObservations
        
        dataFrame.AddColumn("slope", slope)

        let s = kalmanresults
                        |> List.map (fun (key, _, _, S_t: Matrix<_>, _) -> (key, S_t.[0, 0]))
                        |> Series.ofObservations

        dataFrame.AddColumn("S_vect", s)

        let rs = kalmanresults
                        |> List.map (fun (key, _, _, _, r_t: Vector<_>) -> (key, r_t.[0]))
                        |> Series.ofObservations

        dataFrame.AddColumn("r", rs)

        let spread = dataFrame.GetColumn(depsymb) - dataFrame?slope * dataFrame.GetColumn(indepsymb)

        dataFrame.AddColumn("spread", spread)
       
        let positions = 
            dataFrame |>
            Frame.mapRows (fun k row -> if row?r > sqrt(row?S_vect) then
                                            if row?r > sqrt(row?S_vect) * 2. then -2.
                                            else -1.
                                        else if row?r < -sqrt(row?S_vect) then
                                            if row?r < -sqrt(row?S_vect) * 2. then 2.
                                            else 1.
                                        else 0.) 

        dataFrame.AddColumn("positions", positions)

        let gldpos = dataFrame?positions * -dataFrame?slope

        dataFrame.AddColumn("gldpos", gldpos)
        dataFrame.AddColumn("usopos", dataFrame?positions)
        dataFrame.AddColumn("gldret", (log((dataFrame.GetColumn(indepsymb)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb)))) - (0.05 / (dataFrame.GetColumn(indepsymb)) ) * abs((Series.shift 1 dataFrame?positions) - dataFrame?positions) ) * (Series.shift 1 dataFrame?gldpos))
        dataFrame.AddColumn("usoret", (log((dataFrame.GetColumn(depsymb)) / (Series.shift 1 (dataFrame.GetColumn(depsymb)))) - (0.05 / (dataFrame.GetColumn(depsymb)) ) * abs((Series.shift 1 dataFrame?positions) - dataFrame?positions) ) * (Series.shift 1 dataFrame?usopos))
        dataFrame.AddColumn("totalret", (dataFrame?gldret + dataFrame?usoret) / (abs(Series.shift 1 dataFrame?gldpos) + abs(Series.shift 1 dataFrame?usopos)))

        dataFrame.AddColumn("gldposP", -abs(gldpos))
        dataFrame.AddColumn("usoposP", abs(dataFrame?positions))
        dataFrame.AddColumn("gldretP", (log((dataFrame.GetColumn(indepsymb)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb)))) ) * (Series.shift 1 dataFrame?gldposP))
        dataFrame.AddColumn("usoretP", (log((dataFrame.GetColumn(depsymb)) / (Series.shift 1 (dataFrame.GetColumn(depsymb)))) ) * (Series.shift 1 dataFrame?usoposP))
        dataFrame.AddColumn("totalretP", (dataFrame?gldretP + dataFrame?usoretP) / (abs(Series.shift 1 dataFrame?gldposP) + abs(Series.shift 1 dataFrame?usoposP)))
        let aprP = (Stats.sum dataFrame?totalretP) / (float dataFrame.RowCount) * 252.

        dataFrame.AddColumn("gldposN", abs(gldpos))
        dataFrame.AddColumn("usoposN", -abs(dataFrame?positions))
        dataFrame.AddColumn("gldretN", (log((dataFrame.GetColumn(indepsymb)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb)))) ) * (Series.shift 1 dataFrame?gldposN))
        dataFrame.AddColumn("usoretN", (log((dataFrame.GetColumn(depsymb)) / (Series.shift 1 (dataFrame.GetColumn(depsymb)))) ) * (Series.shift 1 dataFrame?usoposN))
        dataFrame.AddColumn("totalretN", (dataFrame?gldretN + dataFrame?usoretN) / (abs(Series.shift 1 dataFrame?gldposN) + abs(Series.shift 1 dataFrame?usoposN)))
        
        let aprN = (Stats.sum dataFrame?totalretN) / (float dataFrame.RowCount) * 252.

        let apr = (Stats.sum dataFrame?totalret) / (float dataFrame.RowCount) * 252.

        let sharpeRatio = apr / ((Stats.stdDev dataFrame?totalret) * sqrt(252.))

        let returns = List.ofSeq (Series.values dataFrame?totalret)

        let cReturns = cumulativeReturns 1. returns

        let drList = drawdowns cReturns (1., 1., 0)

        let maxdrawdown = List.map (fun (h, l, _) -> log(l / h)) drList |> List.min
        let maxdrawdownDuration = List.map (fun (_, _, d) -> d) drList |> List.max
        let pos = List.ofSeq (Series.values dataFrame?positions)
        let longCount = List.filter (fun p -> p > 0.) pos |> List.length
        let shortCount = List.filter (fun p -> p < 0.) pos |> List.length
        printfn "K(%s-%s) APR: %f Sharpe: %f delta: %f r: %f poscount: %i" depsymb indepsymb apr sharpeRatio delta r (Stats.count dataFrame?totalret)
        //dataFrame.SaveCsv ("c:\\temp\\ewcewa.csv", includeRowKeys = true)
        (apr, sharpeRatio, maxdrawdown, maxdrawdownDuration, delta, r, shortCount, longCount, aprN, aprP, dataFrame)

    let pairTradingKalmanFilter3 (dataFrame: Frame<_, _>) (depsymb: string) (indepsymb1: string) (indepsymb2: string) (delta: float) (r:float) =
        let observations = Frame.mapRows (fun r s -> (r, s.GetAs(depsymb, 0.), [s.GetAs(indepsymb1, 0.);s.GetAs(indepsymb2, 0.)])) dataFrame
                            |> Series.values
        let kalmanresults = estimateSlopeKalmanMulti delta r observations
                         
        let slope1 = kalmanresults
                        |> List.map (fun (key, mu: Vector<_>, _, _, _) -> (key, mu.[0]))
                        |> Series.ofObservations
        
        dataFrame.AddColumn("slope1", slope1)

        let slope2 = kalmanresults
                        |> List.map (fun (key, mu: Vector<_>, _, _, _) -> (key, mu.[1]))
                        |> Series.ofObservations
        
        dataFrame.AddColumn("slope2", slope2)

        let s = kalmanresults
                        |> List.map (fun (key, _, _, S_t: Matrix<_>, _) -> (key, S_t.[0, 0]))
                        |> Series.ofObservations

        dataFrame.AddColumn("S_vect", s)

        let rs = kalmanresults
                        |> List.map (fun (key, _, _, _, r_t: Vector<_>) -> (key, r_t.[0]))
                        |> Series.ofObservations

        dataFrame.AddColumn("r", rs)

        let spread = dataFrame.GetColumn(depsymb) - dataFrame?slope1 * dataFrame.GetColumn(indepsymb1) - dataFrame?slope2 * dataFrame.GetColumn(indepsymb2)

        dataFrame.AddColumn("spread", spread)
       
        let positions = 
            dataFrame |>
            Frame.mapRows (fun k row -> if row?r > sqrt(row?S_vect) then -1.
                                        else if row?r < -sqrt(row?S_vect) then 1.
                                        else 0.) 

        dataFrame.AddColumn("positions", positions)

        let indepSymPos1 = dataFrame?positions * -dataFrame?slope1
        let indepSymPos2 = dataFrame?positions * -dataFrame?slope2

        dataFrame.AddColumn("indepSymPos1", indepSymPos1)
        dataFrame.AddColumn("indepSymPos2", indepSymPos2)
        dataFrame.AddColumn("usopos", dataFrame?positions)
        dataFrame.AddColumn("indepsymb1ret", (log((dataFrame.GetColumn(indepsymb1)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb1)))) - (0.05 / (dataFrame.GetColumn(indepsymb1)) ) * abs((Series.shift 1 dataFrame?positions) - dataFrame?positions) ) * (Series.shift 1 dataFrame?indepSymPos1))
        dataFrame.AddColumn("indepsymb2ret", (log((dataFrame.GetColumn(indepsymb2)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb2)))) - (0.05 / (dataFrame.GetColumn(indepsymb2)) ) * abs((Series.shift 1 dataFrame?positions) - dataFrame?positions) ) * (Series.shift 1 dataFrame?indepSymPos2))
        dataFrame.AddColumn("usoret", (log((dataFrame.GetColumn(depsymb)) / (Series.shift 1 (dataFrame.GetColumn(depsymb)))) - (0.05 / (dataFrame.GetColumn(depsymb)) ) * abs((Series.shift 1 dataFrame?positions) - dataFrame?positions) ) * (Series.shift 1 dataFrame?usopos))
        dataFrame.AddColumn("totalret", (dataFrame?indepsymb1ret + dataFrame?indepsymb2ret + dataFrame?usoret) / (abs(Series.shift 1 dataFrame?indepSymPos1) + abs(Series.shift 1 dataFrame?indepSymPos2) + abs(Series.shift 1 dataFrame?usopos)))

        dataFrame.AddColumn("indepSym1PosP", -abs(indepSymPos1))
        dataFrame.AddColumn("indepSym2PosP", -abs(indepSymPos2))
        dataFrame.AddColumn("usoposP", abs(dataFrame?positions))
        dataFrame.AddColumn("indepsymb1retP", (log((dataFrame.GetColumn(indepsymb1)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb1)))) ) * (Series.shift 1 dataFrame?indepSym1PosP))
        dataFrame.AddColumn("indepsymb2retP", (log((dataFrame.GetColumn(indepsymb2)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb2)))) ) * (Series.shift 1 dataFrame?indepSym2PosP))
        dataFrame.AddColumn("usoretP", (log((dataFrame.GetColumn(depsymb)) / (Series.shift 1 (dataFrame.GetColumn(depsymb)))) ) * (Series.shift 1 dataFrame?usoposP))
        dataFrame.AddColumn("totalretP", (dataFrame?indepsymb1retP + dataFrame?usoretP + dataFrame?indepsymb2retP) / (abs(Series.shift 1 dataFrame?indepSym1PosP) + abs(Series.shift 1 dataFrame?indepSym2PosP) + abs(Series.shift 1 dataFrame?usoposP)))
        let aprP = (Stats.sum dataFrame?totalretP) / (float dataFrame.RowCount) * 252.

        dataFrame.AddColumn("indepSym1PosN", abs(indepSymPos1))
        dataFrame.AddColumn("indepSym2PosN", abs(indepSymPos2))
        dataFrame.AddColumn("usoposN", -abs(dataFrame?positions))
        dataFrame.AddColumn("indepsymb1retN", (log((dataFrame.GetColumn(indepsymb1)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb1)))) ) * (Series.shift 1 dataFrame?indepSym1PosN))
        dataFrame.AddColumn("indepsymb2retN", (log((dataFrame.GetColumn(indepsymb2)) / (Series.shift 1 (dataFrame.GetColumn(indepsymb2)))) ) * (Series.shift 1 dataFrame?indepSym2PosN))
        dataFrame.AddColumn("usoretN", (log((dataFrame.GetColumn(depsymb)) / (Series.shift 1 (dataFrame.GetColumn(depsymb)))) ) * (Series.shift 1 dataFrame?usoposN))
        dataFrame.AddColumn("totalretN", (dataFrame?indepsymb1retN + dataFrame?indepsymb2retN + dataFrame?usoretN) / (abs(Series.shift 1 dataFrame?indepSym1PosN) + abs(Series.shift 1 dataFrame?indepSym2PosN) + abs(Series.shift 1 dataFrame?usoposN)))
        
        let aprN = (Stats.sum dataFrame?totalretN) / (float dataFrame.RowCount) * 252.

        let apr = (Stats.sum dataFrame?totalret) / (float dataFrame.RowCount) * 252.

        let sharpeRatio = apr / ((Stats.stdDev dataFrame?totalret) * sqrt(252.))

        let returns = List.ofSeq (Series.values dataFrame?totalret)

        let cReturns = cumulativeReturns 1. returns

        let drList = drawdowns cReturns (1., 1., 0)

        let maxdrawdown = List.map (fun (h, l, _) -> log(l / h)) drList |> List.min
        let maxdrawdownDuration = List.map (fun (_, _, d) -> d) drList |> List.max
        let pos = List.ofSeq (Series.values dataFrame?positions)
        let longCount = List.filter (fun p -> p > 0.) pos |> List.length
        let shortCount = List.filter (fun p -> p < 0.) pos |> List.length
        //printfn "K(%s-%s) APR: %f Sharpe: %f delta: %f poscount: %i" depsymb indepsymb apr sharpeRatio delta (Stats.count dataFrame?totalret)
        //dataFrame.SaveCsv ("c:\\temp\\ewcewa.csv", includeRowKeys = true)
        (apr, sharpeRatio, maxdrawdown, maxdrawdownDuration, delta, r, shortCount, longCount, aprN, aprP, dataFrame)

    //let backtest = List.map (fun delta -> pairTradingKalmanFilter depInstrument indepInstrument delta 0.001) [0.0001..(-0.0000001)..0.] //[0.000002]

    (*Good spreads:
    EMR-PH
    BK-PFG
    PFG-BK
    GNW-MET
    APD-DNR
    PG-SJM
    BMC-MOS
    SCHW-WM
    VNO-WM
    IVZ-MOLX
    SPG-TJX
    BMC-HSP
    *)
    let stop = ()