namespace ML.Console
module Momentum =
    open System
    open System.Collections.Generic
    open SmartTrader.Data
    open SmartTrader.Domain
    open System.Threading
    open System.Threading.Tasks
    open System.Linq
    open Deedle

    let pricesDir = Uri("file://../MarketData/prices/")

    let getInstruments () =
        LocalInstrumentSource(Uri("file://../MarketData/instruments.csv"))
            .GetInstruments(CancellationToken.None)
            |> Async.AwaitTask

    let getPrices mapper min max instrument =
        let store = LocalMarketDataStore(pricesDir)
        let dateOf (item : TimeSeriesPrice) = item.Date
        store.GetAsync(instrument, min, max, CancellationToken.None)
             .ContinueWith<_>(fun (t: Task<seq<TimeSeriesPrice>>) -> Seq.map (fun item -> KeyValue.Create(dateOf item, mapper item)) t.Result)
             |> Async.AwaitTask

    let priceSeries mapper min max instrument =
        let loader = (fun (min, minb : Indices.BoundaryBehavior) (max, maxb : Indices.BoundaryBehavior) -> getPrices mapper min max instrument)
        DelayedSeries.Create(min, max, loader)

    let pricesFrame mapper min max minCount instruments =
        let columnKeyOf (instrument : Instrument) = instrument.Symbol
        let result = Seq.map (fun instrument -> (columnKeyOf instrument, priceSeries mapper min max instrument)) instruments
                     |> Frame.ofColumns
        result.Columns.Where (fun kvp -> kvp.Value.ValueCount > minCount)
            |> Frame.ofColumns
            |> Frame.dropSparseRows

    let closePrices min max instruments =
        let mapper = fun (item : TimeSeriesPrice) -> item.AdjustedClose
        pricesFrame mapper min max instruments

    let priceReturns prices shift =
        prices / (Frame.shift shift prices) |> log

    let returnsForPeriod prices period =
        let result = priceReturns prices period
                     |> Frame.dropSparseRows
        result.IndexColumnsWith (Seq.map (fun c -> c + "_" + (string period)) result.ColumnKeys)
   
    let prices =
        let instruments = Async.RunSynchronously (getInstruments())
        closePrices DateTime.MinValue DateTime.MaxValue 1250 instruments

    let vsReturns period1 period2 =
        let result = Frame.merge (returnsForPeriod prices period1) ((returnsForPeriod prices period2).Shift(-period2))
                     |> Frame.dropSparseRows
        result

    let saveCompare period1 period2 =
        let returns = vsReturns period1 period2
        returns.SaveCsv (@"c:\temp\" + (string period1) + "vs" + (string period2) + ".csv", includeRowKeys = true)

    saveCompare 250 20
    saveCompare 125 20
    //saveCompare 60 20
    //saveCompare 20 20
    //saveCompare 250 5
    //saveCompare 125 5
    //saveCompare 60 5
    //saveCompare 20 5
    //saveCompare 5 5

    let stop = ()