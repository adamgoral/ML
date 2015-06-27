namespace ML.Console
module MM2 =
    open System
    open System.Collections.Generic
    open SmartTrader.Data
    open SmartTrader.Domain
    open System.Threading
    open System.Threading.Tasks
    open System.Linq
    open Deedle

    type OHLC =
        {
            Open : float;
            High : float;
            Low : float;
            Close : float;
            AdjClose : float;
        }

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

    let pricesFrame mapper min max instruments =
        let columnKeyOf (instrument : Instrument) = instrument.Symbol
        Seq.map (fun instrument -> (columnKeyOf instrument, priceSeries mapper min max instrument)) instruments
            |> Frame.ofColumns

    let allInstrumentsPrices min max mapper =
        let instruments = Async.RunSynchronously (getInstruments())
        let result = pricesFrame mapper min max instruments
        result.Columns.Where (fun kvp -> kvp.Value.ValueCount > 0)
            |> Frame.ofColumns

    let closePrices =
        let mapper = fun (item : TimeSeriesPrice) -> item.AdjustedClose
        allInstrumentsPrices (DateTime(2007, 1, 1)) (DateTime.Today) mapper

    let closeDiff shift =
        closePrices - (Frame.shift shift closePrices)

    closePrices.SaveCsv @"c:\temp\adjcloseprices.csv"

    closePrices.Shift(5).SaveCsv @"c:\temp\shifted.csv"

    (closeDiff 5).SaveCsv @"c:\temp\shiftcloseprices.csv"

    let res = 3

    let stop = ()
