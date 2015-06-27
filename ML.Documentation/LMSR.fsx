(*** hide ***)
#I ".."
#load "packages/FsLab.0.0.16/FsLab.fsx"
#load @"..\ML.Documentation\ML.Documentation.fsx"

(**
LMSR Experiment
==

$C(x) =b\log \left( \sum_{k=1}^K \pi_k e^{\frac{x_k}{b}} \right)$ is cost function

where:

$x=\begin{pmatrix}
x_1\\
x_2\\
\vdots\\
x_K\\
\end{pmatrix}$ is payout column vector

$\pi_k=p(x_k|D,\theta)$ prior probability of x_k outcome

$b \in \mathbb{R^+}$ is liquidity parameter
*)

let C x (pi: float[]) b =
    let sum = Array.mapi (fun i v -> pi.[i] * (exp (v / b))) x
              |> Array.sum
    b * (log sum)

(**
$price=C(x^{(1)})-C(x^{(0)})$

where:

$x^{(0)}$ is prior payout column vector

$x^{(1)}$ is payout column vector that includes effects of change in state
*)

let price x0 x1 (pi: float[]) b =
    (C x1 pi b) - (C x0 pi b)

(**
#Small digression:

$E[x]=\int p(x) \times x \,\mathbb{d}x\\
C(f(x))=b\ln \left(\int p(x) e^{\frac{f(x)}{b}} \, \mathbb{d}x \right)=b\ln \mathbb{E} \left[e^{\frac{f(x)}{b}}\right]$

price can also be expressed as:

$C(f^{(1)}(x))-C(f^{(0)}(x))= b\ln \mathbb{E} \left[e^{\frac{f^{(1)}(x)}{b}}\right] - b\ln \mathbb{E} \left[e^{\frac{f^{(0)}(x)}{b}}\right]
=b \ln\left( \frac{\mathbb{E} \left[e^{\frac{f^{(1)}(x)}{b}}\right]}{\mathbb{E} \left[e^{\frac{f^{(0)}(x)}{b}}\right]}\right)$

where expectation can be calculated using MLE:

$g(x)=e^{\frac{f(x)}{b}}\,,\bar g = \frac{1}{N} \sum_{x \in D} g(x) \,, N=\sum_x \mathbb{I}( x\in D )$

#Option price implied probability

$pv_s=\begin{pmatrix}
pv_{s,1} \times oi_{s,1}\\
pv_{s,2} \times oi_{s,2}\\
\vdots\\
pv_{s,K} \times oi_{s,K}
\end{pmatrix}\\
pv_{s,k}=\begin{cases}
put & max(0,S-k)\\
call & max(0,k-S)
\end{cases}$

where $oi_{s,i}$ is open interest of put or call for given option expiry

implied probability can be computed using:

$p(k|D)=\frac{\mathbb{e}^{\frac{pv_k}{b}}}{\sum_{n=1}^K \mathbb{e}^{\frac{pv_n}{b}}}$

Points to consider for implying probability:
Short term expiry
Long term expiry
Inclusion of number of outstanding shares of underlier (for long).
Inclusion of number of shorted shares (where to get this information).

In case of implying probability for period that spans more than single expiry chain,
the probability distribution is a mixture of individual expiry components.

Alternatively probability density can be inferred from prices using Black-Scholes.

$d_1=\frac{log(\frac{x}{k})+(r-D+\frac{1}{2}\sigma^2)(T-t)}{\sigma(T-t)^{\frac{1}{2}}}\\
d_2=\frac{log(\frac{k}{x})+(r-D-\frac{1}{2}\sigma^2)(T-t)}{\sigma(T-t)^{\frac{1}{2}}}\\
cnd(x)=\frac{1}{(2\pi)^{\frac{1}{2}}}\int_{-\infty}^x \mathbb{e}^{-\frac{1}{2} \phi^2} d\phi\\
Call=x\times \mathbb{e}^{-D(T-t)}cnd(d_1)-k\times \mathbb{e}^{-r(T-t)}cnd(d_2)\\
Put(x)=-x\times \mathbb{e}^{-D(T-t)}cnd(-d_1)+k\times \mathbb{e}^{-r(T-t)}cnd(-d_2)$
$ndf(x)=\frac{1}{(2\pi)^{\frac{1}{2}}}\mathbb{e}^{-\frac{1}{2}x^2}$

Probability density can be expressed as:
$p(x)=ndf(d_2)$

In case of implied volatility from price k: $p(x|k)=ndf(d_2|k)$
*)

open MathNet.Numerics.Distributions

let d1 x k r d sigma dt =
    let nom = log(x/k) + (r - d + 1./2.*(pown sigma 2)) * dt
    let den = sigma * (sqrt dt)
    nom / den

let d2 x k r d sigma dt =
    let nom = log(x/k) + (r - d - 1./2.*(pown sigma 2)) * dt
    let den = sigma * (sqrt dt)
    nom / den

let cnd x = Normal.CDF (0., 1., x)

let ndf x = Normal.PDF (0., 1., x)

let call x k r d sigma dt =
    x * exp(-d * dt) * cnd(d1 x k r d sigma dt) - k * exp (-r * dt) * cnd(d2 x k r d sigma dt)

let put x k r d sigma dt =
    - x * exp(-d * dt) * cnd(- d1 x k r d sigma dt) + k * exp (-r * dt) * cnd(- d2 x k r d sigma dt)

(*** define-output:callPriceChart ***)
open FSharp.Charting
let vol = 0.5
let T = 1.
let d = 0.
let r = 0.
let dataRange = Seq.map float [50..200]
let timeRange = Seq.map (fun x -> (float x) / 5.) [0..5]
Chart.Combine(Seq.map (fun t -> 
    Chart.Line(Seq.map (fun x -> (x, call x 100. r d vol (T-t))) dataRange, Name = "t=" + t.ToString()).WithLegend()) timeRange)
      .WithTitle("Call price chart")
(*** include-it:callPriceChart ***)
(*** define-output:putPriceChart ***)
Chart.Combine(Seq.map (fun t -> 
    Chart.Line(Seq.map (fun x -> (x, put x 100. r d vol (T-t))) dataRange, Name = "t=" + t.ToString()).WithLegend()) timeRange)
      .WithTitle("Put price chart")
(*** include-it:putPriceChart ***)

(**
To infer implied volatility, first we need to define price derivative with respect to volatility:

$\frac{\partial price}{\partial \sigma}=vega=x(T-t)^{\frac{1}{2}}\mathbb{e}^{-d(T-t)}ndf(d_1)$

For now I will use simple Newton-Raphson method to find implied volatility:
*)

let vega x k r d sigma dt =
    x * (sqrt dt) * exp (- d * dt) * ndf(d1 x k r d sigma dt)

(*** define-output:vegaChart ***)
Chart.Combine(Seq.map (fun t -> 
    Chart.Line(Seq.map (fun x -> (x, vega x 100. r d vol (T-t))) dataRange, Name = "dt=" + (T - t).ToString()).WithLegend()) timeRange)
      .WithTitle("vega chart")
(*** include-it:vegaChart ***)

let impVolCall price x k r d dt =
    let minError = 0.01
    let dv = minError + 1.
    let startingVol = 0.2
    let rec impVolRec sigma error n =
        if((abs error) < minError || n > 100) then sigma
        else
            let error = (call x k r d sigma dt) - price
            let vega = vega x k r d sigma dt
            let dv = error / (vega * 100.)
            impVolRec (sigma - dv) error (n+1)
    impVolRec startingVol dv 1

let impVolPut price x k r d dt =
    let minError = 0.01
    let dv = minError + 1.
    let startingVol = 0.2
    let rec impVolRec sigma error n =
        if((abs error) < minError || n > 100 ) then sigma
        else
            let error = (put x k r d sigma dt) - price
            let vega = vega x k r d sigma dt
            let dv = error / (vega * 100.)
            impVolRec (sigma - dv) error (n+1)
    impVolRec startingVol dv 1

open System
open System.Threading
open System.Threading.Tasks

let optionsDataDir = Uri @"..\MarketData\Options\"

open SmartTrader.Data
open SmartTrader.Domain

let loadOptionChain (instrument: Instrument) (date: DateTime) =
    let source = new LocalOptionDataStore(optionsDataDir)
    source.GetAsync(instrument, date, CancellationToken.None).Result

let instrumentMarketDates (instrument: Instrument) =
    let source = new LocalOptionDataStore(optionsDataDir)    
    source.GetDatesAsync(instrument, CancellationToken.None).Result

let spy = Instrument "SPY"

let availableDates = instrumentMarketDates spy

open System.Linq

let options = 
     let date = availableDates.First()
     loadOptionChain spy date

let mid (b: Nullable<float>) (a:Nullable<float>) =
    if b.HasValue && a.HasValue then Some((b.Value + a.Value)/2.)
    else None

let mids opts = Seq.filter (fun (o: OptionQuote) -> o.OpenInterest.HasValue && (o.OpenInterest.Value > 0)) opts
               |> Seq.map (fun (o: OptionQuote) -> (o.Strike, mid (o.Bid) (o.Ask)))
               |> Seq.filter (fun (_, v) -> v.IsSome)
               |> Seq.map (fun (s, v) -> (s, v.Value))

let impVolForExpiry (date: DateTime) underlyingPrice (chain: OptionChain) =
    let outOTMImpVols price callVols putVols =
        Seq.append
            (Seq.filter (fun (strike, _) -> strike < price) putVols)
            (Seq.filter (fun (strike, _) -> strike > price) callVols)
    let dt = ((chain.Expiry) - date).TotalDays / 365.
    let putMids = mids (chain.Puts)
    let callMids = mids (chain.Calls)
    let impCallVols = Seq.map (fun (s, p) -> (s, impVolCall p underlyingPrice s r d dt)) callMids
    let impPutVols = Seq.map (fun (s, p) -> (s, impVolPut p underlyingPrice s r d dt)) putMids
    outOTMImpVols underlyingPrice impCallVols impPutVols

let impVols (options: InstrumentOptions) =
    let underlyingPrice = options.InstrumentPrice
    let date = options.Date
    Seq.map (fun chain -> impVolForExpiry date underlyingPrice chain) options.Chains

(*** define-output:impVolChart ***)
Chart.Combine(Seq.map (fun chain -> Chart.Line(impVolForExpiry options.Date options.InstrumentPrice chain, Name = chain.Expiry.ToShortDateString()).WithLegend()) (options.Chains))
    .WithTitle("Imp vol chart")
(*** include-it:impVolChart ***)

