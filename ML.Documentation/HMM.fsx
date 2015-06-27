(*** hide ***)
#I ".."
#load "ML.Documentation.fsx"
#load @"packages\Deedle\Deedle.fsx"
#load @"packages\FSharp.Charting\FSharp.Charting.fsx"
let dataDir = @"..\Data\Market\"

(**
Hidden Markov Model Experiment
==

First I will cover Baum-Welch algorithm.

Given:

$N$ number of states. In this exampe I will set it to 5.

*)

let N = 5

(**
$T$ number of sequential observations

$X_t$ state at time $t$

$Y_t$ observation at time $t$
*)
open Deedle
open System

let loadStock symbol =
        Frame.ReadCsv (dataDir + symbol + ".csv")
        |> Frame.indexRowsDate "Date"

let dailyReturns (frame : Frame<DateTime, string>) =
    let returns = Seq.zip frame?Close.Values frame?Open.Values
    returns |> Seq.map (fun (c, o) -> log(c/o))
    //Seq.pairwise close |> Seq.map (fun (p, n) -> log(n/p))

let observe symbol =
    loadStock symbol
    |> dailyReturns
    |> Array.ofSeq

let windowSize = 30
let stockSymbol = "GLD"

let Y = Array.sub (observe stockSymbol) 0 windowSize

let T = Y.Length
(**
$A=\begin{pmatrix}
a_{1,1} & a_{1,2} & \cdots & a_{1,N} \\
a_{2,1} & a_{2,2} & \cdots & a_{2,N} \\
\vdots & \vdots & \ddots & \vdots \\
a_{N,1} & a_{N,2} & \cdots & a_{N,N}
\end{pmatrix}$ state transition stochastic matrix where 
$a_{i,j}=p(X_t=j|X_{t-1}=i)$

in this example this will be set to uniform prior
*)
let A = Array2D.init N N (fun _ _ -> 1. / (float N))
(**
$B=\begin{pmatrix}
b_1 & b_2 & \cdots & b_N
\end{pmatrix}$ emission probability vector

$b_{n}(y)=p(y|x=n)$ probability distribution of observed variable given state

In this example I will use scalar Gaussian pdf
$p(y|\mu,\sigma^2)=\frac{1}{\sqrt{2\pi}\sigma}e^{\left( \frac{1}{2\sigma^2}(y-\mu)^2 \right)}$
*)

let gaussianPdf mu sigma =
    let normalizer = 1. / (sqrt(2. * Math.PI) * sigma)
    let exponent = - 1./(2. * Math.Pow(sigma, 2.))
    (fun x -> Math.Exp(exponent * Math.Pow(x - mu, 2.)) * normalizer)

let observationMean =
    (Seq.sum Y) / (float T)
(*** include-value:observationMean ***)
let stdDev = (Seq.map (fun y -> Math.Pow(y - observationMean, 2.)) Y |> Seq.sum) / (float T - 1.) |> sqrt
(*** include-value:stdDev ***)
let mu =
    let min = 0. - (stdDev * 2.)
    let range = stdDev * 4.
    let interval = range / (float N)
    Array.init N (fun n -> min + interval * (float n))
(*** include-value:mu ***)
let B =
    Array.init N (fun n -> gaussianPdf mu.[n] stdDev)

(*** define-output:priorichart ***)
open FSharp.Charting

let data = seq { for x in (Array.min Y)..(((Array.max Y) - (Array.min Y)) / 100.)..((Array.max Y)*2.) -> x }
Chart.Combine(
  [for n in 0..(N-1) -> Chart.Line( Seq.map (fun x -> (x, B.[n](x))) data, Name="state" + string(n + 1)) ])
  .WithLegend()
  .WithTitle("Prior distributions")

(*** include-it:priorichart ***)

let By = 
    let arr = [for t in 0..(T-1) -> let bRow = [for n in 0..(N-1) -> B.[n](Y.[t])]
                                    let rowTotal = List.sum bRow
                                    List.map (fun v -> v / rowTotal) bRow |> Array.ofList]
    Array2D.init T N (fun t n -> arr.[t].[n])
(**

$\pi=\begin{pmatrix}
\pi_1 & \pi_2 & \cdots & \pi_N
\end{pmatrix}$ initial state probability vector where each element $\pi_n=p(X_1=n)$ 

in this example I will set it to uniform prior
*)
let pi = Array.init N (fun _ -> 1. / (float N))
(*** include-value:pi ***)
(**
$\theta=(A,B,\pi)$ is tuple of parameters set initially to some priors
*)
let theta = (A, B, pi)
(**
##Forward procedure

Goal is to calculate joint probability of observed variables from begining up to and inclusive of $t$ and state at time $t$ given the parameters $\theta$.
$\alpha_n(t)=p(y_1,y_2,\cdots,y_t,x_t=n|\theta)$

this can be expressed recursively:

$\alpha_i(1)=\pi_i \times b_i(y_1)\\
\alpha_j(t+1)=b_j(y_{t+1}) \times \sum_{i=1}^{N} \alpha_i(t) \times a_{i,j}$
*)

let alpha = 
    let result = Array2D.zeroCreate T N

    [for t in 0..(T-1) -> [for n in 0..(N-1) -> if t = 0 then result.[t,n] <- pi.[n] * By.[t,n]
                                                else
                                                    result.[t, n] <- By.[t,n] * List.sum [for i in 0..(N-1) -> result.[t-1,i] * A.[i,n]]
                          ]] |> ignore
    result

(**
##Backward procedure

The goal is to calculate joint probability of observed variables from $t+1$ up to $T$ given state at time $t$ and parameters $\theta$

$\beta_n(t)=p(y_{t+1},y_{t+2},\cdots,y_T|x_t=n,\theta)$

again this can be expressed recursively:

$\beta_i(T)=1\\
\beta_i(t)=\sum_{j=1}^{N} \beta_j(t+1) \times a_{i,j} \times b_j(y_{t+1})$

*)

let beta = 
    let result = Array2D.zeroCreate T N
    [for t in (T-1)..(-1)..0 -> [for i in 0..(N-1) -> if t = T - 1 then result.[t, i] <- 1.
                                                      else
                                                        result.[t, i] <- List.sum [for j in 0..(N-1) -> result.[t + 1, j] * A.[i, j] * By.[t + 1, j]]
                                ]] |> ignore
    result
(**

The posterior parameters can be inferred by:

1 Computing state probability at time $t$ given observations and priors

$\gamma_i(t)=p(X_t=i|Y,\theta)
=\frac{\alpha_i(t)\times\beta_i(t)}{\sum_{j=1}^{N}\alpha_j(t)\times\beta_j(t)}$
*)
let gamma i t =
    alpha.[t,i] * beta.[t,i] / List.sum [for j in 0..(N-1) -> alpha.[t,j] * beta.[t,j]]
(**
2 Computing joint state probability at time $t$ and $t+1$ given observations and priors

$\xi_{i,j}(t)=p(X_t=i,X_{t+1}=j|Y,\theta)
=\frac{\alpha_i(t)\times a_{i,j} \times\beta_j(t+1) \times b_j(y_{t+1})}{\sum_{k=1}^{N}\sum_{l=1}^{N}\alpha_k(t)\times a_{k,l} \times\beta_l(t+1) \times b_l(y_{t+1})}
=\frac{\alpha_i(t)\times a_{i,j} \times\beta_j(t+1) \times b_j(y_{t+1})}{\sum_{n=1}^{N}\alpha_n(t)\times\beta_n(t)}$
*)
let xi i j t =
    let numerator = alpha.[t,i] * A.[i,j] * beta.[t + 1, j] * By.[t + 1, j]
    let denominator = List.sum [for n in 0..(N-1) -> alpha.[t,n] * beta.[t,n]]
    numerator / denominator
(**
3 Infer posteriors

$\bar\pi_i=\gamma_i(1)$ initial state posterior
*)
let barpi = Array.init N (fun n -> gamma n 0)
(*** include-value:barpi ***)
(**
$\bar a_{i,j}=\frac{\sum_{t=1}^{T-1} \xi_{i,j}(t)}{\sum_{t=1}^{T-1}\gamma_i(t)}$ state transition posterior
*)
let barA = let arr =  Array.init N (fun i -> let denominator = List.sum [for t in 0..(T-2) -> gamma i t]
                                             Array.init N (fun j -> let numerator = List.sum [for t in 0..(T-2) -> xi i j t]
                                                                    numerator / denominator))
           Array2D.init N N (fun i j -> arr.[i].[j])
           
(*** include-value:barA ***)
(**
and finally emission probability:

$\bar b_n(y)=p(y|Y,X_t=n,\theta)$

in case of discrete $Y$ with $K$ possible states:

$\bar b_n(y)=\frac{\sum_{t=1}^{T}\gamma_n(t)\times I(Y_t=y)}{\sum_{t=1}^{T}\gamma_n(t)}$

in case of continous $Y\sim N(\mu_n,\Sigma_n)$ (gaussian) where $\mu_n$ is emission probability mean and $\Sigma_n$ is emission probability covariance for state $n$

posterior emission probability is:

$\hat b_n(y) = N(y|\hat\mu_n,\hat\Sigma_n)$

where:

$\bar \mu_n = \frac{\sum_{t=1}^T \gamma_n(t)\times y_t}{\sum_{t=1}^{T}\gamma_n(t)}$
*)
let barmu =
    Array.init N (fun n -> let numerator = List.sum [for t in 0..(T-1) -> gamma n t * Y.[t]]
                           let denominator = List.sum [for t in 0..(T-1) -> gamma n t]
                           numerator / denominator)
(*** include-value:barmu ***)

(**
$k_{n} = k0_{n} + \sum_{t=1}^{T}\gamma_n(t)$

$v_{n} = v0_{n} + \sum_{t=1}^{T}\gamma_n(t)$
*)

let S0 = Array.init N (fun _ -> 0.01)

let k0 = Array.init N (fun _ -> 1.)

let v0 = Array.init N (fun _ -> 1.)

let kn = Array.init N (fun n -> k0.[n] + List.sum [for t in 0..(T-1) -> gamma n t])

let vn = Array.init N (fun n -> v0.[n] + List.sum [for t in 0..(T-1) -> gamma n t])

(**
$m_n = \frac{m0_n \times k0_n + \bar \mu_n \times N}{k0_n+N}$

$S_n = S0_n + \sum_{t=1}^T (y_t*y_t*\gamma_n(t)) + k0_n m0_n^2 - k_n \bar \mu_n^2$
*)

let m0 = mu

let mn = Array.init N (fun n -> (m0.[n] * k0.[n] + mu.[n] * (float T)) / ((float T) + k0.[n]))

let Sn = Array.init N (fun n -> S0.[n] + (List.sum [for t in 0..(T-1) -> Y.[t] * Y.[t] * gamma n t]) + k0.[n] * m0.[n] * m0.[n] - kn.[n] * barmu.[n] * barmu.[n])

(**
$\bar \Sigma_n = \frac{\sum_{t=1}^{T} (y_t-\bar \mu_n)^2\times\gamma_n(t)}{\sum_{t=1}^{T} \gamma_n(t)}$
*)
let barSigma =
    Array.init N (fun n -> let numerator = List.sum [for t in 0..(T-1) -> (pown (Y.[t] - barmu.[n]) 2) * gamma n t]
                           let denominator = List.sum [for t in 0..(T-1) -> gamma n t]
                           numerator / denominator)
(*** include-value:barSigma ***)
(**
$\hat \Sigma_n = \frac{S_n}{v_n-2}$
*)
let hatmu = mn
(*** include-value:hatmu ***)
let hatSigma = Array.init N (fun n -> Sn.[n] / (vn.[n] - 2.))

(*** include-value:hatSigma ***)
let barB = Array.init N (fun n -> gaussianPdf barmu.[n] (sqrt barSigma.[n]))

(*** define-output:posteriorichart ***)
Chart.Combine(
  [for n in 0..(N-1) -> Chart.Line( Seq.map (fun x -> (x, barB.[n](x))) data, Name="state" + string(n + 1)) ])
  .WithLegend()
  .WithTitle("Posterior distributions")
(*** include-it:posteriorichart ***)

(**
#K-th step ahead y prediction

This needs revision. Predicting $y$ change at step $k$ is not very usefull at the moment. $y$ change across all steps $T+1:T+k$ will be more desirable.

$f_n(k)=p(x_T=n|y_{1:T})p(y_{T+k}|x_{T}=n) \\
=p(x_T=n|y_{1:T})\sum_{i=1}^N \left( p(y_{T+k},x_{T+k}=i|x_{T}=n) \right)\\
=p(x_T=n|y_{1:T})\sum_{i=1}^N \left( p(y_{T+k}|x_{T+k}=i)p(x_{T+k}=i|x_{T}=n) \right)\\
=p(x_T=n|y_{1:T})\sum_{i=1}^N \left( p(y_{T+k}|x_{T+k}=i)\sum_{j=1}^N  p(x_{T+k}=i,x_{T+k-1}=j|x_{T}=n) \right)\\
=p(x_T=n|y_{1:T})\sum_{i=1}^N \left( p(y_{T+k}|x_{T+k}=i)\sum_{j=1}^N  p(x_{T+k}=i|x_{T+k-1}=j)p(x_{T+k-1}=j|x_{T}=n) \right)$

$g_n(1)= \sum_{i=1}^N p(x_{T+1}=n|x_T=i) = \sum_{i=1}^Na_{i,n}\\
g_n(k)= \sum_{i=1}^N g_i(k-1)p(x_{T+k}=n|x_{T+k-1}=i) = \sum_{i=1}^N g_i(k-1)a_{i,n}\\
f_n(k,y_{T+k})=p(x_T=n|y_{1:T})g_n(k)p(y_{T+k}|x_{T+k}=n)\\
=\gamma_n(T)g_n(k)b_n(y_{T+k}) \\
\hat \mu_y = E_{\mu}[g_n(k)\times \bar\mu_n] = \frac{\sum_{n=1}^N g_n(k)\times \bar\mu_n}{\sum_{n=1}^N g_n(k)}\\
f(y_{T+k})=E_y[f_n(y_{T+k})]
=\frac{\sum_{n=1}^N p(x_T=n|y_{1:T})g_n(k)p(y_{T+k}|x_{T+k}=n)}{\sum_{n=1}^Np(x_T=n|y_{1:T})g_n(k)}
=\frac{\sum_{n=1}^N\gamma_n(T)g_n(k)b_n(y_{T+k})}{\sum_{n=1}^N\gamma_n(T)g_n(k)}$
*)

(**
#Iterative algorithm
In the following section I will introduce inference of parameters from number of sequences.
*)

type EmissionParameters =
    {
        m : float[];
        k : float[];
        S : float[];
        v : float[];
        Sigma : float[];
        SumObs: float[];
        SumDev: float[];
        SumGamma: float[];
        ObsCount: int;
    }

let inferHMM ((A: float[,]), (Bp: EmissionParameters), priorpi: float[]) (Y: float[]) =
    //let uniformPriorPi = Array.init N (fun _ -> 1./(float N))
    let T = Y.Length
    let B =
        Array.init N (fun n -> gaussianPdf Bp.m.[n] (Bp.Sigma.[n]))

    let By = 
        let arr = [for t in 0..(T-1) -> let bRow = [for n in 0..(N-1) -> B.[n](Y.[t])]
                                        let rowTotal = List.sum bRow
                                        List.map (fun v -> v / rowTotal) bRow |> Array.ofList]
        Array2D.init T N (fun t n -> arr.[t].[n])
    let alpha = 
        let result = Array2D.zeroCreate T N

        [for t in 0..(T-1) -> [for n in 0..(N-1) -> if t = 0 then result.[t,n] <- priorpi.[n] * By.[t,n]
                                                    else
                                                        result.[t, n] <- By.[t,n] * List.sum [for i in 0..(N-1) -> result.[t-1,i] * A.[i,n]]
                              ]] |> ignore
        result
    let beta = 
        let result = Array2D.zeroCreate T N
        [for t in (T-1)..(-1)..0 -> [for i in 0..(N-1) -> if t = T - 1 then result.[t, i] <- 1.
                                                          else
                                                            result.[t, i] <- List.sum [for j in 0..(N-1) -> result.[t + 1, j] * A.[i, j] * By.[t + 1, j]]
                                    ]] |> ignore
        result
    let gamma i t =
        alpha.[t,i] * beta.[t,i] / List.sum [for j in 0..(N-1) -> alpha.[t,j] * beta.[t,j]]
    let xi i j t =
        let numerator = alpha.[t,i] * A.[i,j] * beta.[t + 1, j] * By.[t + 1, j]
        let denominator = List.sum [for n in 0..(N-1) -> alpha.[t,n] * beta.[t,n]]
        numerator / denominator
    let barpi = Array.init N (fun n -> gamma n 0)
    let barA = let arr =  Array.init N (fun i -> let denominator = List.sum [for t in 0..(T-2) -> gamma i t]
                                                 Array.init N (fun j -> let numerator = List.sum [for t in 0..(T-2) -> xi i j t]
                                                                        numerator / denominator))
               Array2D.init N N (fun i j -> arr.[i].[j])
    let gamman = Array.init N (fun n -> List.sum [for t in 0..(T-1) -> gamma n t])
    let sumObs = Array.init N (fun n -> List.sum [for t in 0..(T-1) -> gamma n t * Y.[t]] + Bp.SumObs.[n])
    let sumGamma = Array.init N (fun n -> Bp.SumGamma.[n] + gamman.[n])
    let barmu =
        Array.init N (fun n -> let numerator = sumObs.[n]
                               let denominator = sumGamma.[n]
                               numerator / denominator)
    let sumDev = Array.init N (fun n -> List.sum [for t in 0..(T-1) -> (pown (Y.[t] - barmu.[n]) 2) * gamma n t] + Bp.SumDev.[n])
    let barSigma =
        Array.init N (fun n -> let numerator = sumDev.[n]
                               let denominator = sumGamma.[n]
                               sqrt( numerator / denominator))
    let m0 = Bp.m
    let k0 = Bp.k
    let S0 = Bp.S
    let v0 = Bp.v
    let kn = Array.init N (fun n -> k0.[n] + gamman.[n])
    let vn = Array.init N (fun n -> v0.[n] + gamman.[n])
    let mn = Array.init N (fun n -> (m0.[n] * k0.[n] + mu.[n] * gamman.[n]) / (gamman.[n] + k0.[n]))
    let S = Array.init N (fun n -> (List.sum [for t in 0..(T-1) -> Y.[t] * Y.[t] * gamma n t]))
    let Sn = Array.init N (fun n -> S0.[n] + S.[n] + k0.[n] * m0.[n] * m0.[n] - kn.[n] * barmu.[n] * barmu.[n])
//    let hatmu = mn
//    let hatSigma = Array.init N (fun n -> sqrt( Sn.[n] / (vn.[n] - 2.)))

    if Array.TrueForAll(barpi, fun v -> not(Double.IsNaN(v))) then
        (barA, { m = barmu; Sigma = barSigma; k = kn; v = vn; S = Sn; ObsCount = Bp.ObsCount + T; SumObs = sumObs; SumDev = sumDev; SumGamma = sumGamma}, barpi)
    else
        (A, Bp, priorpi)

let Y2 = (observe stockSymbol)

let slidingWindowY = 
    Seq.windowed windowSize Y2
    |> List.ofSeq

let rec divide n (ys: 'a list) =
    if ys.Length < n then []
    else
        let result = Seq.take n ys |> Seq.toArray
        result::divide n (Seq.skip n (Seq.ofList ys) |> Seq.toList)

let Yseqences = divide windowSize (List.ofArray Y2)

let rec inferLoop priors ys =
    match ys with
    | [] -> priors
    | y::ys -> inferLoop (inferHMM priors y) ys

let T2 = Y2.Length

let mean = 
    (Seq.sum Y2) / (float T2 - 1.)

let stdDev2 = (Seq.map (fun y -> Math.Pow(y - mean, 2.)) Y2 |> Seq.sum) / (float T2 - 1.) |> sqrt
(*** include-value:stdDev2 ***)

let mu2 =
    let max = observationMean + (stdDev2 * 2.)
    let range = stdDev2 * 4.
    let interval = range / (float N)
    Array.init N (fun n -> max - interval * (float n))
(*** include-value:mu2 ***)

let theta2 = (A, Array.init N (fun n -> (mu2.[n], stdDev2)), pi)
(*** include-value:theta2 ***)

(*** define-output:priorichart2 ***)
let data2 = seq { for x in ( (- stdDev2 * 3.))..((stdDev2 * 6.) / 400.)..(stdDev2 * 3.) -> x }
let (_, Bp2, _) = theta2
let B2 =
    Array.init N (fun n -> gaussianPdf (fst Bp2.[n]) (snd Bp2.[n]))

Chart.Combine(
  [for n in 0..(N-1) -> Chart.Line( Seq.map (fun x -> (x, B2.[n](x))) data2, Name="state" + string(n + 1)) ])
  .WithLegend()
  .WithTitle("Priori distributions")
(*** include-it:priorichart2 ***)

let emptyObs = Array.init N (fun _ -> 0.)

let priorTheta = { m = m0; Sigma = Array.init N (fun _ -> stdDev); k = k0; v = v0; S = S0; SumObs = emptyObs; SumDev = emptyObs; SumGamma = emptyObs; ObsCount = 0}

let barTheta = 
    inferLoop (A, priorTheta, pi) Yseqences
(*** include-value:barTheta ***)

(*** define-output:posteriorichart2 ***)
let (_, Bp, _) = barTheta
let B3 =
    Array.init N (fun n -> gaussianPdf Bp.m.[n] Bp.Sigma.[n])

Chart.Combine(
  [for n in 0..(N-1) -> Chart.Line( Seq.map (fun x -> (x, B3.[n](x))) data2, Name="state" + string(n + 1)) ])
  .WithLegend()
  .WithTitle("Posterior unweighted distributions of " + stockSymbol)
(*** include-it:posteriorichart2 ***)

(**
Predictive Gaussian mix
--

One step ahead

$p(y_{T+1}|y_{1:T})=\sum_{n=1}^N \left( p(y_{T+1},x_T=n|y_{1:T}) \right)\\
=\sum_{n=1}^N \left( p(y_{T+1}|x_T=n)p(x_T=n|y_{1:T}) \right)\\
=\sum_{n=1}^N \left( p(x_T=n|y_{1:T}) \left( \sum_{k=1}^N p(y_{T+1},x_{T+1}=k|x_T=n) \right) \right)\\
=\sum_{n=1}^N \left( p(x_T=n|y_{1:T}) \left( \sum_{k=1}^N p(y_{T+1}|x_{T+1}=k)p(x_{T+1}=k|x_{T}=n) \right)\right)\\
=\sum_{n=1}^N \left( \gamma_n(T) \left( \sum_{k=1}^N b_k(y_{T+1}) a_{n,k} \right) \right)$
*)

let stepAheadDistribution (A: float[,], Bp : EmissionParameters, priorpi: float[]) (Y: float[]) =
    let uniformPriorPi = priorpi// Array.init N (fun _ -> 1./(float N))
    let T = Y.Length
    let B =
        Array.init N (fun n -> gaussianPdf Bp.m.[n] (Bp.Sigma.[n]))

    let By = 
        let arr = [for t in 0..(T-1) -> let bRow = [for n in 0..(N-1) -> B.[n](Y.[t])]
                                        let rowTotal = List.sum bRow
                                        List.map (fun v -> v / rowTotal) bRow |> Array.ofList]
        Array2D.init T N (fun t n -> arr.[t].[n])
    let alpha = 
        let result = Array2D.zeroCreate T N

        [for t in 0..(T-1) -> [for n in 0..(N-1) -> if t = 0 then result.[t,n] <- uniformPriorPi.[n] * By.[t,n]
                                                    else
                                                        result.[t, n] <- By.[t,n] * List.sum [for i in 0..(N-1) -> result.[t-1,i] * A.[i,n]]
                              ]] |> ignore
        result
    let beta = 
        let result = Array2D.zeroCreate T N
        [for t in (T-1)..(-1)..0 -> [for i in 0..(N-1) -> if t = T - 1 then result.[t, i] <- 1.
                                                          else
                                                            result.[t, i] <- List.sum [for j in 0..(N-1) -> result.[t + 1, j] * A.[i, j] * By.[t + 1, j]]
                                    ]] |> ignore
        result
    let gamma i t =
        alpha.[t,i] * beta.[t,i] / List.sum [for j in 0..(N-1) -> alpha.[t,j] * beta.[t,j]]
    let gamman = Array.init N (fun n -> List.sum [for t in 0..(T-1) -> gamma n t])
    let gammaSum = Array.sum gamman
    let gammaNormalised = Array.init N (fun n -> gamman.[n] / gammaSum)
    (fun y -> List.sum [for n in 0..(N-1) -> gammaNormalised.[n] * (List.sum [for k in 0..(N-1) -> A.[n,k] * B.[k](y)])])

let predictionDistribution = 
    stepAheadDistribution barTheta (Seq.last Yseqences)

(*** define-output:predictiveDistribution ***)
Chart.Line(Seq.map (fun x -> (x, predictionDistribution(x))) data2)
  .WithTitle("One step ahead predictive distributions of " + stockSymbol)
(*** include-it:predictiveDistribution ***)

(*** define-output:predictiveDistributionAll ***)
Chart.Combine(Seq.map (fun slice -> let predictionDistribution = stepAheadDistribution barTheta slice
                                    Chart.Line(Seq.map (fun x -> (x, predictionDistribution(x))) data2)) Yseqences)
  .WithTitle("One step ahead predictive distributions of " + stockSymbol + " for all sequences")
(*** include-it:predictiveDistributionAll ***)

(**
Geometric return
--

$r=\begin{pmatrix}
r_1\\
r_2\\
\vdots\\
r_N
\end{pmatrix},
r_n \in \mathbb{R}\\
\zeta = abs( min_t \space r_t)\\
0<f<1\\
G(f)=\left[ \prod_{n=1}^N \left(1 + \frac{r_n \times f}{\zeta} \right)^{p(r_n)} \right]^{\frac{1}{\sum_{n=1}^N p(r_n)}}\\
\hat r = argmax_f \space G(f)$
*)

let findGAndF volume range probDist =
    let r = Seq.map (fun x -> volume * (exp x)) range
    let pr = Seq.map (fun x -> probDist(x)) range
    let zeta = Seq.min r |> abs
    let fRange = seq {for f in (0.1)..(0.1)..(0.9) -> f}
    let G f =
        let product = Seq.reduce (*) (Seq.zip r pr |> Seq.map (fun (r, pr) -> Math.Pow(1. + (r * f / zeta), 1./pr)))
        Math.Pow(product, (1./ (Seq.sum pr)))
    let gs = Seq.map (fun f -> (G f, f)) fRange |> Seq.sortBy (fun (g, _) -> -g)
    gs |> List.ofSeq

let backTest window steps =
    let capital = 100.
    let volInvested = Seq.map (fun y -> let probDist = stepAheadDistribution barTheta y
                                        let shortGf = (findGAndF -capital data2 probDist).Head
                                        let longGf = (findGAndF capital data2 probDist).Head
                                        match (fst shortGf) with
                                        | g when g > 1.00001 -> capital * (snd shortGf)
                                        | g when g < 0.99999 -> -capital * (snd shortGf)
                                        | _ -> 0.) (Seq.windowed window Y2)
    Seq.zip volInvested (Seq.skip window Y2) 
        |> Seq.map (fun (v, r) -> v * (exp r) - v)
        |> Seq.take steps
    