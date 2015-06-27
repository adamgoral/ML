(*** hide ***)
#load @"ML.Documentation.fsx"
open System
open System.IO
open System.Collections.Generic
open SmartTrader.Data
open SmartTrader.Domain
open System.Threading
open System.Threading.Tasks
open System.Linq

let pricesDir = @"..\Data\Market\"

open MathNet.Numerics.LinearAlgebra

open CsvHelper

open FSharp.Charting

(**
State Space Models
==
*)

(**
Kalman filter general definition
--

State transition model

$z_t=A_t z_{t-1} + B_t u_t + \epsilon_t$

Observation model

$y_t=C_t z_t + D_t u_t + \delta_t$

where system noise is $\epsilon_t \sim N(0,Q_t)$

observation noise is $\delta_t \sim N(0,R_t)$

$u_t$ is optional input or control signal 

It is assumed that $p(\epsilon_t , \delta_t)=p(\epsilon_t)p(\delta_t)$ (are independent).

Kalman filter inference
--

Prediction step
-

$p(z_t|y_{1:t-1}, u_{1:t}) = N(z_t| \mu_{t|t-1}, \Sigma_{t|t-1})$

where $\mu_{t:t-1}$ is conditional state mean and $\Sigma_{t|t-1}$ is conditional state covariance

$\mu_{t|t-1} = A_t \mu_{t-1} + B_t u_t$
*)

let mucond (A: Matrix<_>) (mu: Vector<_>) (B: Matrix<_>) (u: Vector<_>) =
    A * mu + B * u

(**
$\Sigma_{t|t-1} = A_t \Sigma_{t-1} A_t^T + Q_t$
*)

let sigmacond (A: Matrix<_>) (sigma: Matrix<_>) (Q: Matrix<_>) =
    A * sigma * (A.Transpose()) + Q

(**
predicted observation is defined as:

$\hat y_t = C_t \mu_{t|t-1} +D_t u_t$
*)

let ypred (C: Matrix<_>) (mucond: Vector<_>) (D: Matrix<_>) (u: Vector<_>) =
    C * mucond + D * u

(**
Measurement step
-

$r_t = y_t- \hat y_t$ is residual and $y_t$ is observation

$S_t= cov[r_t|y_{1:t-1},u_{1:t}] = E[(C_t z_t + \delta_t - \hat y_t)(C_t z_t + \delta_t - \hat y_t)^T | y_{1:t-1},u_{1:t}] = C_t \Sigma_{t|t-1} C_t^T + R_t$

therefore observation noise covariance can be computed by

$R_t = cov[r_t|y_{1:t-1},u_{1:t}] - C_t \Sigma_{t|t-1} C_t^T$

*)

let R (r: Vector<_> list) (C: Matrix<_>) (sigmacond: Matrix<_>) =
    let rsum = List.fold (fun state v -> state + v) (DenseVector.zero (sigmacond.RowCount)) r
    let ravg = rsum / (float r.Length)
    let covsum = Seq.pairwise r 
                |> Seq.map (fun (x, y) -> (x - ravg) * (y - ravg))
                |> Seq.fold (fun state v -> state + v) (DenseVector.zero (sigmacond.RowCount))
    let cov = (covsum / (float r.Length)).ToColumnMatrix()
    cov - C * sigmacond * (C.Transpose())
(**
Kalman gain matrix is given by

$K_t= \Sigma_{t|t-1} C_t^T S_t^{-1} = \Sigma_{t|t-1} C_t^T (C_t \Sigma_{t|t-1} C_t^T + R_t)^{-1} = (\Sigma_{t|t-1}^{-1} + C_t^T RC)^{-1} C^T R^{-1}$
*)

let kalmangain (sigmacond: Matrix<_>) (C: Matrix<_>) (R: Matrix<_>) =
    let Ctrans = C.Transpose()
    sigmacond * Ctrans * ((C * sigmacond * Ctrans + R).Inverse())

(**
$\mu_t = \mu_{t|t-1} + K_t r_t$
*)

let mu (mucond: Vector<_>) (K: Matrix<_>) (r: Vector<_>) =
    mucond + K * r
(**
$\Sigma_t = (I - K_t C_t)\Sigma_{t|t-1}$
*)

let sigma (K: Matrix<_>) (C: Matrix<_>) (sigmacond: Matrix<_>) =
    let I = DenseMatrix.identity sigmacond.RowCount
    (I - K * C) * sigmacond

(**
Recursive evaluation
*)

let rec evaluateKalman mu_p sigma_p A B C D u Q R (observations: Vector<_> list) =
    let mucond_t = mucond A mu_p B u
    let sigmacond_t = sigmacond A sigma_p Q
    let pred = ypred C mucond_t D u
    match observations with
    | [] -> []
    | o::tail -> 
        let r = o - pred
        let K_t = kalmangain sigmacond_t C R
        let mu_t = mu mucond_t K_t r
        let sigma_t = sigma K_t C sigmacond_t
        (pred, o)::(evaluateKalman mu_t sigma_t A B C D u Q R tail)

(**
Timeseries input
--
*)

type TSItem() =
    member val Date = DateTime.MinValue with get, set
    member val Open = 0. with get, set
    member val High = 0. with get, set
    member val Low = 0. with get, set
    member val Close = 0. with get, set

let getPrices instrument =
    let reader = new CsvReader(new StreamReader(pricesDir + instrument + ".csv"))
    reader.GetRecords<TSItem>()

let getReturns instrument =
    let prices = getPrices instrument
    let closeOf (price: TSItem) = price.Close
    Seq.map closeOf prices
        |> Seq.map log
        |> Seq.pairwise
        |> Seq.map (fun (p, n) -> n - p)

(**
Local level model
-

Scalar linear transformations for state, observation and control

$A_t=B_t=C_t=D_t=\begin{pmatrix}
1
\end{pmatrix}$

Minimal state noise $Q_t=\begin{pmatrix} 0.001 \end{pmatrix}$

No control signal $u_t=\begin{pmatrix}0\end{pmatrix}$

Initial mean and variance

$\mu_0 = \begin{pmatrix} 0 \end{pmatrix}$

$\sigma_0 = \begin{pmatrix} 1 \end{pmatrix}$

*)

let localLevelModel observations q r =
    let A = DenseMatrix.create 1 1 1.
    let B = DenseMatrix.create 1 1 1.
    let C = DenseMatrix.create 1 1 1.
    let D = DenseMatrix.create 1 1 1.
    let Q = DenseMatrix.create 1 1 q
    let R = DenseMatrix.create 1 1 r
    let u = DenseVector.zero 1
    let sigma_0 = DenseMatrix.create 1 1 10.
    let mu_0 = DenseVector.create 1 1.
    let results = evaluateKalman mu_0 sigma_0 A B C D u Q R observations
    results

//(*** define-output:localLevelChart ***)

let goog = List.ofSeq (getPrices "GOOG" |> Seq.map (fun r -> DenseVector.create 1 r.Close))
Chart.Combine([Chart.Line(List.mapi (fun i (actual: Vector<_>) -> (i, actual.[0])) goog, Name = "actual");
               Chart.Line(List.mapi (fun i x -> (i, x)) (localLevelModel goog 0. 0.1 |> List.map (fun (_, y:Vector<_>)-> y.[0])), Name = "q=0 r=0.1")
               Chart.Line(List.mapi (fun i x -> (i, x)) (localLevelModel goog 0.1 0. |> List.map (fun (_, y:Vector<_>)-> y.[0])), Name = "q=0.1 r=0.")
               Chart.Line(List.mapi (fun i x -> (i, x)) (localLevelModel goog 0.1 0.1 |> List.map (fun (_, y:Vector<_>)-> y.[0])), Name = "q=0.1 r=0.1")
               ]).WithTitle("One step ahead predictive distributions of " + "GOOG").WithLegend()
//(*** include-it:localLevelChart ***)
