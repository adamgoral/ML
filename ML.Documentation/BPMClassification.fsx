(*** hide ***)
//#I ".."
#load "ML.Documentation.fsx"
//#r "ML.dll"

(**
Bernoulli product model (BPM)
================

First, I will cover the basics of the model and define like for like implementation in F#.
Later, I will cover more flexible implementation that allows online learning and filtering out features with low mutual information.

I have chosen to use BigRational in most of the implementation, as I wish to maintain high precision of calculations.
(and reduce clutter with $float \rightarrow int$ and $int \rightarrow float$ conversions all over the place)
Performance considerations are out of scope of this document.

So, given:

$N \in \mathbb{N}_0$ is count of observations

observations $D = (X, Y)$
*)
open MathNet.Numerics
open System.Numerics

let observe () = Seq.empty<_ list * _>

let D = observe() |> List.ofSeq

let N = BigRational.FromInt D.Length

(**
$X =
 \begin{pmatrix}
  x_{1,1} & x_{1,2} & \cdots & x_{1,K} \\
  x_{2,1} & x_{2,2} & \cdots & x_{2,K} \\
  \vdots  & \vdots  & \ddots & \vdots  \\
  x_{N,1} & x_{N,2} & \cdots & x_{N,K}
 \end{pmatrix},
Y =
 \begin{pmatrix}
  y_{1} \\
  y_{2} \\
  \vdots \\
  y_{N} \\
 \end{pmatrix}
$

feature $x_{n,k} \in \left \{ 0,1 \right\}$
label $y_{n} \in \left \{1,...,C \right\}$

*)

let X = List.map fst D
let Y = List.map snd D

(**
$K \in \mathbb{N}_0$ is count of features

$C \in \mathbb{N}_0$ is count of labels
*)

let Cs = Seq.distinct Y |> List.ofSeq

let C = BigRational.FromInt Cs.Length

(**

Assuming that features are independent from each other (naive bayes), the model can be defined as:

$
p(x,y|D) = p(y|D) \times p(x|y,D) \\
= \prod_{c=1}^{C} \left( p(y=c|D) \times \prod_{k=1}^{K} p(x_k|y=c,D) \right) \\
= \prod_{c=1}^{C} \left( \int_{\pi_c} p(y=c|\pi_c) \times p(\pi_c|D) d\pi_c \times \prod_{k=1}^{K} \int_{\theta_{c,k}} p(x_k|y=c,\theta_{c,k}) \times p(\theta_{c,k}|D) d\theta_{c,k} \right) \\
= \prod_{c=1}^{C} \left( \int_{\pi_c} Cat(y=c|\pi_c) \times Dir(\pi_c|D) d\pi_c \times \prod_{k=1}^{K} \int_{\theta_{c,k}} Ber(x_k|y=c,\theta_{c,k}) \times Beta(\theta_{c,k}|D) d\theta_{c,k} \right) \\
= \prod_{c=1}^{C} \left( \int_{\pi_c} Cat(y=c|\pi_c) \times \frac{Mul(D|\pi_c) \times Dir(\pi_c|\alpha_c)}{p(D)} d\pi_c \times \prod_{k=1}^{K} \int_{\theta_{c,k}} Ber(x_k|y=c,\theta_{c,k}) \times \frac{Bin(D|\theta_{c,k}) \times Beta(\theta_{c,k}|a_{c,k},b_{c,k})}{p(D)} d\theta_{c,k} \right) \\
= \prod_{c=1}^{C} \left( \int_{\pi_c} Cat(y=c|\pi_c) \times Dir(\pi_c|N_c+\alpha_c) d\pi_c \times \prod_{k=1}^{K} \int_{\theta_{c,k}} Ber(x_k|y=c,\theta_{c,k}) \times Beta(\theta_{c,k}|N_{c,k}+a_{c,k},N_c-N_{c,k}+b_{c,k}) d\theta_{c,k} \right) \\
= \prod_{c=1}^{C} \left( Cat(y=c|\bar\pi_c) \times \prod_{k=1}^{K} Ber(x_k|y=c,\bar\theta_{c,k}) \right) \\
= \prod_{c=1}^{C} \left( \bar\pi_c \times \prod_{k=1}^{K} \left( \bar\theta_{c,k}^{I(x_k=1)} \times(1-\bar\theta_{c,k})^{I(x_k\neq 1)} \right) \right)^{I(y=c)}\\
$

where

$N_c = 
\begin{pmatrix}
N_1\\
N_2\\
\vdots\\
N_C\\
\end{pmatrix} =
\sum_{n=1}^N I(y_n = c)$ is column vector with count of each observed label
*)

let Nc = Seq.countBy id Y
         |> Seq.map (fun (k, v) -> (k, BigRational.FromInt v))
         |> Map.ofSeq
(**
$N_{c,k} = 
\begin{pmatrix}
N_{1,1} & N_{1,2} & \cdots & N_{1,K}\\
N_{2,1} & N_{2,2} & \cdots & N_{2,K}\\
\vdots & \vdots & \ddots & \vdots\\
N_{C,1} & N_{C,2} & \cdots & N_{C,K}\\
\end{pmatrix}
= \sum_{n=1}^{N} I(x_{n,k} = 1) \times I (y_{n} = c)$ is matrix with counts of presence of each feature $k$ in $c$ label
*)

let rec updateCount (stats: Map<_,_>) data =
    match data with
    | [] -> stats
    | d::ds -> match stats.TryFind d with
                | None -> updateCount (stats.Add(d, 1N)) ds
                | Some n -> updateCount (stats.Add(d, n + 1N)) ds

let Nck = Seq.groupBy snd D
          |> Seq.map (fun (key, xs) -> let ds = Seq.map fst xs
                                       let stats = Seq.fold (fun state ds -> updateCount state ds) Map.empty ds
                                       (key, stats))
          |> Map.ofSeq

let Ks = Nck |> Seq.collect (fun kvp -> kvp.Value |> Seq.map (fun kvp -> kvp.Key))
             |> Seq.distinct
             |> List.ofSeq

let K = Ks.Length

(**
$\alpha_c=
\begin{pmatrix}
\alpha_1\\
\alpha_2\\
\vdots\\
\alpha_C\\
\end{pmatrix}
$ is column vector of priors (set to uniform $1$ in this example)
*)

let Alphac = Cs |> List.map (fun c -> (c, 1N))
                |> Map.ofSeq
(**
$\alpha_0 = \sum_{c=1}^{C} \alpha_c$ is sum of priors
*)
let Alpha0 = Alphac |> Map.toSeq
                    |> Seq.sumBy snd
(**
$\bar\pi_c=
\begin{pmatrix}
\bar\pi_1\\
\bar\pi_2\\
\vdots\\
\bar\pi_C\\
\end{pmatrix}
=E \left[ Dir(\pi_c|N_c+\alpha_c) \right] = \frac{N_c+\alpha_c}{N+\alpha_0}$ is expected posterior of $\pi_c$
*)
let inferPic c = (Nc.[c] + Alphac.[c]) / (N + Alpha0)

let Pic = Cs |> List.map (fun c -> (c, inferPic c))
             |> Map.ofSeq
(**
$\bar\theta_{c,k} =
\begin{pmatrix}
\bar\theta_{1,1} & \bar\theta_{1,2} & \cdots & \bar\theta_{1,K}\\
\bar\theta_{2,1} & \bar\theta_{2,2} & \cdots & \bar\theta_{2,K}\\
\vdots & \vdots & \ddots & \vdots\\
\bar\theta_{C,1} & \bar\theta_{C,2} & \cdots & \bar\theta_{C,K}\\
\end{pmatrix}
= E \left[ Beta(\theta_{c,k}|N_{c,k}+a_{c,k}, N_c-N_{c,k}+b_{c,k}) \right]= \frac{N_{c,k}+a_{c,k}}{N_c+a_{c,k}+b_{c,k}}$ is expected posterior of $\theta_{c,k}$
*)
let ack = Ks |> Seq.map (fun k -> (k, 1N)) |> Map.ofSeq
let bck = Ks |> Seq.map (fun k -> (k, 1N)) |> Map.ofSeq

module Map =
    let findOrDefault (map: Map<_,_>) key defaultValue =
        match map.TryFind key with
        | None -> defaultValue
        | Some value -> value

let inferThetack c k = ((Map.findOrDefault Nck.[c] k 0N) + ack.[k]) / (Nc.[c] + ack.[k] + bck.[k])
    
let Thetack = Cs |> List.map (fun c -> (c, Ks |> List.map (fun k -> (k, inferThetack c k))
                                              |> Map.ofSeq))
                 |> Map.ofSeq
(**
$a_{c,k} =
\begin{pmatrix}
a_{1,1} & a_{1,2} & \cdots & a_{1,K} \\
a_{2,1} & a_{2,2} & \cdots & a_{2,K} \\
\vdots & \vdots & \ddots & \vdots \\
a_{C,1} & a_{C,2} & \cdots & a_{C,K} \\
\end{pmatrix}
, b_{c,k} =
\begin{pmatrix}
b_{1,1} & b_{1,2} & \cdots & b_{1,K} \\
b_{2,1} & b_{2,2} & \cdots & b_{2,K} \\
\vdots & \vdots & \ddots & \vdots \\
b_{C,1} & b_{C,2} & \cdots & b_{C,K} \\
\end{pmatrix}$ are matrices of priors for features $k$ of label $c$ (set to uniform 1 in this example)
*)
let ac = Cs |> Seq.map (fun c -> (c, 1N)) |> Map.ofSeq
let bc = Cs |> Seq.map (fun c -> (c, 1N)) |> Map.ofSeq
(**
$I(expr)= \begin{cases} true & 1 \\ 0\\ \end{cases}$
*)
let I expr = if expr then 1
             else 0
(**
$
p(x|D) = \prod_{k=1}^{K} \int_{\theta_k} p(x_k|\theta_k) \times p(\theta_k|D) d\theta_k\\
= \prod_{k=1}^{K} \int_{\theta_k} Ber(x_k|\theta_k) \times Beta(\theta_k|D) d\theta_k\\
= \prod_{k=1}^{K} \int_{\theta_k} Ber(x_k|\theta_k) \times \frac{Ber(D|\theta_k) \times Beta(\theta_k|a_k, b_k)}{p(D)} d\theta_k\\
= \prod_{k=1}^{K} \int_{\theta_k} Ber(x_k|\theta_k) \times Beta(\theta_k|N_k+a_k, N-N_k+b_k) d\theta_k\\
= \prod_{k=1}^{K} Ber(x_k|\bar\theta_k) \\
= \prod_{k=1}^{K} \left( \bar\theta_k^{I(x_k=1)} \times (1-\bar\theta_k)^{I(x_k \neq 1)} \right)
$

where:

$N_k = 
\begin{pmatrix}
N_1\\
N_2\\
\vdots\\
N_K\\
\end{pmatrix}^T
 = \sum_{n=1}^N I(x_{n,k} = 1)$ is vector with count of presence of each feature
*)
let rec merge (stats: Map<_,_>) data =
    match data with
    | [] -> stats
    | (k, n)::rest -> match stats.TryFind k with
                      | None -> merge (stats.Add(k, n)) rest
                      | Some v -> merge (stats.Add(k, n + v)) rest

let Nk = Nck |> Seq.collect (fun kvp -> Map.toSeq kvp.Value)
             |> List.ofSeq
             |> merge (Map.empty)

(**
$\bar\theta_k =
\begin{pmatrix}
\bar\theta_1\\
\bar\theta_2\\
\vdots\\
\bar\theta_K\\
\end{pmatrix}^T
 = E \left[ Beta(\theta_k|N_k+a_k,N-N_k+b_k) \right] = \frac{N_k+a_k}{N+a_k+b_k}$ is expected posterior of $\theta_k$

$a_k =
\begin{pmatrix}
a_1\\
a_2\\
\vdots\\
a_K\\
\end{pmatrix}^T
, b_k =
\begin{pmatrix}
b_1\\
b_2\\
\vdots\\
b_K\\
\end{pmatrix}^T$ are vectors of priors for features $k$ (set to uniform 1)
*)
let ak = Ks |> Seq.map (fun k -> (k, 1N)) |> Map.ofSeq
let bk = Ks |> Seq.map (fun k -> (k, 1N)) |> Map.ofSeq

let inferThetak k = ((Map.findOrDefault Nk k 0N) + ak.[k]) / (N + ak.[k] + bk.[k]) 

let Thetak = Ks |> Seq.map (fun k -> (k, inferThetak k))
                |> Map.ofSeq

(**
##Mutual information

Documents can potentialy contain large variety of features (words), so it makes sense to identify mutual information of each feature.

This will be very handy when discarding features of insignificant information value.

$I[x_k;y]=KL[p(x_k,y)||p(x_k)p(y)]\\
=\sum_{x_k}\sum_yp(x_k,y|D)\times log\frac{p(x_k,y|D)}{p(x_k|D)\times p(y|D)}\\
=\sum_{c=1}^C \left( p(x_k=1,y=c|D)\times log\frac{p(x_k=1,y=c|D)}{p(x_k=1|D)\times p(y=c|D)}+ p(x_k=0,y=c|D)\times log\frac{p(x_k=0,y=c|D)}{p(x_k=0|D)\times p(y=c|D)} \right)\\
=\sum_{c=1}^C \left( \bar\pi_c \times \bar\theta_{c,k} \times log\frac{  \bar\pi_c \times \bar\theta_{c,k}}{\bar\theta_k \times \bar\pi_c} + \bar\pi_c \times  (1-\bar\theta_{c,k}) \times log\frac{ \bar\pi_c \times (1-\bar\theta_{c,k})}{(1-\bar\theta_k) \times \bar\pi_c} \right)\\
=\sum_{c=1}^C \left( \bar\pi_c \times \left( \bar\theta_{c,k}\times log \frac{\bar\theta_{c,k}}{\bar\theta_k} + (1-\bar\theta_{c,k}) \times log \frac{1-\bar\theta_{c,k}}{1-\bar\theta_k}) \right) \right)\\
$
$
MI_k=
\begin{pmatrix}
MI_1\\
MI_2\\
\vdots\\
MI_K\\
\end{pmatrix}
=\sum_{c=1}^C \left( \bar\pi_c \times \left( \bar\theta_{c,k}\times log \frac{\bar\theta_{c,k}}{\bar\theta_k} + (1-\bar\theta_{c,k}) \times log \frac{1-\bar\theta_{c,k}}{1-\bar\theta_k}) \right) \right)\\
$ is mutual information for each $k$ feature
*)

let calculateMI c k =
    let thetaCk = BigRational.ToDouble Thetack.[c].[k]
    let thetak = BigRational.ToDouble Thetak.[k]
    let pic = BigRational.ToDouble Pic.[c]
    pic * (thetaCk * (log(thetaCk/thetak)) + (1. - thetaCk) * (log((1.-thetaCk)/(1.-thetak))))

let MIk = Ks |> Seq.map (fun k ->
                            (k, Cs |> Seq.map (fun c -> calculateMI c k)
                                   |> Seq.sum)
                        )
             |> Map.ofSeq
(**
##Predictive model

$
p(y|x,D)= \frac{p(x,y|D)}{p(x|D)}
= \frac{\prod_{c=1}^{C} \left( \bar\pi_c \times \prod_{k=1}^{K} \left( \bar\theta_{c,k}^{I(x_k=1)} \times(1-\bar\theta_{c,k})^{I(x_k\neq 1)} \right) \right)^{I(y=c)}}{\prod_{k=1}^{K} \left( \bar\theta_k^{I(x_k=1)} \times (1-\theta_k)^{I(x_k \neq 1)}\right)}\\
\propto \prod_{c=1}^{C} \left( \bar\pi_c \times \prod_{k=1}^{K} \left( \bar\theta_{c,k}^{I(x_k=1)} \times(1-\bar\theta_{c,k})^{I(x_k\neq 1)} \right) \right)^{I(y=c)} \\
$

$
\DeclareMathOperator*{\argmax}{argmax}
\hat y = \argmax_c p(y=c|x,D)
= \argmax_c \left( \bar\pi_c \times \prod_{k=1}^{K} \left( \bar\theta_{c,k}^{I(x_k=1)} \times(1-\bar\theta_{c,k})^{I(x_k\neq 1)} \right)\right)\\
$
*)

let predictY x =
    let xset = Set x
    let calcBeta c k = if (xset.Contains(k)) then Thetack.[c].[k]
                       else (1N - Thetack.[c].[k])
    let ys = Cs |> Seq.map (fun c -> (c, Pic.[c] * (Ks |> Seq.map (fun k -> calcBeta c k)
                                                       |> Seq.reduce (*))))
                |> Seq.sortBy snd
    Seq.last ys |> fst

(**
##F# implementation considerations

1 New labels and features can be observed during new learning iterations. New features can be supplied during testing.
Default (uniform) prior is used for unobserved features and labels.

2 In online learning setting, posterior becomes prior for next iteration of learning.

Instead of calulating $\bar\theta, \bar\pi$ vectors and matrices and then using them in prediction, I will directly use functions $\theta_c(c), \theta_k(k), \theta_{ck}(c, k), \pi_c(c)$ to provide same posteriors.
They will simplify handling missing values in observation matrices $N_{c,k}, N_{c}, N_{k}$ and using priors for unobserved (and can have memoisation as part of it).

$N_{c,k}, N_{c}, N_{k}, N$ will continue to be used and updated with each iteration of learning and serve as prior parameters.

Example:

$N_c =
\begin{pmatrix}
N_1\\
N_2\\
\vdots\\
N_C\\
\end{pmatrix}$ is accumulation of current and previous learning batches

$C$ is set of observed labels

$\alpha = 1$ is the uniform prior for every element in $ C $ (regardless if it was observed or not)

$\alpha_c(c) = \begin{cases} c \in C & N_c + \alpha\\ c \not\in C & \alpha \end{cases}$

$\alpha_0 = \sum_{c \in C} \alpha_c(c)$

$\pi_c(c) = E \left[ Dir(\pi_{c}|\alpha_c(c)) \right] = \frac{\alpha_c(c)}{\alpha_0}$

Therefore a model state is needed that can be incrementaly updated with observations and exposes needed inference functions.

For now I will use basic softmax approach. (There will be always one label predicted that has highest probability amongst all label predictions).
*)

type Classification<'a, 'b> when 'a: comparison and 'b: comparison =
    abstract member Observe : (Set<'a> * 'b) list -> Classification<'a, 'b>
    abstract member InferLabel : (Set<'a>) -> 'b

(**

3 Input features can be very sparse in document classification scenarios. It will be more efficient to pass to the model only features that have been observed:

So, instead of representing input as very large and sparse vector or matrix:
$x=
\begin{pmatrix} 0 & 0 & 1 & 0 & 0 & \cdots & 0
\end{pmatrix}$

it makes more sense to represent it as a set of elements that have been observed:
$x=\lbrace feature1,feature2, \cdots, featurek \rbrace$


4 Depending on a quality of input it might be necessary to handle some elements as 'unknown'.
This transformation will be done at input preprocessing stage together with other operations (such as word stemming in case of document classification).
This will be out of scope of the model functionality.

Putting all (well, almost all) of the above into one type gives:
*)

open System.Collections.Concurrent

let memoize<'a,'b> f =
    let cache = new ConcurrentDictionary<'a,'b>()
    (fun k -> cache.GetOrAdd(k, fun k -> f k))

type ClassificationBPM<'a, 'b> when 'a: comparison and 'b: comparison (n, nc, nk, nck) =
    let N = n
    let Nc = nc
    let Nk = nk
    let Nck = nck
    let Cs = Map.toSeq nc |> Seq.map fst |> List.ofSeq
    let Ks = Map.toSeq nk |> Seq.map fst |> List.ofSeq
    let Thetack (c, k) = ((Map.findOrDefault (Nck.[c]) k 0N) + 1N) / (Nc.[c] + 2N)
    let mThetack = memoize Thetack
    let Alpha0 = BigRational.FromInt Cs.Length
    let Pic c = ((Map.findOrDefault Nc c 0N) + 1N) / (N + Alpha0)
    let mPic = memoize Pic
    let predictY (x: Set<'a>) = 
        let calcBeta c k = if (x.Contains(k)) then mThetack (c, k)
                           else (1N - (mThetack (c, k)))
        let ys = Cs |> Seq.map (fun c -> (c, (mPic c) * (Ks |> Seq.map (fun k -> calcBeta c k)
                                                           |> Seq.reduce (*))))
                    |> Seq.sortBy snd
        Seq.last ys |> fst
    new() = ClassificationBPM(0N, Map.empty<_,_>, Map.empty<_,_>, Map.empty<_,_>)
    interface Classification<'a, 'b> with
        member this.Observe(data) = let Nc = updateCount Nc (List.map snd data)
                                    let Nk = updateCount Nk (List.map fst data |> List.map Set.toList |> List.concat)
                                    let Nck = Seq.groupBy snd data
                                              |> Seq.map (fun (key, xs) -> let ds = Seq.map fst xs |> Seq.map Set.toList
                                                                           let stats = Seq.fold (fun state ds -> updateCount state ds) Map.empty ds
                                                                           (key, stats))
                                              |> Map.ofSeq
                                    new ClassificationBPM<'a, 'b>(N + BigRational.FromInt(data.Length), Nc, Nk, Nck) :> Classification<'a, 'b>
        member this.InferLabel(features) = predictY features
