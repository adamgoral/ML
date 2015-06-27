(*** hide ***)
#load "ML.Documentation.fsx"
#load @"packages\MathNet.Numerics.FSharp\MathNet.Numerics.fsx"
//#load @"packages\MathNet.Numerics.FSharp\MathNet.Numerics.IfSharp.fsx"
(**
Conditional Gaussian
==

First I will cover Gaussian model for conditional dependency between two Gaussian distributed random variables $x \sim N(\mu_x,\Sigma_x)$ and $y \sim N(\mu_y,\Sigma_y)$.

Given:
*)

open MathNet.Numerics.LinearAlgebra

let observe () = ((DenseVector.create 5 0.) , (DenseVector.create 2 0.))::[]

(**
$N \in \mathbb{N}_0$ number of observations

$D \in \mathbb{N}$ number of dimensions
*)

let X, Y = observe() |> List.unzip

let N = X.Length

let D = X.Head.Count

(**
$x=\begin{pmatrix}
x_1 \\
x_2 \\
\vdots \\
x_D \\
\end{pmatrix}$ random variable column vector

$\mu_x=\begin{pmatrix}
\mu_{x,1} \\
\mu_{x,2} \\
\vdots \\
\mu_{x,D} \\
\end{pmatrix}$ mean column vector

$\Sigma_x=\begin{pmatrix}
\Sigma_{x,1,1} & \Sigma_{x,1,2} & \cdots & \Sigma_{x,1,D}\\
\Sigma_{x,2,1} & \Sigma_{x,2,2} & \cdots & \Sigma_{x,2,D}\\
\vdots & \vdots & \ddots & \vdots \\
\Sigma_{x,D,1} & \Sigma_{x,D,2} & \cdots & \Sigma_{x,D,D}\\
\end{pmatrix}$ variance matrix

Multivariate gaussian distribution can be defined as:
$p(x|\mu_x,\Sigma_x) = (2\pi)^{-\frac{D}{2}}|\Sigma|^{-\frac{1}{2}}e^{\left[ -\frac{1}{2}(x-\mu_x)^T\Sigma^{-1}(x-\mu_x) \right]}$
*)

open System

let GaussianPdf (mu: Vector<float>) (sigma: Matrix<float>) =
    let lambda = sigma.Inverse()
    let d = float (mu.Count)
    let normalizer = Math.Pow ((2. * Math.PI), -d/2.) * Math.Pow(sigma.Determinant(), -1./2.)
    (fun (x: Vector<float>) -> let diff = x - mu
                               let exponent = diff * lambda * diff * -1./2.
                               normalizer * Math.Exp(exponent))
(**
It is known that mean can be estimated with MLE: $\hat\mu_x = \frac{1}{N} \sum_{n=1}^{N} x_n$
*)
let mu = 1./ (float N) * (List.fold (fun sum x -> sum + x) (DenseVector.zero D) X)
(**
and variance with MLE: $\hat\Sigma_x = \frac{1}{N} \sum_{n=1}^{N} (x_n-\hat\mu_n)^T(x_n-\hat\mu_n)$
*)
let Sigma = 1./(float N) * (List.fold (fun sum (x: Vector<_>) -> 
                                                    let diff = x - mu
                                                    let mul = diff.ToColumnMatrix().Multiply(diff.ToRowMatrix())
                                                    sum + mul) (DenseMatrix.zero D D) X)
(**
Joint gaussian distribution can be defined as:

$\mu=\begin{pmatrix}
\mu_x\\
\mu_y\\
\end{pmatrix}$,
$\Sigma=\begin{pmatrix}
\Sigma_{x,x} & \Sigma_{x, y} \\
\Sigma_{y,x} & \Sigma_{y, y} \\
\end{pmatrix}$,
$\Sigma^{-1}=\Lambda=\begin{pmatrix}
\Lambda_{x,x} & \Lambda_{x, y} \\
\Lambda_{y,x} & \Lambda_{y, y} \\
\end{pmatrix}$

$p(x)=N(x|\mu_x,\Sigma_{x,x})$

$p(y)=N(y|\mu_y,\Sigma_{y,y})$

$p(x,y) = N\left( \begin{pmatrix}x\\y\end{pmatrix}| \begin{pmatrix}\mu_x\\\mu_y\end{pmatrix}, \begin{pmatrix}\Sigma_{x,x}&\Sigma_{x,y}\\\Sigma_{y,x}&\Sigma_{y,y}\end{pmatrix} \right)
=\frac{1}{(2\pi)^{\frac{D}{2}}|\Sigma|^{\frac{1}{2}}} \space
e^{\left[-\frac{1}{2} 
\left( \begin{pmatrix}x\\y\end{pmatrix}-\begin{pmatrix}\mu_x\\\mu_y\end{pmatrix}\right)^T
\begin{pmatrix}\Lambda_{x,x} & \Lambda_{x,y} \\ \Lambda_{y,x} & \Lambda_{y,y}\end{pmatrix}
\left( \begin{pmatrix}x\\y\end{pmatrix}-\begin{pmatrix}\mu_x\\\mu_y\end{pmatrix}\right)
\right] }$

Let's define $\Lambda=\begin{pmatrix}
\Lambda_{x,x} & \Lambda_{x, y} \\
\Lambda_{y,x} & \Lambda_{y, y} \\
\end{pmatrix}$ in terms of elements of $\Sigma$ with help of Schur complements:

$M=\begin{pmatrix}E&F\\G&H\end{pmatrix}\\
\underbrace{\begin{pmatrix}I&-FH^{-1}\\0&I\end{pmatrix}}_{X}\begin{pmatrix}E&F\\G&H\end{pmatrix}=\begin{pmatrix}E-FH^{-1}G&0\\G&H\end{pmatrix}\\
\begin{pmatrix}E-FH^{-1}G&0\\G&H\end{pmatrix} \underbrace{\begin{pmatrix}I&0\\-H^{-1}G&I\end{pmatrix}}_Z=\underbrace{\begin{pmatrix}E-FH^{-1}G & 0 \\ 0 & H\end{pmatrix}}_W\\
XMZ=\begin{pmatrix}I&-FH^{-1}\\0&I\end{pmatrix}\begin{pmatrix}E&F\\G&H\end{pmatrix}\begin{pmatrix}I&0\\-H^{-1}G&I\end{pmatrix}=W\\
M=X^{-1}WZ^{-1}\\
M^{-1}=ZW^{-1}X\\
=\begin{pmatrix}I&0\\-H^{-1}G&I\end{pmatrix}\begin{pmatrix}E-FH^{-1}G & 0 \\ 0 & H\end{pmatrix}^{-1}\begin{pmatrix}I&-FH^{-1}\\0&I\end{pmatrix}\\
=\begin{pmatrix}I&0\\-H^{-1}G&I\end{pmatrix}\begin{pmatrix}(E-FH^{-1}G)^{-1} & 0 \\ 0 & H^{-1}\end{pmatrix}\begin{pmatrix}I&-FH^{-1}\\0&I\end{pmatrix}\\
=\begin{pmatrix}(E-FH^{-1}G)^{-1}&0\\-H^{-1}G(E-FH^{-1}G)^{-1}&H^{-1}\end{pmatrix}\begin{pmatrix}I&-FH^{-1}\\0&I\end{pmatrix}\\
=\begin{pmatrix}(E-FH^{-1}G)^{-1} & -(E-FH^{-1}G)^{-1}FH^{-1} \\ -H^{-1}G(E-FH^{-1}G)^{-1} & H^{-1}G(E-FH^{-1}G)^{-1}FH^{-1} + H^{-1}\end{pmatrix}
$

Lets apply this inversion and factor out conditional probability:

$p(x,y)=N\left( \begin{pmatrix}x\\y\end{pmatrix}| \mu,\Sigma \right) = \frac{1}{(2\pi\Sigma)^{\frac{D}{2}}}e^{-\frac{1}{2}E}$

Now the exponent $E$ can be defined as:

$E=\left( \begin{pmatrix}x\\y\end{pmatrix}-\begin{pmatrix}\mu_x\\\mu_y\end{pmatrix} \right)^T
\begin{pmatrix}
\Sigma_{x,x} & \Sigma_{x, y} \\
\Sigma_{y,x} & \Sigma_{y, y} \\
\end{pmatrix}^{-1}
\left( \begin{pmatrix}x\\y\end{pmatrix}-\begin{pmatrix}\mu_x\\\mu_y\end{pmatrix} \right)\\
=\begin{pmatrix}x-\mu_x\\y-\mu_y\end{pmatrix}^T
\begin{pmatrix}I&0\\-\Sigma_{y,y}^{-1}\Sigma_{y,x}&I\end{pmatrix}
\begin{pmatrix}(\Sigma_{x,x}-\Sigma_{x,y}\Sigma_{y,y}^{-1}\Sigma_{y,x})^{-1}&0\\0&\Sigma_{y,y}^{-1}\end{pmatrix}
\begin{pmatrix}I&-\Sigma_{x,y}\Sigma_{y,y}^{-1}\\0&I\end{pmatrix}
\begin{pmatrix}x-\mu_x\\y-\mu_y\end{pmatrix}\\
=\begin{pmatrix}x^T-\mu_x^T-(y-\mu_y)^T\Sigma_{yy}^{-1}\Sigma_{y,x}&(y-\mu_y)^T\end{pmatrix}
\begin{pmatrix}(\Sigma_{x,x}-\Sigma_{x,y}\Sigma_{y,y}^{-1}\Sigma_{y,x})^{-1}&0\\0&\Sigma_{y,y}^{-1}\end{pmatrix}
\begin{pmatrix}x-\mu_x-\Sigma_{x,y}\Sigma_{y,y}^{-1}(y-\mu_y)\\y-\mu_y\end{pmatrix}\\
=\begin{pmatrix}(x^T-\mu_x^T-(y-\mu_y)^T\Sigma_{yy}^{-1}\Sigma_{y,x})(\Sigma_{x,x}-\Sigma_{x,y}\Sigma_{y,y}^{-1}\Sigma_{y,x})^{-1}&(y-\mu_y)^T\Sigma_{y,y}^{-1}\end{pmatrix}
\begin{pmatrix}x-\mu_x-\Sigma_{x,y}\Sigma_{y,y}^{-1}(y-\mu_y)\\y-\mu_y\end{pmatrix}\\
=(x^T-\mu_x^T-(y-\mu_y)^T\Sigma_{yy}^{-1}\Sigma_{y,x})(\Sigma_{x,x}-\Sigma_{x,y}\Sigma_{y,y}^{-1}\Sigma_{y,x})^{-1}(x-\mu_x-\Sigma_{x,y}\Sigma_{y,y}^{-1}(y-\mu_y))
+(y-\mu_y)^T\Sigma_{y,y}^{-1}(y-\mu_y)\\
=(x-\mu_x-\underbrace{\Sigma_{y,x}^T}_{\Sigma_{x,y}}\Sigma_{yy}^{-1}(y-\mu_y))^T(\Sigma_{x,x}-\Sigma_{x,y}\Sigma_{y,y}^{-1}\Sigma_{y,x})^{-1}(x-\mu_x-\Sigma_{x,y}\Sigma_{y,y}^{-1}(y-\mu_y))
+(y-\mu_y)^T\Sigma_{y,y}^{-1}(y-\mu_y)\\
=(x-\underbrace{\mu_x-\Sigma_{x,y}\Sigma_{yy}^{-1}(y-\mu_y)}_{\mu_{x|y}})^T {\underbrace{(\Sigma_{x,x}-\Sigma_{x,y}\Sigma_{y,y}^{-1}\Sigma_{y,x})}_{\Sigma_{x|y}}}^{-1}
(x-\underbrace{\mu_x-\Sigma_{x,y}\Sigma_{yy}^{-1}(y-\mu_y)}_{\mu_{x|y}})
+(y-\mu_y)^T\Sigma_{y,y}^{-1}(y-\mu_y)$

Now in terms of conditional variance and conditional mean, it is equal to:

$E=(x-\mu_{x|y})^T \Sigma_{x|y} (x-\mu_{x|y}) + (y-\mu_y)^T\Sigma_{y,y}(y-\mu_y)$

$p(x,y)\propto e^{-\frac{1}{2}E}\\
\propto e^{-\frac{1}{2} \left[ (x-\mu_{x|y})^T \Sigma_{x|y} (x-\mu_{x|y}) + (y-\mu_y)^T\Sigma_{y,y}(y-\mu_y) \right] }\\
\propto e^{-\frac{1}{2} \left[ (x-\mu_{x|y})^T \Sigma_{x|y} (x-\mu_{x|y}) \right] }
\times
e^{-\frac{1}{2} \left[ (y-\mu_y)^T\Sigma_{y,y}(y-\mu_y) \right] }$

$p(x,y)=p(x|y)\times p(y)
=N(x|\mu_{x|y},\Sigma_{x|y})\times N(y|\mu_y,\Sigma_y)$

Where:

Conditional mean $\mu_{x|y}=\mu_x+\Sigma_{x,y}\Sigma_{yy}^{-1}(y-\mu_y)$

Conditional variance $\Sigma_{x|y}=\Sigma_{x,x}-\Sigma_{x,y}\Sigma_{y,y}^{-1}\Sigma_{y,x}$

which comes from above expressions (in underbraces).

Parameter inference
--

$p(\mu,\Sigma)=p(\mu|\Sigma)p(\Sigma) \\
=N(\mu|m,\frac{1}{k}\Sigma)\times IW(\Sigma|S,v)=NIW(\mu,\Sigma|m,k,S,v)$

$p(\mu,\Sigma|D)\propto p(D|\mu,\Sigma)\times NIW(\mu,\Sigma|m_0,k_0,S_0,v_0)\\
=NIW(\mu,\Sigma|m_n,k_n,S_n,v_n)=N(\mu|m_n,\Sigma)\times IW(\Sigma|S_n,v_n)\\
=N \left(\mu|\frac{m_0k_0+N\bar x}{k_n},\Sigma \right)\times IW \left(\Sigma|S_0+S+k_0 m_0 m_0^T - k_n \bar x \bar x^T,v_n \right)$

where

$S=\sum_{i=1}^N x_i x_i^T\\
v_n=v_0+N
k_n=k_0+N$

$E[p(\mu,\Sigma|D)]=\left(m_n, \frac{S_n}{v_n-D-1} \right)$
*)