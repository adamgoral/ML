namespace ML.Console

module Samplers =
    let uniformDraw =
        let rand = new System.Random()
        fun min max -> rand.Next(min, max)

    let uniform = 
        let rand = new System.Random()
        rand.NextDouble

    let binomial p = 
        uniform() < p