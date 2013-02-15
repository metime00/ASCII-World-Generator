module Erosion

open System
open System.Collections.Generic

///how much water per square of rain, should be 45
let rainDensity = 25.0
///how large of a rain cloud there is, should be 1000
let rainSize = 400
///how long a rain cloud rains, should be 120
let rainDuration = 80
///how much of the water evaporates per iteration in percentage, should be 0.6
let evaporation = 0.6
///how much sediment a unit of water can hold, should be 0.1
let solubility = 0.1

///if the first value is less than the second, return 0.0, else return the first.
let flooredDiff first second =
    if first < second then 0.0
    else first

///sum of all values in a 2D array of floats
let sum2D (map : float [,]) =
    let mutable sum = 0.0
    for i = 0 to Array2D.length1 map - 1 do
        for j = 0 to Array2D.length2 map - 1 do
            sum <- sum + (flooredDiff map.[i,j] 0.0005)
    sum

///checks which direction water should flow, or if it should stay still, and the amount of water that should flow
let flowDirection x y (map : float [,]) (waterMap : float [,]) =
    let mutable lowx = x
    let mutable lowy = y
    let difference i j lowx lowy = map.[x + i, y + j] + (waterMap.[x + i, y + j] / 2.0) - map.[lowx, lowy] + (waterMap.[lowx, lowy] / 2.0)
    for i = -1 to 1 do
        for j = -1 to 1 do
            if x + i > 0 && x + i < Array2D.length1 map
            && y + j > 0 && y + j < Array2D.length2 map then
                if difference i j lowx lowy < 0.0 then
                    lowx <- x + i
                    lowy <- y + j
    ///allow for level multitile bodies of water
    let output =
        if difference 0 0 lowx lowy < waterMap.[x, y] then difference 0 0 lowx lowy
        else waterMap.[x, y]
    (lowx, lowy, output)

///erodes a heightmap based on a given x y coordinate for rain cloud position
let erode (initMap : float [,]) x0 y0 =
    let heightmap = Array2D.zeroCreate (Array2D.length1 initMap) (Array2D.length2 initMap)
    for i = 0 to Array2D.length1 initMap - 1 do //make float heightmap from the initial int heightmap
        for j = 0 to Array2D.length2 initMap - 1 do
            heightmap.[i,j] <- initMap.[i,j]
    let mutable waterMap = Array2D.zeroCreate (Array2D.length1 heightmap) (Array2D.length2 heightmap) //create map of water location
    let rain (map : float [,]) = //define how rain is distributed
        let r = new Random ()
        for i = -(rainSize / 2) to rainSize / 2 do
            for j = -(rainSize / 2) to (rainSize / 2) do
                if x0 + i > 0 && x0 + i < Array2D.length1 map
                && y0 + j > 0 && y0 + j < Array2D.length2 map then map.[x0 + i, y0 + j] <- map.[x0 + i, y0 + j] + rainDensity
        map

    waterMap <- rain waterMap//initial rain
    let mutable time = 0
    while sum2D waterMap > 0.0 || time < rainDuration do
        
        //erode
        for i = 0 to Array2D.length1 waterMap - 1 do
            for j = 0 to Array2D.length2 waterMap - 1 do
                heightmap.[i,j] <- heightmap.[i,j] - waterMap.[i,j] * solubility

        //water flow
        for i = 0 to Array2D.length1 waterMap - 1 do
            for j = 0 to Array2D.length2 waterMap - 1 do
                let (x, y, amount) = flowDirection i j heightmap waterMap
                waterMap.[i, j] <- waterMap.[i, j] - amount
                waterMap.[x, y] <- waterMap.[x, y] + amount
        //evaporate and deposit sediment
        for i = 0 to Array2D.length1 waterMap - 1 do
            for j = 0 to Array2D.length2 waterMap - 1 do
                let lost = waterMap.[i, j] * evaporation
                waterMap.[i, j] <- flooredDiff (waterMap.[i, j] - lost) 0.0
                heightmap.[i, j] <- heightmap.[i, j] + lost * solubility


        time <- time + 1
        if time < rainDuration then waterMap <- rain waterMap//rain before water is checked
        printfn "%i" time
        printfn "%f" (sum2D waterMap)
        printfn "%f" (sum2D heightmap)

        for i = 0 to Array2D.length1 heightmap - 1 do //make float heightmap into output int heightmap
            for j = 0 to Array2D.length2 heightmap - 1 do
                initMap.[i,j] <- heightmap.[i,j]
    printfn "done"
    initMap