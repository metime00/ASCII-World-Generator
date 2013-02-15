module Fractals
open System

///H, depth, left, middle, right. returns an array of height values, where the middle index is the middle of the terrain. H is the roughness factor; depth is recursion depth, dealing with level of detail; leftInit, midInit, and rightInit are the initial values
let Terrain2D H depth leftInit midInit rightInit =
    let r = new System.Random ()
    [|
        let rec side left right (level : int) =
            let middle = (if left < right then left else right) + ((abs (left - right)) / 2) //finds the midpoint of the left and right values
            let seed = 
                let algPick = r.Next(101)//use random number to pick between two good but different height algorithms
                //algorithm that uses a shrinking number range
                if algPick > 65 then (float (if left < right then (r.Next(midInit / level) - midInit / (2 * level)) else (r.Next(midInit / level) - midInit / (2 * level)))) //gets a seed that checks whether or not left is the larger side, so no errors occur
                //algorithm that uses a range relative to the endpoints around it
                else (float (if left < right then (r.Next(left, right)) - (left + (right - left) / 2) else (r.Next(right, left)) - (right + (left - right) / 2))) //gets a seed that checks whether or not left is the larger side, so no errors occur
            let mid = int ((float middle) + seed / (H)) //displaces midpoint, more displacement from a lower H value
            [|
                if level < depth then 
                    yield! side left mid (level + 1) //yields the arrays in order, so that the values next to each other are spatially adjacent
                    yield mid
                    yield! side mid right (level + 1)
                else yield mid
            |]
        yield leftInit
        yield! side leftInit midInit 1 //yield! makes all of the nested arrays one array of heights
        yield midInit
        yield! side midInit rightInit 1
        yield rightInit
    |]

///creates 3d height map using midpoint displacement and diamond square algorithm
let Terrain3D (H : float) (depth : int) (range : int) topRight topLeft bottomRight bottomLeft =
    let map = Array2D.zeroCreate (int (2.0 ** (float depth)) + 1) (int (2.0 ** (float depth)) + 1)
    //assigns the starting values
    map.[(Array2D.length1 map - 1), (Array2D.length2 map - 1)] <- (float) topRight
    map.[0, (Array2D.length2 map - 1)] <- (float) topLeft
    map.[(Array2D.length1 map - 1), 0] <- (float) bottomRight
    map.[0, 0] <- (float) bottomLeft

    let r = new System.Random ()
    let initRange = (float) range //range is two thirds of the range given

    let rec square x0 y0 x1 y1 level =
        let middlex, middley = (abs (x0 - x1) / 2 + x0), (abs (y1 - y0) / 2 + y0) //the x and y midpoints
        let reduction = (2.0 ** (float)level) * (float (r.Next (13, 26)) * 0.01)  //how much the range is reduced per depth recursion
        let seed () = (float (r.Next((int) (initRange / reduction - 1.0))) + r.NextDouble () - (initRange  / reduction / 2.0)) / H //generates the random offset
        map.[middlex, middley] <- (map.[x0, y1] + map.[x1, y1] + map.[x0, y0] + map.[x1, y0]) / 4.0 + seed () //displaces midpoint

        map.[x0, middley] <- (map.[x0, y0] + map.[x0, y1]) / 2.0 + (r.NextDouble () * 2.0 - 1.0) //left side
        map.[x1, middley] <- (map.[x1, y0] + map.[x1, y1]) / 2.0 + (r.NextDouble () * 2.0 - 1.0) //right side
        map.[middlex, y0] <- (map.[x0, y0] + map.[x1, y0]) / 2.0 + (r.NextDouble () * 2.0 - 1.0) //bottom
        map.[middlex, y1] <- (map.[x0, y1] + map.[x1, y1]) / 2.0 + (r.NextDouble () * 2.0 - 1.0) //top

        if level < depth then
            square x0 y0 middlex middley (level + 1) //lower left
            square x0 middley middlex y1 (level + 1) //upper left
            square middlex middley x1 y1 (level + 1) //upper right
            square middlex y0 x1 middley (level + 1) //lower right

        ()    
    square 0 0 ((Array2D.length1 map) - 1) ((Array2D.length2 map) - 1) 1
    map