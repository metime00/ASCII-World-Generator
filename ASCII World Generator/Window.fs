module Window

open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms
open Fractals
open Erosion


type Window () as this =
    inherit Form ()

    ///roughness factor, lower is more rough
    let H = 0.65
    ///how deep the recursive terrain generation goes, higher number means more detail
    let depth = 9
    ///range of random values possible, in +/-
    let range = 900
    ///light direction
    let mutable lightVector = (1, 1, -1)
    ///how many times the range a height value has to be to show the maximum or minimum color
    let rangeRatio = 2.0
    ///takes lightVector as argument, returns rotated to direction of second argument
    let lightRot (x1, y1) (x, y, z) = (x1, y1, z)
    ///last x mouse position
    let mutable x = 0
    ///last y mouse position
    let mutable y = 0
    ///global heightmap for testing
    let mutable heightmap1 = Array2D.zeroCreate depth depth
    ///global heightmap for testing
    let mutable heightmap2 = Array2D.zeroCreate depth depth
    ///global heightmap for testing
    let mutable heightmap3 = Array2D.zeroCreate depth depth

    do
        this.Text <- "PubarGen"
        this.Width <- 513
        this.Height <- 513
        this.Cursor <- Cursors.Cross
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.White

    ///main bitmap
    let mutable (mit : Bitmap) = new System.Drawing.Bitmap (this.Width, this.Height)

    ///draws the horizon using the Terrain2D function and two points
    let drawHorizon x0 y0 x1 y1 y2 =
        let horizon = Terrain2D H depth y0 y2 y1//makes the array of the horizon line
        let mitDraw = Drawing.Graphics.FromImage (mit)//creates the graphics object that will modify mit
        let step = (float) (abs (x0 - x1)) / (float) horizon.Length
        let mitPen = new Pen (Color.Goldenrod)
        for i = 0 to horizon.Length - 2 do
            mitDraw.DrawLine (mitPen, int ((float i) * step + float x0), horizon.[i], int (float (i + 1) * step + float x0), horizon.[i + 1])

    ///generates heightmap 1 2 and 3 and saves them
    let genHeightMap x0 y0 =
        let r = new System.Random ()
        let seed () = r.Next(8 * range) - 4 * range
        heightmap1 <- Terrain3D H depth range (seed ()) (seed ()) (seed ()) (seed ())
        heightmap2 <- Terrain3D H depth range (seed ()) (seed ()) (seed ()) (seed ())
        heightmap3 <- Terrain3D H depth range (seed ()) (seed ()) (seed ()) (seed ())
    
    ///takes a height value and converts it to a usable value for argb
    let heightCap height =
        let scaledHeight = int (((float height) / (float range)) * (127.5  / rangeRatio))
        if scaledHeight > 127 then 255
        elif scaledHeight < -127 then 0
        else scaledHeight + 127

    ///takes a normal and light vector and figures out how bright it is
    let lightCap (x0, y0, z0) (x1, y1, z1) =
        let dot = x0 * x1 + y0 * y1 + z0 * z1
        if dot > 255 then 255
        else if dot < 0 then 0
        else dot

    ///draws 3d heightmap with the coordinates drawn from corner to corner
    let drawHeightMap x0 y0 =
        let mitDraw = Drawing.Graphics.FromImage (mit)//creates the graphics object that will modify mit

        let mutable newMit = new System.Drawing.Bitmap (Array2D.length1 heightmap1, Array2D.length2 heightmap1)
        for i = 0 to (Array2D.length1 heightmap1) - 1 do
            for j = 0 to (Array2D.length2 heightmap1) - 1 do
                let heightColor = Color.FromArgb (heightCap (1 * heightmap1.[i, j]), heightCap (1 * heightmap1.[i, j]), heightCap (1 * heightmap1.[i, j]))
                newMit.SetPixel(i, j, heightColor)

        mitDraw.DrawImage(newMit, 0, 0, mit.Width, mit.Height)

    ///draws a lightmap from the heightmaps in memory
    let drawLightMap X Y =
        let mitDraw = Drawing.Graphics.FromImage (mit)//creates the graphics object that will modify mit

        let mutable newMit = new System.Drawing.Bitmap (Array2D.length1 heightmap1, Array2D.length2 heightmap1)
        for i = 0 to (Array2D.length1 heightmap1) - 2 do //normalize and compare light vector and the normal
            for j = 0 to (Array2D.length2 heightmap1) - 2 do
                let normal (heightmap : int [,]) = ((heightmap.[i + 1, j] - heightmap.[i, j]), (heightmap.[i, j + 1] - heightmap.[i, j]), 1) //get normal from pixel below and to the right
                let heightColor = Color.FromArgb (lightCap (normal heightmap1) lightVector, lightCap (normal heightmap1) lightVector, lightCap (normal heightmap1) lightVector)
                newMit.SetPixel(i, j, heightColor)

        mitDraw.DrawImage(newMit, 0, 0, mit.Width, mit.Height)

    override this.OnPaint args =
        if mit <> null then args.Graphics.DrawImageUnscaled (mit, 0, 0)

    override this.OnMouseDown args =
        match args.Button with
        | MouseButtons.Right -> 
            drawLightMap args.X args.Y
        | MouseButtons.Middle ->
            heightmap1 <- erode heightmap1 args.X args.Y
//            heightmap2 <- erode heightmap2 args.X args.Y
//            heightmap3 <- erode heightmap3 args.X args.Y
        | MouseButtons.Left ->
            drawHeightMap args.X args.Y
//            x <- args.X
//            y <- args.Y
        | _ -> ()

    override this.OnMouseUp args =
        ()

    override this.OnKeyDown args =
        if args.Alt then mit <- new System.Drawing.Bitmap (this.Width, this.Height)
        if args.Control then
            genHeightMap 0 0
            drawHeightMap 0 0
        if args.KeyCode = Keys.L then //rotate light source
            if lightVector = (1, 1, -1) then lightVector <- lightRot (1, -1) lightVector
            elif lightVector = (1, -1, -1) then lightVector <- lightRot (-1, -1) lightVector
            elif lightVector = (-1, -1, -1) then lightVector <- lightRot (-1, 1) lightVector
            elif lightVector = (-1, 1, -1) then lightVector <- lightRot (1, 1) lightVector
            drawLightMap 0 0
        if args.KeyCode = Keys.M then //mirror light source
            if lightVector = (1, 1, -1) then lightVector <- lightRot (-1, -1) lightVector
            elif lightVector = (1, -1, -1) then lightVector <- lightRot (-1, 1) lightVector
            elif lightVector = (-1, -1, -1) then lightVector <- lightRot (1, 1) lightVector
            elif lightVector = (-1, 1, -1) then lightVector <- lightRot (1, -1) lightVector
            drawLightMap 0 0
        if args.KeyCode = Keys.D then //detensify light
            lightVector <- lightVector |> (fun (x, y, z) -> (x, y, z - 100))
            drawLightMap 0 0
        if args.KeyCode = Keys.I then //intensify light
            lightVector <- lightVector |> (fun (x, y, z) -> (x, y, z + 100))
            drawLightMap 0 0