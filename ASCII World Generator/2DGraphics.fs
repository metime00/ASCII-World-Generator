module _2DGraphics

open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms
open Erosion
open Fractals

type Window () as this =
    inherit Form ()
    
    let remap () =
        let r = new System.Random ()
        let seed () = r.Next(4 * 50) - 2 * 50
        Terrain3D 0.35 9 50 (seed ()) (seed ()) (seed ()) (seed ())

    let height = 512

    let width = 512

    let bitmap = new Bitmap (width, height)

    let redraw () =
        let mapR = remap ()
        let mapG = remap ()
        let mapB = remap ()

        for i = 0 to bitmap.Width - 1 do
            for j = 0 to bitmap.Height - 1 do
                bitmap.SetPixel (i, j, Color.FromArgb(abs((int) mapR.[i,j]), abs((int) mapG.[i,j]), abs((int) mapB.[i,j])))

    do
        this.Text <- "Avant Garde Terrain Generation"
        this.Width <- width
        this.Height <- height
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.MaximizeBox <- false
        this.BackColor <- Color.White

    override this.OnMouseDown args =
        redraw ()
        this.Invalidate ()

    override this.OnPaint args =
        args.Graphics.DrawImageUnscaled (bitmap, 0, 0)
        