module Graphics
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms
open Erosion
open Fractals

/// Main program window.
type Graphics () =
    inherit GameWindow (640, 480, GraphicsMode.Default, "WorldView")

    ///roughness factor, lower is more rough, 0.45 to 0.65
    let H = 0.70
    ///how deep the recursive terrain generation goes, higher number means more detail
    let depth = 7
    ///range of random values possible, in +/-
    let range = 100
    ///how many times the range a height value has to be to show the maximum or minimum color, 2.0 to 5.0
    let rangeRatio = 5.0
    ///global heightmap for testing
    let mutable heightmap = Array2D.zeroCreate depth depth
    ///how stretched the heightmap is
    let scale = 3.0

    let aspect = 640.0f / 480.0f
    
    let mutable cameraPos = 0.0f

    let mutable angle = 0.0f

    let mutable theta = 0.0f
    let mutable phi = 0.0f

    let remap () =
        let r = new System.Random ()
        let seed () = r.Next(4 * range) - 2 * range
        Terrain3D H depth range (seed ()) (seed ()) (seed ()) (seed ())

    let erode toby =
        erode toby 40 40

    override this.OnLoad args =
        GL.Enable (EnableCap.DepthTest)
        GL.Enable EnableCap.Lighting
        GL.Enable EnableCap.Light0

        GL.Light (LightName.Light0, LightParameter.Position, Vector4.Normalize (Vector4 (-1.0f, -1.0f, -1.0f, 0.0f)))
        GL.Light (LightName.Light0, LightParameter.Ambient, Color4 (0.0f, 0.0f, 0.0f, 0.0f))
        GL.Light (LightName.Light0, LightParameter.Diffuse, Color4 (0.9f, 0.9f, 0.9f, 0.0f))
        GL.Light (LightName.Light0, LightParameter.Specular, Color4 (0.0f, 0.0f, 1.0f, 0.0f))

        GL.Light (LightName.Light1, LightParameter.Position, Vector4.Normalize (Vector4 (1.0f, 1.0f, 1.0f, 0.0f)))
        GL.Light (LightName.Light1, LightParameter.Ambient, Color4 (0.0f, 0.0f, 0.0f, 0.0f))
        GL.Light (LightName.Light1, LightParameter.Diffuse, Color4 (0.4f, 0.9f, 0.2f, 0.0f))
        GL.Light (LightName.Light1, LightParameter.Specular, Color4 (0.0f, 0.0f, 0.0f, 0.0f))

        this.Mouse.WheelChanged.Add (fun args -> (cameraPos <- cameraPos + (args.DeltaPrecise * 5.0f) |> min 0.0f))
        
        this.Keyboard.KeyDown.Add (fun args ->
            if args.Key = Input.Key.N then heightmap <- remap ()
            if args.Key = Input.Key.L then if GL.IsEnabled (EnableCap.Light1) then GL.Disable (EnableCap.Light1) else GL.Enable (EnableCap.Light1)
            if args.Key = Input.Key.E then heightmap <- erode heightmap)

        this.Keyboard.KeyRepeat <- true

        heightmap <- remap ()

    override this.OnKeyPress args =
        if (int) args.KeyChar =  27 then this.Close ()

    override this.OnRenderFrame args =
        GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
        GL.Enable (EnableCap.Lighting)

        let mutable perspective = Matrix4.CreatePerspectiveFieldOfView (1.2f, aspect, 0.1f, 1000.0f)
        let mutable lookat = Matrix4.LookAt (cos theta * cos phi * cameraPos, sin theta * cos phi * cameraPos, sin phi * cameraPos, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f)

        GL.MatrixMode (MatrixMode.Projection)
        GL.LoadIdentity ()
        GL.MultMatrix (ref perspective)
        GL.MultMatrix (ref lookat)
        
        GL.Light (LightName.Light0, LightParameter.Position, Vector4.Normalize (Vector4 ((cos theta * cos phi), -(sin theta * cos phi), -(sin phi), 0.0f)))

        GL.Begin BeginMode.Quads
        for i = 0 to (Array2D.length1 heightmap) - 3 do
            for j = 0 to (Array2D.length2 heightmap) - 3 do
                GL.Color3 (1.0f / ((float32) (2 * (i + 1))), 1.0f / ((float32) (3 / (i + 1))), 1.0f / ((float32) ((i + 1) / 20)))

                ///takes the x y position of a point on the heightmap and returns the normal
                let normal x y = 
                    let U = Vector3 (-1.0f, 0.0f, float32 (heightmap.[x + 1, y] - heightmap.[x, y]))
                    let V = Vector3 (0.0f, -1.0f, float32 (heightmap.[x, y + 1] - heightmap.[x, y]))
                    let outptu  = Vector3 (U.Y * V.Z - U.Z * V.Y, U.X * V.Z - U.Z * V.X, U.Y * V.X - U.Z * V.X)
                    Vector3.Normalize outptu

                GL.Normal3 (normal i j)
                GL.Vertex3 (scale * (float) (i - (Array2D.length1 heightmap) / 2), (scale * (float) (j - (Array2D.length2 heightmap) / 2)), (scale / 3.0) * heightmap.[i, j])
                GL.Normal3 (normal (i + 1) j)
                GL.Vertex3 (scale * (float) (i + 1 - (Array2D.length1 heightmap) / 2), (scale * (float) (j - (Array2D.length2 heightmap) / 2)), (scale / 3.0) * heightmap.[i + 1, j])
                GL.Normal3 (normal (i + 1) (j + 1))
                GL.Vertex3 (scale * (float) (i + 1 - (Array2D.length1 heightmap) / 2), (scale * (float) (j + 1 - (Array2D.length2 heightmap) / 2)), (scale / 3.0) * heightmap.[i + 1, j + 1])
                GL.Normal3 (normal i (j + 1))
                GL.Vertex3 (scale * (float) (i - (Array2D.length1 heightmap) / 2), (scale * (float) (j + 1 - (Array2D.length2 heightmap) / 2)), (scale / 3.0) * heightmap.[i, j + 1])

        GL.End ()

        this.SwapBuffers ()
    
    override this.OnUpdateFrame args =
        if this.Focused then
            Cursor.Hide ()
            let center = 
                Point  (this.Bounds.Left + this.Bounds.Width / 2, 
                        this.Bounds.Top + this.Bounds.Height / 2)
            let cursor = Cursor.Position
            let dX = cursor.X - center.X
            let dY = cursor.Y - center.Y
            Cursor.Position <- center
            theta <- theta - ((float32)dX / 100.0f)
            phi <- (phi - ((float32)dY / 100.0f) |> max ((float32)(-Math.PI / 2.000001)) |> min ((float32)(Math.PI / 2.000001)))
        else Cursor.Show ()

    override this.OnResize args =
        GL.Viewport (0, 0, this.Width, this.Height)