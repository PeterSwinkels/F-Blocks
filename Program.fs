//F Blocks v1.02 - by: Peter Swinkels, ***2019***
//This program's imported namespaces.
open System
open System.Diagnostics
open System.Drawing
open System.Reflection
open System.Windows.Forms

//This module contains this program's core procedures and interface window class.
module private CoreModule =
   type public ShapesE = I = 0|J = 1|L = 2|O = 3|S = 4|T = 5|Z = 6   //This enum lists the different shapes used.

   let public BLOCK_SCALE = 48                        //Defines the scale at which the blocks are drawn.
   let public NO_COLOR = Color.FromArgb(0, 0, 0, 0)   //Defines a lack of color.
   let public PIT_HEIGHT = 16                         //Defines the pit's height.
   let public PIT_WIDTH = 10                          //Defines the pit's width.
      
   //This structure defines the game's state.
   type public GameStateStr =
      struct
         val GameOver: bool           //Indicates whether the game has been lost.
         val Pit: List<List<Color>>   //Defines the pit.
         val Score: uint64            //Defines the number of rows cleared.
         new (gameOver: bool, pit: List<List<Color>>, score: uint64) = {GameOver = gameOver; Pit = pit; Score = score}
      end
   
   //This structure defines a shape.
   type public ShapeStr =
      struct
         val Angle: int               //Defines a shape's angle. (range 0-3 (x 90 = degrees.))
         val Dimensions: Rectangle    //Defines a shape's dimensions.
         val DropRate: int            //Defines the length of the interval between a shape's drops.
         val Map: List<List<Color>>   //Defines a shape's map.
         val PitXY: Point             //Defines a shape's position.
         val Shape: ShapesE           //Defines a shape.
         new (angle: int, dimensions: Rectangle, dropRate: int, map: List<List<Color>>, pitxy: Point, shape: ShapesE) = {Angle = angle; Dimensions = dimensions; DropRate = dropRate; Map = map; PitXY = pitxy; Shape = shape;}
      end

   //This function returns an indicator of whether the specified shape at the specified position can move in the specified direction inside the specified pit. 
   let private CanMove (map:List<List<Color>>) (xy:Point) (direction:Point) (pit:List<List<Color>>) =
      let rec BlockY y movable = 
         if y < map.Length then
            let rec BlockX x movable = 
               if x >= map.[y].Length then
                  movable
               else
                  if movable then
                     if map.[y].[x] = NO_COLOR then
                        BlockX (x + 1) movable
                     else
                        let NewX = (xy.X + x) + direction.X
                        let NewY = (xy.Y + y) + direction.Y

                        if NewY < 0 then
                           if NewX < 0 || NewX >= PIT_WIDTH  then
                              BlockX (x + 1) false
                           else
                              BlockX (x + 1) movable
                        else if NewX < 0 || NewX >= PIT_WIDTH || NewY >= PIT_HEIGHT then
                           BlockX (x + 1) false
                        else if not (pit.[NewY].[NewX] = NO_COLOR) then
                           BlockX (x + 1) false
                        else
                           BlockX (x + 1) movable
                  else
                     BlockX (x + 1) movable
            BlockY (y + 1) (BlockX 0 movable)
         else
            movable
      BlockY 0 true

   //This function returns the left and right most positions of the colored blocks in the specified shape.
   let private GetShapeLeftRightSides (map:List<List<Color>>) = 
      let rec BlockY y left right = 
         if y < map.Length then
            let rec BlockX x left right = 
               if x < map.[y].Length then
                  if map.[y].[x] = NO_COLOR then
                     BlockX (x + 1) left right
                  else
                     BlockX (x + 1) (if x <= left then x else left) (if x >= right then x else right)
               else
                  (left, right)
            let (left, right) = BlockX 0 left right
            BlockY (y + 1) left right
         else
            (left, right)
      BlockY 0 Int32.MaxValue Int32.MinValue

   //This function counts which of the specified columns are colored and returns the result.
   let private ColoredColumnCount columns = 
      (List.filter(fun column -> not (column = NO_COLOR)) columns).Length

   //This procedure checks the specified shape's colored block positions and returns the rectangular area containing colored blocks.
   let private GetShapeDimensions (map:List<List<Color>>) = 
      let (Left, Right) = GetShapeLeftRightSides map
      let Top = List.findIndex(fun columns -> ColoredColumnCount columns > 0) map
      let Bottom = List.findIndexBack(fun columns -> ColoredColumnCount columns > 0) map

      new Rectangle(Left, Top, Right - Left, Bottom - Top)
  
   //This function returns the specified shape's map.
   let private GetShapeMap (shape:ShapesE) =
      match shape with
      | ShapesE.I -> [[NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]; [Color.Cyan; Color.Cyan; Color.Cyan; Color.Cyan]; [NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]; [NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]]
      | ShapesE.J -> [[NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]; [Color.Blue; Color.Blue; Color.Blue; NO_COLOR]; [NO_COLOR; NO_COLOR; Color.Blue; NO_COLOR]; [NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]]
      | ShapesE.L -> [[NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]; [Color.Orange; Color.Orange; Color.Orange; NO_COLOR]; [Color.Orange; NO_COLOR; NO_COLOR; NO_COLOR]; [NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]]
      | ShapesE.O -> [[NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]; [NO_COLOR; Color.Yellow; Color.Yellow; NO_COLOR]; [NO_COLOR; Color.Yellow; Color.Yellow; NO_COLOR]; [NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]]
      | ShapesE.S -> [[NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]; [NO_COLOR; Color.Green; Color.Green; NO_COLOR]; [Color.Green; Color.Green; NO_COLOR; NO_COLOR]; [NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]]
      | ShapesE.T -> [[NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]; [Color.Purple; Color.Purple; Color.Purple; NO_COLOR]; [NO_COLOR; Color.Purple; NO_COLOR; NO_COLOR]; [NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]]
      | ShapesE.Z -> [[NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]; [Color.Red; Color.Red; NO_COLOR; NO_COLOR]; [NO_COLOR; Color.Red; Color.Red; NO_COLOR]; [NO_COLOR; NO_COLOR; NO_COLOR; NO_COLOR]]
      | _ -> []

   //This function returns the specified shape map rotated at the specified angle.
   let private GetRotatedShapeMap (shape:ShapesE) angle = 
      let Map = GetShapeMap shape

      let rec BlockY y rotatedMap =
         if y < Map.Length then
            let rec BlockX x columns =
               if x < Map.Length then
                  let xy = 
                     match angle with
                     | 1 ->
                        new Point(y, (Map.[y].Length - 1) - x)
                     | 2 ->
                        new Point((Map.[y].Length - 1) - x, (Map.Length - 1) - y)
                     | 3 ->
                        new Point((Map.Length - 1) - y, x)
                     | _ ->
                        new Point(x, y)
                  BlockX (x + 1) (columns @ [Map.[xy.Y].[xy.X]])
               else
                  columns
            BlockY (y + 1) (rotatedMap @ [(BlockX 0 [])])
         else
            rotatedMap
      BlockY 0 []

   //This function creates and returns a shape and enables the dropper.
   let private CreateShape (randomO:Random) = 
      let Angle = randomO.Next(0, 3)
      let DropRate = 1000
      let Shape = enum<ShapesE>(randomO.Next(ShapesE.I |> int, ShapesE.Z |> int))
      let Map = GetRotatedShapeMap Shape Angle
      let Dimensions = GetShapeDimensions Map
      let PitXY = new Point(randomO.Next(-Dimensions.X, (PIT_WIDTH - 1) - Dimensions.Width), -(Dimensions.Y + Dimensions.Height))
     
      new ShapeStr(angle = Angle, dimensions = Dimensions, dropRate = DropRate, map = Map, pitxy = PitXY, shape = Shape)

    //This function displays the game's status on the specified graphical surface.
   let private DisplayStatus (gameState:GameStateStr) (canvas:Graphics) = 
      let Text = if gameState.GameOver then "Game over - press Escape." else String.Format("Score: {0}", gameState.Score)

      canvas.Clear(Color.Black)
      canvas.DrawString(Text, new Font("Comic Sans MS", 16.0F, FontStyle.Bold), Brushes.Red, new PointF(0.0F, 0.0F))

   //This function draws a block of the specified color at the specified position in the specified image box.
   let private DrawBlock (colorO:Color) (xy:Point) (pitBox:PictureBox) =
      let x = xy.X * BLOCK_SCALE
      let y = xy.Y * BLOCK_SCALE

      pitBox.CreateGraphics().FillRectangle(new SolidBrush(colorO), x, y, BLOCK_SCALE, BLOCK_SCALE)
      pitBox.CreateGraphics().DrawRectangle(Pens.Black, x + (BLOCK_SCALE / 10) |> int, y + (BLOCK_SCALE / 10) |> int, BLOCK_SCALE - (BLOCK_SCALE / 5) |> int, BLOCK_SCALE - (BLOCK_SCALE / 5) |> int)
 
   //This function draws the specified pit reflecting the specified game state.
   let private DrawPit (pitBox:PictureBox) (gameState:GameStateStr) = 
      List.iteri (fun y columns -> (List.iteri (fun x column -> DrawBlock (if column = NO_COLOR then Color.Black else if gameState.GameOver then Color.Red else column) (new Point(x, y)) pitBox) columns)) gameState.Pit

    //This function draws/erases the specified shape at the specified position in the specified image box.
   let private DrawShape (shape:ShapeStr) (pit:List<List<Color>>) (pitBox:PictureBox) eraseShape = 
      let Block x y = 
         if y < shape.Map.Length && x < shape.Map.[y].Length then
            let PitX = shape.PitXY.X + x
            let PitY = shape.PitXY.Y + y
            if PitX >= 0 && PitX < PIT_WIDTH && PitY >= 0 && PitY < PIT_HEIGHT then
               DrawBlock (if eraseShape then (if pit.[PitY].[PitX] = NO_COLOR then Color.Black else pit.[PitY].[PitX]) else shape.Map.[y].[x]) (new Point(PitX, PitY)) pitBox
            else
               () 
          else
             ()
      List.iteri (fun y columns -> (List.iteri (fun x column -> Block x y) columns)) pit
      
   //This function returns whether the game has been lost and takes action to reflect the game's state.
   let private CheckGameState (shape:ShapeStr) (canvas:Graphics) (dropper:Timer)(gameState:GameStateStr) (pitBox:PictureBox) =
      dropper.Enabled <- not gameState.GameOver
      DrawPit pitBox gameState
      DisplayStatus gameState canvas
      (shape.PitXY.Y < 0)
  
   //This function checks whether the active block covers the specified location in the pit and if so returns its color.
   let private GetBlockAt (pitXY:Point) (shape:ShapeStr) = 
      let blockX = pitXY.X - shape.PitXY.X
      let blockY = pitXY.Y - shape.PitXY.Y

      let colorO = 
         if blockX >= shape.Dimensions.Left && blockX <= shape.Dimensions.Right && blockY >= shape.Dimensions.Top && blockY <= shape.Dimensions.Bottom then
            shape.Map.[blockY].[blockX]
         else
            NO_COLOR
      colorO

   //This function adds rows to the specified pit until it has reached the required size.
   let private AddRows pit = 
      let rec PitY y (newPit:List<List<Color>>) = 
         if newPit.Length < PIT_HEIGHT then
            let rec PitX x columns = 
               if x < PIT_WIDTH then
                  PitX (x + 1) (columns @ [NO_COLOR])
               else
                  columns         
            PitY (y + 1) ([(PitX 0 [])] @ newPit)
         else
            newPit
      PitY 0 pit

   //This function initializes the specified dropper and returns a random block and the game's state.
   let private InitializeGame (dropper:Timer) (randomO:Random) = 
      let Block = CreateShape randomO
      let GameState =  new GameStateStr(gameOver = false, pit = AddRows [], score = 0UL)
      
      dropper.Enabled <- true
      dropper.Interval <- Block.DropRate
      
      (Block, GameState)

   //This function returns the specified pit after removing any full rows and also returns the new score.
   let private RemoveFullRows (pit:List<List<Color>>) (gameState:GameStateStr) = 
      let ClearedPit = List.filter(fun columns -> not (List.forall(fun colorO -> not (colorO = NO_COLOR)) columns)) pit
      
      new GameStateStr(gameOver = gameState.GameOver, pit = AddRows ClearedPit, score = gameState.Score + ((pit.Length - ClearedPit.Length) |> uint64))

   //This function settles the active shape in the pit and gives the command to check for and remove any full rows.
   let private SettleActiveShape (shape:ShapeStr) (gameState:GameStateStr) = 
      let rec PitY y newPit =
         if y < PIT_HEIGHT then
            let rec PitX x columns =
               if x >= PIT_WIDTH then
                  columns
               else
                  let ColorO = GetBlockAt (new Point(x, y)) shape
                  PitX (x + 1) (columns @ [(if ColorO = NO_COLOR then gameState.Pit.[y].[x] else ColorO)])
            PitY (y + 1) (newPit @ [PitX 0 []])
         else
            newPit
      RemoveFullRows (PitY 0 []) gameState
    
   let mutable private ActiveShape = new ShapeStr(angle = 0, dimensions = new Rectangle(), dropRate = 0, map = [], pitxy = new Point(), shape = ShapesE.I)   //Contains the active shape.
   let mutable private GameState = new GameStateStr(gameOver = false, pit = [], score = 0UL)                                                              //Contains the game's state.
   let private Dropper = new Timer(Enabled = false)                                                                                                       //This manages the dropping of the active block.
   let private PitBox = new PictureBox(BackColor = Color.Black, Height = PIT_HEIGHT * BLOCK_SCALE, Left = 0, Top = 48, Width = PIT_WIDTH * BLOCK_SCALE)   //Contains the pit box's graphics.    
   let private RandomO = new Random()                                                                                                                     //Contains the random number generator.

   //This class contains this program's main interface window.
   type private InterfaceWindow() as Form =       
      inherit Form()
      do Form.InitializeForm

      //This procedure initializes this window and gives the command to initialize the game.
      member private this.InitializeForm = 
         let ProgramInformation = FileVersionInfo.GetVersionInfo(Assembly.GetExecutingAssembly().Location) 

         Dropper.Tick.AddHandler(new EventHandler (fun sender eventArgs -> this.Dropper_Tick(sender, eventArgs)))

         this.BackColor <- Color.Black 
         this.Controls.Add(PitBox)
         this.FormBorderStyle <- FormBorderStyle.Fixed3D
         this.Height <- this.Height + ((PitBox.Height + PitBox.Top) - this.ClientSize.Height)
         this.KeyPreview <- true
         this.KeyUp.AddHandler(new KeyEventHandler (fun sender keyEventArgs -> this.Form_KeyUp(sender, keyEventArgs)))
         this.MaximizeBox <- false
         this.Paint.AddHandler(new PaintEventHandler (fun sender paintEventArgs -> this.Form_Paint(sender, paintEventArgs)))
         this.StartPosition <- FormStartPosition.CenterScreen 
         this.Text <- String.Format("{0} v{1} - by: {2}", ProgramInformation.ProductName, ProgramInformation.ProductVersion, ProgramInformation.CompanyName)                 
         this.Width <- this.Width + (PitBox.Width - this.ClientSize.Width)
         
         let NewBlock, NewState = InitializeGame Dropper RandomO
         ActiveShape <- NewBlock
         GameState <- NewState
         this.Invalidate()

      //This procedure drops the active block until it cannot continue dropping.
      member private this.Dropper_Tick(sender:Object, e:EventArgs) =
         if CanMove (ActiveShape.Map) ActiveShape.PitXY (new Point(0, 1)) GameState.Pit then
            DrawShape ActiveShape GameState.Pit PitBox true
            ActiveShape <- new ShapeStr(angle = ActiveShape.Angle, dimensions = ActiveShape.Dimensions, dropRate = ActiveShape.DropRate, map = ActiveShape.Map, shape = ActiveShape.Shape,pitxy = new Point(ActiveShape.PitXY.X, ActiveShape.PitXY.Y + 1))
            DrawShape ActiveShape GameState.Pit PitBox false
         else 
            GameState <- SettleActiveShape ActiveShape GameState
            GameState <- new GameStateStr(gameOver = (CheckGameState ActiveShape (this.CreateGraphics()) Dropper GameState PitBox), pit = GameState.Pit, score = GameState.Score)
            if GameState.GameOver then
               ()
            else               
               ActiveShape <- CreateShape RandomO
               Dropper.Enabled <- true
               Dropper.Interval <- ActiveShape.DropRate
               DrawShape ActiveShape GameState.Pit PitBox true
               
      //This procedure handles the user's keystrokes.
      member private this.Form_KeyUp(sender:Object, e:KeyEventArgs) = 
         if GameState.GameOver then
            if e.KeyCode = Keys.Escape then
               let NewBlock, NewState = InitializeGame Dropper RandomO
               ActiveShape <- NewBlock
               GameState <- NewState
               this.Invalidate()
            else
               ()
         else
            match e.KeyCode with
            | Keys.A ->
               DrawShape ActiveShape GameState.Pit PitBox true
               let NewAngle = if ActiveShape.Angle = 3 then 0 else ActiveShape.Angle + 1
               let RotatedMap = GetRotatedShapeMap ActiveShape.Shape NewAngle
               if CanMove RotatedMap ActiveShape.PitXY (new Point(0, 0)) GameState.Pit then
                  ActiveShape <- new ShapeStr(angle = NewAngle, dimensions = GetShapeDimensions(RotatedMap), dropRate = ActiveShape.DropRate, map = RotatedMap, shape = ActiveShape.Shape, pitxy = ActiveShape.PitXY)
               else
                  ()
               DrawShape ActiveShape GameState.Pit PitBox false 
            | Keys.Left ->
               DrawShape ActiveShape GameState.Pit PitBox true
               let NewX = if CanMove ActiveShape.Map ActiveShape.PitXY (new Point(-1, 0)) GameState.Pit then ActiveShape.PitXY.X - 1 else ActiveShape.PitXY.X
               ActiveShape <- new ShapeStr(angle = ActiveShape.Angle, dimensions = ActiveShape.Dimensions, dropRate = ActiveShape.DropRate, map = ActiveShape.Map, shape = ActiveShape.Shape, pitxy = new Point(NewX, ActiveShape.PitXY.Y))
               DrawShape ActiveShape GameState.Pit PitBox false
            | Keys.Right ->
               DrawShape ActiveShape GameState.Pit PitBox true
               let NewX = if CanMove ActiveShape.Map ActiveShape.PitXY (new Point(+1, 0)) GameState.Pit then ActiveShape.PitXY.X + 1 else ActiveShape.PitXY.X
               ActiveShape <- new ShapeStr(angle = ActiveShape.Angle, dimensions = ActiveShape.Dimensions, dropRate = ActiveShape.DropRate, map = ActiveShape.Map, shape = ActiveShape.Shape, pitxy = new Point(NewX, ActiveShape.PitXY.Y))
               DrawShape ActiveShape GameState.Pit PitBox false
            | Keys.Space ->
               Dropper.Interval <- 1
            | _ ->
               ()
      
      //This procedure gives the command to redraw the pit when this window is redrawn.
      member private this.Form_Paint(sender:Object, e:PaintEventArgs) = 
         DrawPit PitBox GameState
         DisplayStatus GameState (this.CreateGraphics())
         
   //This procedure is executed when this program is started.
   [<STAThread>]
   do Application.Run(new InterfaceWindow())

