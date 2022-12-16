--
--         Simple Tetris Game Clone in ADA using SFML 
--
--    Raymond NGUYEN THANH                      24-10-2022  
-- 

--with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Discrete_Random;
--with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.strings.Unbounded; use Ada.strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;

with Sf.Audio.Sound, Sf.Audio.SoundBuffer, Sf.Audio.Music;

with Sf.Graphics.Color, Sf.Graphics.Font, Sf.Graphics.Rect,
     Sf.Graphics.Text, Sf.Graphics.VertexArray, Sf.Graphics.Vertex, Sf.Graphics.PrimitiveType, Sf.Graphics.RenderWindow;

with Sf.System.Time, Sf.System.Clock;

with Sf.Window.Event, Sf.Window.Keyboard;

use  Sf, Sf.Audio, Sf.Graphics, Sf.System, Sf.Window;

with tetris_const;
with tetris_shape; use tetris_shape;

procedure sfml_tetris is

    type GameMode_Type is (STAND_BY,PLAY,GAME_OVER,HIGH_SCORES);
    curGameMode : GameMode_Type := STAND_BY;

    --Pi : constant := Ada.Numerics.Pi;
    Win: sfRenderWindow_Ptr;
    Evt: Event.sfEvent;
    

    HTimer, VTimer, RTimer  : sfClock_Ptr;
    elapseH,elapseV,elapseR : Sf.System.Time.sfTime;
    fDrop           : Boolean := False;
    nbCompledLines  : Integer := 0;

    --type Vertex_Ref  is access Vertex.sfVertex;
    --procedure Free_Vertex is new Ada.Unchecked_Deallocation(Object=>Vertex.sfVertex,Name=>Vertex_Ref);
    --v1 : Vertex_Ref;
    --v1 := new Vertex.sfVertex'((10.0,10.0),Color.sfCyan,(0.0,0.0));
    --VertexArray.append(vertex_array1, Vertex.sfVertex(v1.all));
    --Free_Vertex(v1);
    
    type HightScore is record
        name    : Unbounded_String;
        score   : Integer; 
    end record;
    curScore        : Integer := 0;
    playerName      : Unbounded_String := To_Unbounded_String("");
    idHighScore     : Integer := 0;
    iHighScoreColor : Integer := 0;

    subtype Index_highScores is Integer range 1 .. 10;
    type highScores_t is array (Index_highScores) of HightScore;
    hightScores : highScores_t := (others => (To_Unbounded_String("XXXXX"),0));

    board : tetris_const.arrBoard := (others => 0);

    subtype discreteRange_t is Integer range 0..13;
    package RandomInt is new Ada.Numerics.Discrete_Random(discreteRange_t);
    use RandomInt;
    Gen : RandomInt.Generator;
    type tetrisBag_t is array (discreteRange_t) of Integer;
    tetrisBag : tetrisBag_t := (1,2,3,4,5,6,7,1,2,3,4,5,6,7);
    iTetrisBag : Integer := 14;

    veloH : sfInt32 := 0;

    horizontalMove : sfInt32 := 0;
    horizontalMoveStartColumn : Integer := 0;

    fFastDown   : Boolean := False;
    fPause      : Boolean := False;
    fEscapePlayMode : Boolean := False;

    package Character_Ordered_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps
        (Key_Type        => Integer,
        Element_Type    => Character);
    --use Character_Ordered_Maps;
    KeyMap : Character_Ordered_Maps.Map;

    use type Event.sfEventType;
    use type Keyboard.sfKeyCode;

    type isOutLimit_t is access function(tetro : in out Tetromino) return Boolean;
    isOutLimit : isOutLimit_t; 

    type ProcessEvent_Type is access procedure(win : sfRenderWindow_Ptr;evt : in Event.sfEvent);
    processEvent : ProcessEvent_Type; 

    GameFont            : sfFont_Ptr;
    textScore           : sfText_Ptr;
    successSoundBuffer  : sfSoundBuffer_Ptr;
    successSound        : sfSound_Ptr;
    tetrisMusic         : sfMusic_Ptr;

    curTetromino        : Tetromino;
    nextTetromino       : Tetromino;

    function TetrisRandomizer return Integer is
        ityp : Integer;
        iSrc : Integer;
    begin
        if iTetrisBag <= discreteRange_t'Last then
            ityp := tetrisBag(iTetrisBag);
            iTetrisBag := iTetrisBag + 1;
        else
            -- Shuttle Bag
            for i in 1..24 loop
                iSrc := Random(Gen);
                ityp := tetrisBag(iSrc);
                tetrisBag(iSrc) := tetrisBag(discreteRange_t'First);
                 tetrisBag(discreteRange_t'First) := ityp;
            end loop;

            for it of tetrisBag loop
                Put(it'Image);
                Put(" ");
            end loop;
            New_Line;

            ityp := tetrisBag(discreteRange_t'First);
            iTetrisBag := 1;
        end if;
        return ityp;
    end TetrisRandomizer;

    procedure drawBoard(render : sfRenderWindow_Ptr) is
        t     : Integer;
        vx,vy : Float; 
        vertex_arr : sfVertexArray_Ptr;
        v1 : Vertex.sfVertex;
    begin

        --
        vertex_arr := VertexArray.create;

        -- Draw Background
        v1.color := Color.fromRGB(10,10,100);
        v1.position.x := Float(tetris_const.LEFT);
        v1.position.y := Float(tetris_const.TOP);
        VertexArray.append(vertex_arr, v1);
        v1.position.x := Float(tetris_const.LEFT+ tetris_const.NB_COLUMNS *tetris_const.CELL_SIZE);
        VertexArray.append(vertex_arr, v1);
        v1.position.y := Float(tetris_const.TOP + tetris_const.NB_ROWS*tetris_const.CELL_SIZE);
        VertexArray.append(vertex_arr, v1);
        v1.position.x := Float(tetris_const.LEFT);
        VertexArray.append(vertex_arr, v1);

        for r in 0..(tetris_const.NB_ROWS-1) loop
            for c in 0..(tetris_const.NB_COLUMNS-1) loop

                t := board(r*tetris_const.NB_COLUMNS+c);
                if (t /= 0) then
                    v1.color := tetris_const.tetrisColors(t);
                    vx := Float(tetris_const.LEFT + c*tetris_const.CELL_SIZE) + 2.0;
                    vy := Float(tetris_const.TOP + r*tetris_const.CELL_SIZE) + 2.0;
                    v1.position.x := vx;
                    v1.position.y := vy;
                    VertexArray.append(vertex_arr, v1);
                    v1.position.x := vx + Float(tetris_const.CELL_SIZE) - 2.0;
                    VertexArray.append(vertex_arr, v1);
                    v1.position.y := vy + Float(tetris_const.CELL_SIZE) - 2.0;
                    VertexArray.append(vertex_arr, v1);
                    v1.position.x := vx;
                    VertexArray.append(vertex_arr, v1);
                end if;

            end loop;            
        end loop;

        VertexArray.setPrimitiveType(vertexArray =>vertex_arr, 
                            primitiveType => PrimitiveType.sfQuads );

        RenderWindow.drawVertexArray(render,vertex_arr);

        --
        VertexArray.destroy(vertex_arr);

    end drawBoard;

    procedure newTetromino is 
    begin
        curTetromino.ityp := nextTetromino.ityp;
        curTetromino.Init(nextTetromino.ityp,6*tetris_const.CELL_SIZE,0);
        curTetromino.y := -(curTetromino.maxY*tetris_const.CELL_SIZE);
        nextTetromino.Init(TetrisRandomizer,(tetris_const.NB_COLUMNS+3)*tetris_const.CELL_SIZE,10*tetris_const.CELL_SIZE);
    end newTetromino;

    function computeCompletedLines return Integer is
        nbLines : Integer := 0;
        fCompleted : Boolean;
    begin
        for r in 0..(tetris_const.NB_ROWS-1) loop
            fCompleted := True;
            for c in 0..(tetris_const.NB_COLUMNS-1) loop
                if board(r*tetris_const.NB_COLUMNS+c)=0 then
                    fCompleted := False;
                    exit;
                end if;
            end loop;
            if fCompleted then
                nbLines := nbLines + 1;
            end if;
        end loop;
        return nbLines;
    end;

    function computeScore(nbLines : Integer) return Integer is
    begin
        return (case nbLines is
            when 0 =>
                0,
            when 1 =>
                40,
            when 2 =>
                100,
            when 3 =>
                300,
            when 4 =>
                1200,
            when others =>
                2000);
    end;

    procedure FreezeCurTetromino is
        ix,iy : Integer;
        x,y : Integer;
    begin
        if curTetromino.ityp /=0 then
            ix := Integer((curTetromino.x + 1)/tetris_const.CELL_SIZE);
            iy := Integer((curTetromino.y + 1)/tetris_const.CELL_SIZE);
            for p of curTetromino.v loop
                x := ix + Integer(p.x);
                y := iy + Integer(p.y);
                if x>=0 and x<tetris_const.NB_COLUMNS and y>=0 and y<tetris_const.NB_ROWS then
                    board(x + y*tetris_const.NB_COLUMNS) := curTetromino.ityp;
                end if;
            end loop;
        end if;
        --
        nbCompledLines := computeCompletedLines;
        if nbCompledLines>0 then
            curScore := curScore + computeScore(nbCompledLines);
            Text.setString (textScore, "SCORE : " & Tail(curScore'Image(curScore'Img'First + 1 .. curScore'Img'Last),6,'0'));
        end if;

    end FreezeCurTetromino;

    function isHighScore(score : Integer) return Integer is
    begin
        if score>0 then
            for i in Index_highScores loop
                if score>=hightScores(i).score then
                    return i;
                end if;
            end loop;
        end if;
        return 0;
    end isHighScore;

    procedure insertHighScore( id:Integer; name:Unbounded_String; score:Integer) is
        i : Integer;
    begin
        -- Shift down highScores array
        i := Index_highScores'Last;
        while i>id loop
            --
            hightScores(i).name := hightScores(i-1).name;
            hightScores(i).score := hightScores(i-1).score;
            --
            i := i - 1;
        end loop;
        hightScores(id) := (name,score);

    end insertHighScore;

    procedure saveHighScores(fileName : String) is
        fich : File_Type;
    begin
        Create(fich,Out_File,fileName);
        for i in Index_highScores loop
            Put_Line(fich, To_String(hightScores(i).name) & ";" & Ada.Strings.Fixed.Trim(hightScores(i).score'Image,Both) & ";");
        end loop;
        Close(fich);

    end saveHighScores;

    function parseWord(ic : in out Integer; strLine : in Unbounded_String; sep : Character) return Unbounded_String is
        l : Integer;
        c : Character;
        strWord : Unbounded_String;
    begin
        l := Ada.strings.Unbounded.Length(strLine);
        while ic<=l loop
            c := Ada.strings.Unbounded.Element(strLine,ic);
            if c=sep then
                ic := ic + 1;
                return strWord;
            else
                Ada.Strings.Unbounded.Append(strWord,c);
            end if;
            ic := ic + 1;
        end loop;
        return strWord;
    end parseWord;

    procedure loadHighScores(fileName : String) is
        i,ic      : Integer; 
        fich    : File_Type;
        strLine : Unbounded_String;
        strWord : Unbounded_String;
    begin
        begin
            i := Index_highScores'First;
            Open(fich,In_File,fileName);
            while not End_Of_File(fich) and (i<=Index_highScores'Last) loop
                strLine := To_Unbounded_String(Get_Line(fich));
                ic := 1;
                strWord := parseWord(ic, strLine , ';');
                hightScores(i).name := strWord;
                strWord := parseWord(ic, strLine , ';');
                Ada.Strings.Unbounded.Trim(strWord, Both);
                hightScores(i).score := Integer'Value(To_String(strWord));
                i := i + 1;
            end loop;
            Close(fich);
        exception
            when Name_Error => null;
        end;

    end loadHighScores;

    procedure eraseFirstCompletedLine is
        fCompleted : Boolean;
    begin
        for r in 0..(tetris_const.NB_ROWS-1) loop
            fCompleted := True;
            for c in 0..(tetris_const.NB_COLUMNS-1) loop
                if board(r*tetris_const.NB_COLUMNS+c)=0 then
                    fCompleted := False;
                    exit;
                end if;
            end loop;
            if fCompleted then
                -- Shift down Board content
                for r1 in reverse 1..r loop
                    for c1 in 0..(tetris_const.NB_COLUMNS-1) loop
                        board(r1*tetris_const.NB_COLUMNS+c1) := board((r1-1)*tetris_const.NB_COLUMNS+c1);
                    end loop;
                end loop;
                return;
            end if;
        end loop;

    end eraseFirstCompletedLine;

    procedure processPlayEvent (win : sfRenderWindow_Ptr;evt : in Event.sfEvent) is
        backupX : sfInt32;
    begin

        case Evt.eventType is
            when Event.sfEvtClosed =>
                RenderWindow.Close (Win);
            when Event.sfEvtKeyPressed =>

                case Evt.key.code is
                    when  Keyboard.sfKeyEscape =>
                        --
                        fEscapePlayMode := True;
                    when Keyboard.sfKeyPause =>
                        if fPause then
                            fPause := False;
                            Music.play(tetrisMusic);                            
                        else
                            fPause := True;
                            Music.pause(tetrisMusic);
                        end if;
                    when  Keyboard.sfKeyUp =>
                        --
                        curTetromino.rotateLeft;

                        if curTetromino.hitGround(board) then
                            -- Undo Rotate
                            curTetromino.rotateRight;
                        elsif curTetromino.isOutRightLimit then
                            backupX := curTetromino.x;
                            -- Move curTetromino inside board
                            loop
                                curTetromino.x := curTetromino.x - 1;
                                if not curTetromino.isOutRightLimit then
                                    exit;
                                end if;
                            end loop;
                            if curTetromino.hitGround( board) then
                                -- Undo Move
                                curTetromino.x := backupX;
                                -- Undo Rotate
                                curTetromino.rotateRight;
                            end if;
                        elsif curTetromino.isOutLeftLimit then
                            backupX := curTetromino.x;
                            -- Move curTetromino inside board
                            loop
                                curTetromino.x := curTetromino.x + 1;
                                if not curTetromino.isOutLeftLimit
                                 then
                                    exit;
                                end if;
                            end loop;
                            if curTetromino.hitGround( board) then
                                -- Undo Move
                                curTetromino.x := backupX;
                                -- Undo Rotate
                                curTetromino.rotateRight;
                            end if;
                        end if;


                    when  Keyboard.sfKeyDown =>
                        --
                        fFastDown := True;
                    when  Keyboard.sfKeyLeft =>
                        --
                        veloH := -1;
                        isOutLimit := tetris_shape.isOutLeftLimit'Access;
                    when Keyboard.sfKeyRight =>
                        --
                        veloH := 1;
                        isOutLimit := tetris_shape.isOutRightLimit'Access;
                    when Keyboard.sfKeySpace =>
                        --
                        fDrop := True;
                    when others => null;
                end case;

            when Event.sfEvtKeyReleased =>
                case Evt.key.code is
                    when  Keyboard.sfKeyLeft =>
                        --
                        veloH := 0;
                        --isOutLimit := TetrisShape.isAlwaysOutLimit'Access;
                    when Keyboard.sfKeyRight =>
                        --
                        veloH := 0;
                        --isOutLimit := TetrisShape.isAlwaysOutLimit'Access;
                   when  Keyboard.sfKeyDown =>
                        --
                        fFastDown := False;
                    when others => null;
                end case;

            when others => null;
        end case;    
    end processPlayEvent;

    procedure processStandByEvent (win : sfRenderWindow_Ptr;evt : in Event.sfEvent) is
    begin
        case Evt.eventType is
            when Event.sfEvtClosed =>
                RenderWindow.Close (Win);
            when Event.sfEvtKeyPressed =>
                
                case Evt.key.code is
                    when  Keyboard.sfKeyEscape =>
                        RenderWindow.Close (Win);
                    when Keyboard.sfKeySpace =>
                        --
                        curGameMode := PLAY;
                        processEvent := processPlayEvent'Access;
                        newTetromino;
                    when others => null;
                end case;

            when others => null;
        end case;    
    end processStandByEvent;

    procedure processHighScoresEvent (win : sfRenderWindow_Ptr;evt : in Event.sfEvent) is
        ik : Integer;
        newName : Unbounded_String;
        l : Natural;
    begin
        case Evt.eventType is
            when Event.sfEvtClosed =>
                RenderWindow.Close (Win);
            when Event.sfEvtKeyPressed =>
                ik := Integer(Evt.key.code);
                case ik is
                    when  Integer(Keyboard.sfKeyEscape) =>
                        curGameMode := STAND_BY;
                        processEvent := processStandByEvent'Access;
                    when  Integer(Keyboard.sfKeyReturn) =>
                        saveHighScores("HighScores.txt");
                        curGameMode := STAND_BY;
                        processEvent := processStandByEvent'Access;
                    when 59 | Integer(Keyboard.sfKeyDelete) =>
                        -- BackSpace
                        l := Ada.Strings.Unbounded.Length(playerName);
                        if l>0 then
                            newName := Ada.Strings.Unbounded.Delete(playerName,l,l);
                            hightScores(idHighScore).name := newName;
                            playerName := newName;
                        end if;
                        --
                    when others => 
                        if KeyMap.Contains(ik) then
                            if  Ada.Strings.Unbounded.Length(playerName) <= 8 then
                                Ada.Strings.Unbounded.Append(playerName,KeyMap(ik));
                                hightScores(idHighScore).name := playerName;
                            end if;
                        end if; 

                end case;

            when others => null;
        end case;    
    end processHighScoresEvent;

    procedure processGameOverEvent (win : sfRenderWindow_Ptr;evt : in Event.sfEvent) is
    begin
        case Evt.eventType is
            when Event.sfEvtClosed =>
                RenderWindow.Close (Win);
            when Event.sfEvtKeyPressed =>
                
                case Evt.key.code is
                    when  Keyboard.sfKeyEscape =>
                        RenderWindow.Close (Win);
                    when Keyboard.sfKeySpace =>
                        --
                        curGameMode := STAND_BY;
                        processEvent := processStandByEvent'Access;
                    when others => null;
                end case;

            when others => null;
        end case;    
    end processGameOverEvent;

    function isGameOver return Boolean is
    begin
        for c in 0..(tetris_const.NB_COLUMNS-1) loop
            if board(c) /=0 then
                return True;
            end if;
        end loop;
        return False;
    end isGameOver;

    procedure initGame is 
    begin
        curTetromino.ityp := 0;
        nextTetromino.Init(TetrisRandomizer,(tetris_const.NB_COLUMNS+3)*tetris_const.CELL_SIZE,10*tetris_const.CELL_SIZE);
        board := (others => 0);
        curScore := 0;
        Text.setString (textScore, "SCORE : " & Tail(curScore'Image(curScore'Img'First + 1 .. curScore'Img'Last),6,'0'));
    end initGame;

    procedure drawGameOverMode(render : sfRenderWindow_Ptr) is
        textLine  : sfText_Ptr;
        txtX,txtY : Float;
        rectText  : Sf.Graphics.Rect.sfFloatRect; 
    begin

        textLine := Text.Create;

        Text.setFont (textLine, GameFont);
        Text.setCharacterSize (textLine, 18);
        Text.setStyle(textLine, Sf.Graphics.Text.sfTextBold or Sf.Graphics.Text.sfTextRegular);
        Text.setColor (textLine, Color.fromRGB(255,255,0));

        Text.setString (textLine, "GAME OVER");
        rectText := Text.getLocalBounds(textLine);
        txtX := Float(tetris_const.LEFT + (tetris_const.NB_COLUMNS/2)*tetris_const.CELL_SIZE) - rectText.width/2.0;
        txtY := Float(tetris_const.TOP + 3*tetris_const.CELL_SIZE);
        Text.setPosition (textLine, (X => txtX, Y => TxtY));
        RenderWindow.DrawText (render, textLine);

        txtY := txtY + rectText.height*3.0;
        Text.setString (textLine, "Press SPACE to Continue");
        rectText := Text.getLocalBounds(textLine);
        txtX := Float(tetris_const.LEFT + (tetris_const.NB_COLUMNS/2)*tetris_const.CELL_SIZE) - rectText.width/2.0;
        Text.setPosition (textLine, (X => txtX, Y => TxtY));
        RenderWindow.DrawText (render, textLine);

        --
        Text.destroy(textLine);
    end drawGameOverMode;

    procedure drawStandByMode(render : sfRenderWindow_Ptr) is
        textLine  : sfText_Ptr;
        txtX,txtY : Float;
        rectText  : Sf.Graphics.Rect.sfFloatRect; 
    begin

        textLine := Text.Create;

        Text.setFont (textLine, GameFont);
        Text.setCharacterSize (textLine, 18);
        Text.setStyle(textLine, Sf.Graphics.Text.sfTextBold or Sf.Graphics.Text.sfTextRegular);
        Text.setColor (textLine, Color.fromRGB(255,255,0));

        Text.setString (textLine, "ADA Tetris in SFML");
        rectText := Text.getLocalBounds(textLine);
        txtX := Float(tetris_const.LEFT + (tetris_const.NB_COLUMNS/2)*tetris_const.CELL_SIZE) - rectText.width/2.0;
        txtY := Float(tetris_const.TOP + 3*tetris_const.CELL_SIZE);
        Text.setPosition (textLine, (X => txtX, Y => TxtY));
        RenderWindow.DrawText (render, textLine);

        txtY := txtY + rectText.height*3.5;
        Text.setString (textLine, "R. NGUYEN THANH");
        rectText := Text.getLocalBounds(textLine);
        txtX := Float(tetris_const.LEFT + (tetris_const.NB_COLUMNS/2)*tetris_const.CELL_SIZE) - rectText.width/2.0;
        Text.setPosition (textLine, (X => txtX, Y => TxtY));
        RenderWindow.DrawText (render, textLine);

        txtY := txtY + rectText.height*3.0;
        Text.setString (textLine, "Press SPACE to Play");
        rectText := Text.getLocalBounds(textLine);
        txtX := Float(tetris_const.LEFT + (tetris_const.NB_COLUMNS/2)*tetris_const.CELL_SIZE) - rectText.width/2.0;
        Text.setPosition (textLine, (X => txtX, Y => TxtY));
        RenderWindow.DrawText (render, textLine);

        --
        Text.destroy(textLine);
    end drawStandByMode;

    procedure drawHighScoresMode(render : sfRenderWindow_Ptr) is
        textLine  : sfText_Ptr;
        txtX,txtY : Float;
        rectText  : Sf.Graphics.Rect.sfFloatRect; 
    begin

        textLine := Text.Create;

        Text.setFont (textLine, GameFont);
        Text.setCharacterSize (textLine, 18);
        Text.setStyle(textLine, Sf.Graphics.Text.sfTextBold or Sf.Graphics.Text.sfTextRegular);
        Text.setColor (textLine, Color.fromRGB(255,255,0));

        Text.setString (textLine, "HIGH SCORES");
        rectText := Text.getLocalBounds(textLine);
        txtX := Float(tetris_const.LEFT + (tetris_const.NB_COLUMNS/2)*tetris_const.CELL_SIZE) - rectText.width/2.0;
        txtY := Float(tetris_const.TOP + 2*tetris_const.CELL_SIZE);
        Text.setPosition (textLine, (X => txtX, Y => TxtY));
        RenderWindow.DrawText (render, textLine);

        txtY := txtY + Float(2*tetris_const.CELL_SIZE);

        for i in 1..10 loop

            -- Flashing current High Score
            if i=idHighScore then
                if (iHighScoreColor mod 2)/=0 then
                    Text.setColor (textLine, Color.fromRGB(155,155,0));                
                else
                    Text.setColor (textLine, Color.fromRGB(255,255,0));
                end if;
            else
                Text.setColor (textLine, Color.fromRGB(255,255,0));
            end if;

            txtX := Float(tetris_const.LEFT + tetris_const.CELL_SIZE);
            Text.setString (textLine, To_String(hightScores(i).Name));
            Text.setPosition (textLine, (X => txtX, Y => TxtY));
            RenderWindow.DrawText (render, textLine);

            txtX := Float(tetris_const.LEFT + (tetris_const.NB_COLUMNS/2+2)*tetris_const.CELL_SIZE);
            Text.setString (textLine, Tail(hightScores(i).score'Image(hightScores(i).score'Img'First + 1 .. hightScores(i).score'Img'Last),6,'0'));
            Text.setPosition (textLine, (X => txtX, Y => TxtY));
            RenderWindow.DrawText (render, textLine);

            --
            txtY := txtY + rectText.height + 10.0;

        end loop;

        --
        Text.destroy(textLine);
    end drawHighScoresMode;


begin

    Reset (Gen);

    -- Fill KeyMap for Player Name Input in HighScores Mode
    KeyMap.Include(Integer(Keyboard.sfKeyA),'A');
    KeyMap.Include(Integer(Keyboard.sfKeyB),'B');
    KeyMap.Include(Integer(Keyboard.sfKeyC),'C');
    KeyMap.Include(Integer(Keyboard.sfKeyD),'D');
    KeyMap.Include(Integer(Keyboard.sfKeyE),'E');
    KeyMap.Include(Integer(Keyboard.sfKeyF),'F');
    KeyMap.Include(Integer(Keyboard.sfKeyG),'G');
    KeyMap.Include(Integer(Keyboard.sfKeyH),'H');
    KeyMap.Include(Integer(Keyboard.sfKeyI),'I');
    KeyMap.Include(Integer(Keyboard.sfKeyJ),'J');
    KeyMap.Include(Integer(Keyboard.sfKeyK),'K');
    KeyMap.Include(Integer(Keyboard.sfKeyL),'L');
    KeyMap.Include(Integer(Keyboard.sfKeyM),'M');
    KeyMap.Include(Integer(Keyboard.sfKeyN),'N');
    KeyMap.Include(Integer(Keyboard.sfKeyO),'O');
    KeyMap.Include(Integer(Keyboard.sfKeyP),'P');
    KeyMap.Include(Integer(Keyboard.sfKeyQ),'Q');
    KeyMap.Include(Integer(Keyboard.sfKeyR),'R');
    KeyMap.Include(Integer(Keyboard.sfKeyS),'S');
    KeyMap.Include(Integer(Keyboard.sfKeyT),'T');
    KeyMap.Include(Integer(Keyboard.sfKeyU),'U');
    KeyMap.Include(Integer(Keyboard.sfKeyV),'V');
    KeyMap.Include(Integer(Keyboard.sfKeyW),'W');
    KeyMap.Include(Integer(Keyboard.sfKeyX),'X');
    KeyMap.Include(Integer(Keyboard.sfKeyY),'Y');
    KeyMap.Include(Integer(Keyboard.sfKeyZ),'Z');
    KeyMap.Include(Integer(Keyboard.sfKeyNum0),'0');
    KeyMap.Include(Integer(Keyboard.sfKeyNum1),'1');
    KeyMap.Include(Integer(Keyboard.sfKeyNum2),'2');
    KeyMap.Include(Integer(Keyboard.sfKeyNum3),'3');
    KeyMap.Include(Integer(Keyboard.sfKeyNum4),'4');
    KeyMap.Include(Integer(Keyboard.sfKeyNum5),'5');
    KeyMap.Include(Integer(Keyboard.sfKeyNum6),'6');
    KeyMap.Include(Integer(Keyboard.sfKeyNum7),'7');
    KeyMap.Include(Integer(Keyboard.sfKeyNum8),'8');
    KeyMap.Include(Integer(Keyboard.sfKeyNum9),'9');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad0),'0');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad1),'1');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad2),'2');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad3),'3');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad4),'4');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad5),'5');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad6),'6');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad7),'7');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad8),'8');
    KeyMap.Include(Integer(Keyboard.sfKeyNumpad9),'9');


    Win := RenderWindow.Create(mode => (tetris_const.WIN_WIDTH, tetris_const.WIN_HEIGHT, 32), title => "Ada SFML Tetris");
    RenderWindow.setKeyRepeatEnabled(Win, enabled => false);

    RenderWindow.SetVerticalSyncEnabled(Win, sfTrue);

    --Put_Line("CELL_SIZE = " & Integer'Image(Game.CELL_SIZE));

    loadHighScores("HighScores.txt");

    successSoundBuffer := SoundBuffer.CreateFromFile("109662__grunz__success.wav");
    successSound := Sound.Create;
    Sound.SetBuffer(successSound, successSoundBuffer);
    Sound.setVolume(successSound, 10.0);

    tetrisMusic := Music.createFromFile("Tetris.wav");
    Music.setVolume(tetrisMusic, 10.0);
    Music.setLoop(tetrisMusic,True);
    Music.play(tetrisMusic);

    GameFont := Font.CreateFromFile("sansation.ttf");
    textScore := Text.Create;
    Text.setFont (textScore, GameFont);
    Text.setCharacterSize (textScore, 18);
    Text.setStyle(textScore, Sf.Graphics.Text.sfTextBold or Sf.Graphics.Text.sfTextItalic);
    Text.setPosition (textScore, (X => Float(tetris_const.LEFT), Y => Float(tetris_const.TOP+tetris_const.NB_ROWS*tetris_const.CELL_SIZE+tetris_const.CELL_SIZE/2)));
    Text.setColor (textScore, Color.fromRGB(255,255,0));

    
    initGame;
    curGameMode := STAND_BY;
    processEvent:= processStandByEvent'Access;
    isOutLimit  := isOutLeftLimit'Access;

    HTimer := Clock.Create;
    VTimer := Clock.Create;
    RTimer := Clock.Create;

    while RenderWindow.isOpen (Win) loop

        while RenderWindow.PollEvent(Win, Evt) loop

            processEvent(Win,Evt);

            if fEscapePlayMode then
                fEscapePlayMode := False;
                -- Check High Score
                idHighScore := isHighScore(curScore);
                if idHighScore>0 then
                    insertHighScore(idHighScore,playerName,curScore);
                    curGameMode := HIGH_SCORES;
                    processEvent := processHighScoresEvent'Access;
                    initGame;
                else
                    curGameMode := STAND_BY;
                    processEvent := processStandByEvent'Access;
                    initGame;
                end if;
            end if;
        end loop;

        -- Update game state
        if curGameMode=PLAY and (not fPause) then

            if nbCompledLines>0 then
                --
                elapseV := CLock.getElapsedTime(VTimer);
                if elapseV.microseconds>250000 then
                    elapseV := Clock.restart(VTimer);
                    nbCompledLines := nbCompledLines - 1;
                    eraseFirstCompletedLine;
                    Sound.play(successSound);
                end if;

            elsif horizontalMove/=0 then
                --
                elapseH := CLock.getElapsedTime(HTimer);
                if elapseH.microseconds>20000 then
                    declare
                        backupX : sfInt32;
                    begin
                        elapseH := Clock.restart(HTimer);

                        for i in 1..4 loop
                            backupX := curTetromino.x;
                            curTetromino.x := curTetromino.x + horizontalMove;

                            if isOutLimit(curTetromino) then
                                curTetromino.x := backupX;
                                horizontalMove := 0;
                                exit;
                            else
                                if curTetromino.hitGround( board) then
                                    curTetromino.x := backupX;
                                    horizontalMove := 0;
                                    exit;                                
                                end if;
                            end if;

                            if horizontalMove /=0 then
                                if horizontalMoveStartColumn/=curTetromino.Column then
                                    curTetromino.x := backupX;
                                    horizontalMove := 0;
                                    elapseH := Clock.restart(HTimer);
                                    exit;
                                end if; 

                            end if;

                        end loop;

                    end;
                end if;

            elsif fDrop then
                --
                elapseV := CLock.getElapsedTime(VTimer);
                if elapseV.microseconds>10000 then
                    elapseV := Clock.restart(VTimer);
                    for i in 1..6 loop
                        -- Move Down
                        curTetromino.y := curTetromino.y + 1;
                        if curTetromino.hitGround( board) then
                            curTetromino.y := curTetromino.y - 1;
                            FreezeCurTetromino;
                            NewTetromino;
                            fDrop := False;                    
                        elsif curTetromino.isOutBottomLimit then
                            curTetromino.y := curTetromino.y - 1;
                            FreezeCurTetromino;
                            NewTetromino;
                            fDrop := False;                                        
                        end if;
                        if fDrop then
                            if veloH /=0 then
                                elapseH := CLock.getElapsedTime(HTimer);
                                if elapseH.microseconds>20000 then
                                    declare
                                        backupX : sfInt32;
                                    begin
                                        backupX := curTetromino.x;
                                        curTetromino.x := curTetromino.x + veloH;

                                        if isOutLimit(curTetromino) then
                                            curTetromino.x := backupX;
                                        else
                                            elapseH := Clock.restart(HTimer);
                                            horizontalMove := veloH;
                                            horizontalMoveStartColumn := curTetromino.Column;
                                            exit;
                                        end if;

                                    end;
                                end if;
                            end if;
                        end if;
                    end loop;
                end if;
            else
                --
                declare
                    limitElapse : sfInt64 := 31000;
                    fMove : Boolean;
                begin
                    if fFastDown then
                        limitElapse := 15000;
                    end if;
                    elapseV := CLock.getElapsedTime(VTimer);
                    if elapseV.microseconds>limitElapse then

                        --Put_Line(elapseV.microseconds'Image);
                        elapseV := Clock.restart(VTimer);

                        for i in 1..3 loop

                            curTetromino.y := curTetromino.y + 1;
                            fMove := True;

                            if curTetromino.hitGround(board) then
                                curTetromino.y := curTetromino.y - 1;
                                FreezeCurTetromino;
                                NewTetromino;
                                fMove := False;
                            elsif curTetromino.isOutBottomLimit then
                                curTetromino.y := curTetromino.y - 1;
                                FreezeCurTetromino;
                                NewTetromino;
                                fMove := False;                        
                            end if;

                            if fMove then
                                if veloH /= 0 then
                                    elapseH := CLock.getElapsedTime(HTimer);
                                    if elapseH.microseconds>12000 then
                                        declare
                                            backupX : sfInt32;
                                        begin
                                            backupX := curTetromino.x;
                                            curTetromino.x := curTetromino.x + veloH;

                                            if isOutLimit(curTetromino) then
                                                curTetromino.x := backupX;
                                            else
                                                if curTetromino.hitGround(board) then
                                                    curTetromino.x := backupX;                                             
                                                else
                                                    elapseH := Clock.restart(HTimer);
                                                    horizontalMove := veloH;
                                                    horizontalMoveStartColumn := curTetromino.Column;
                                                    exit;
                                                end if;
                                            end if;

                                        end;
                                    end if;
                                end if;
                            end if;

                        end loop;
                    
                    end if;
                end;
            
            end if;

            -- Check Game Over
            if isGameOver then
                -- Check High Score
                idHighScore := isHighScore(curScore);
                if idHighScore>0 then
                    insertHighScore(idHighScore,playerName,curScore);
                    curGameMode := HIGH_SCORES;
                    processEvent := processHighScoresEvent'Access;
                    initGame;
                else
                    curGameMode := GAME_OVER;
                    processEvent := processGameOverEvent'Access;
                    initGame;
                end if;
            end if;
        
        end if;

        elapseR := CLock.getElapsedTime(RTimer);
        if elapseR.microseconds>1000000 then
            elapseR := Clock.restart(RTimer);
            nextTetromino.rotateRight;
        end if;

        RenderWindow.Clear(Win, Color.FromRGB(32, 32, 200));

        drawBoard(Win);

        case curGameMode is
            when STAND_BY =>
                drawStandByMode(Win);
            when HIGH_SCORES => 
                drawHighScoresMode(Win);
                elapseV := CLock.getElapsedTime(VTimer);
                if elapseV.microseconds>500000 then
                    elapseV := Clock.restart(VTimer);
                    iHighScoreColor := iHighScoreColor + 1;
                end if;
            when GAME_OVER => 
                drawGameOverMode(Win);
            when others => null;
        end case;

        if curTetromino.ityp /=0 then
            curTetromino.draw(Win);
        end if;

        nextTetromino.draw(Win);
        
        -- Draw Current Score
        RenderWindow.DrawText (Win, textScore);

        --
        RenderWindow.Display (Win);

    end loop;

    -- Free audio memory
    SoundBuffer.destroy(successSoundBuffer);
    Sound.destroy(successSound);

    Music.destroy(tetrisMusic);

    --
    RenderWindow.destroy(Win);


end sfml_tetris;