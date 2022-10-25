--
--
--
--
--
with Sf; use Sf;
with Sf.Graphics; use Sf.Graphics;
with Sf.Graphics.VertexArray, Sf.Graphics.Vertex, Sf.Graphics.PrimitiveType, Sf.Graphics.RenderWindow;

with Game; use Game;

package body TetrisShape is

    procedure Init(Self : in out Tetromino; typ : Integer; x : sfInt32; y : sfInt32) is
    begin

        Self.ityp := typ;
        Self.x := x;
        Self.y := y;
        case typ is
            when 0 =>
                Self.v := ((0,0),(0,0),(0,0),(0,0));
            when 1 =>
                Self.v := ((0,-1),(0,0),(-1,0),(-1,1));
            when 2 =>
                Self.v := ((0,-1),(0,0),(1,0),(1,1));
            when 3 =>
                Self.v := ((0,-1),(0,0),(0,1),(0,2));
            when 4 =>
                Self.v := ((-1,0),(0,0),(1,0),(0,1));
            when 5 =>
                Self.v := ((0,0),(1,0),(0,1),(1,1));
            when 6 =>
                Self.v := ((-1,-1),(0,-1),(0,0),(0,1));
            when 7 =>
                Self.v := ((1,-1),(0,-1),(0,0),(0,1));
            when others =>
                Self.v := ((0,0),(0,0),(0,0),(0,0));
        end case;

    end Init;

    procedure draw(Self : in out Tetromino;render : sfRenderWindow_Ptr) is
        ox,oy : Float;
        vx,vy : Float; 
        vertex_arr : sfVertexArray_Ptr;
        v1 : Vertex.sfVertex;

    begin
        --
        vertex_arr := VertexArray.create;

        ox := Float(LEFT + Self.x);
        oy := Float(TOP + Self.y);

        v1.color := tetrisColors(Integer(Self.ityp));
        for p of Self.v loop
            vx := ox + Float(p.x*CELL_SIZE) + 2.0;
            vy := oy + Float(p.y*CELL_SIZE) + 2.0;
            v1.position.x := vx;
            v1.position.y := vy;
            VertexArray.append(vertex_arr, v1);
            v1.position.x := vx + Float(CELL_SIZE) - 2.0;
            VertexArray.append(vertex_arr, v1);
            v1.position.y := vy + Float(CELL_SIZE) - 2.0;
            VertexArray.append(vertex_arr, v1);
            v1.position.x := vx;
            VertexArray.append(vertex_arr, v1);

        end loop;

        VertexArray.setPrimitiveType(vertexArray =>vertex_arr, 
                            primitiveType => PrimitiveType.sfQuads );

        RenderWindow.drawVertexArray(render,vertex_arr);

        --
        VertexArray.destroy(vertex_arr);

    end draw;

    procedure rotateLeft(Self : in out Tetromino) is
        x,y : sfInt32;
    begin
        if Self.ityp/= 5 then
            for i in Index_arrV loop
                x := Self.v(i).y;
                y := -Self.v(i).x;
                Self.v(i).x := x;
                Self.v(i).y := y;

            end loop;
        end if;
    end rotateLeft;

    procedure rotateRight(Self : in out Tetromino) is
        x,y : sfInt32;
    begin
        if Self.ityp/= 5 then
            for i in Index_arrV loop
                x := -Self.v(i).y;
                y := Self.v(i).x;
                Self.v(i).x := x;
                Self.v(i).y := y;

            end loop;
        end if;
    end rotateRight;

    function minX(Self : in out Tetromino) return sfInt32 is
        x       : sfInt32;
        mini    : sfInt32;
    begin
        mini := Self.v(Index_arrV'First).x;
        for i in (Index_arrV'First+1)..Index_arrV'Last loop
        x := Self.v(i).x;
        if (x<mini) then
            mini := x;
        end if;
        end loop;
        return mini;
    end minX;

    function maxX(Self : in out Tetromino) return sfInt32 is
        x       : sfInt32;
        maxi    : sfInt32;
    begin
        maxi := Self.v(Index_arrV'First).x;
        for i in (Index_arrV'First+1)..Index_arrV'Last loop
        x := Self.v(i).x;
        if (x>maxi) then
            maxi := x;
        end if;
        end loop;
        return maxi;
    end maxX;

    function maxY(Self : in out Tetromino) return sfInt32 is
        y       : sfInt32;
        maxi    : sfInt32;
    begin
        maxi := Self.v(Index_arrV'First).y;
        for i in (Index_arrV'First+1)..Index_arrV'Last loop
        y := Self.v(i).y;
        if (y>maxi) then
            maxi := y;
        end if;
        end loop;
        return maxi;
    end maxY;

    function Column(Self : in Tetromino) return Integer is
    begin
        return Integer(Self.x/CELL_SIZE);
    end Column;

    function isOutLeftLimit( Self : in out Tetromino) return Boolean is
        l : sfInt32;
    begin
        l := Self.minX*Game.CELL_SIZE + Self.x;
        return l < 0;
    end isOutLeftLimit;

    function isOutRightLimit(Self : in out Tetromino) return Boolean is
        r : sfInt32;
    begin
        r := Self.maxX*Game.CELL_SIZE + Game.CELL_SIZE + Self.x;
        return r > (Game.NB_COLUMNS*Game.CELL_SIZE);
    end isOutRightLimit;

    function isOutBottomLimit(tetro : in out Tetromino) return Boolean is
        b : sfInt32;
    begin
        b := tetro.maxY*Game.CELL_SIZE + Game.CELL_SIZE + tetro.y;
        return b > (Game.NB_ROWS*Game.CELL_SIZE);
    end isOutBottomLimit;

    function isAlwaysOutLimit(tetro : in out Tetromino) return Boolean is
    begin
        return True;
    end isAlwaysOutLimit;

    function hitGround(tetro : in out Tetromino; board : in Game.arrBoard) return Boolean is
        ix,iy     : Integer;
        sx,sy     : Integer;

        function hit(x : Integer; y : Integer) return Boolean is
            ix,iy : Integer;
        begin
            ix := x/Game.CELL_SIZE;
            iy := y/Game.CELL_SIZE;
            if (ix>=0) and (ix<Game.NB_COLUMNS) and (iy>=0) and (iy<Game.NB_ROWS) then
                if (board(iy*Game.NB_COLUMNS+ix)/=0) then 
                    return True;
                end if;
            end if;
            return False;
        end hit;
        pragma Inline (hit);

    begin

        for p of tetro.v loop

            sx := Integer(p.x*Game.CELL_SIZE + tetro.x + 1);
            sy := Integer(p.y*Game.CELL_SIZE + tetro.y + 1);
            if hit(sx, sy) then
                return True;
            end if;

            sx := Integer(p.x*Game.CELL_SIZE + Game.CELL_SIZE -1 + tetro.x);
            sy := Integer(p.y*Game.CELL_SIZE + tetro.y + 1);
            if hit(sx, sy) then
                return True;
            end if;

            sx := Integer(p.x*Game.CELL_SIZE + Game.CELL_SIZE - 1 + tetro.x);
            sy := Integer(p.y*Game.CELL_SIZE + Game.CELL_SIZE - 1 + tetro.y);
            if hit(sx, sy) then
                return True;
            end if;

            sx := Integer(p.x*Game.CELL_SIZE + tetro.x + 1);
            sy := Integer(p.y*Game.CELL_SIZE + Game.CELL_SIZE - 1 + tetro.y);
            if hit(sx, sy) then
                return True;
            end if;

        end loop;

        return False;
    end hitGround;

    function isOutLRLimit( Self : in out Tetromino; veloH : sfInt32) return Boolean is
    begin
        if veloH<0 then
            return Self.isOutLeftLimit;
        elsif veloH>0 then
            return Self.isOutRightLimit;
        end if;
        return True;
    end isOutLRLimit;

end TetrisShape;
