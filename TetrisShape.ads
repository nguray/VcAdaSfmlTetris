--
--
--
--
with Sf; use Sf;
with Sf.Graphics; use Sf.Graphics;

with Game; use Game;

package TetrisShape is

    type Vector2i is record
        x : sfInt32;
        y : sfInt32; 
    end record;
    
    subtype Index_arrV is Integer range 0 .. 3;
    type arrV is array (Index_arrV) of Vector2i;

    type Tetromino is tagged 
    record
        x       : sfInt32;
        y       : sfInt32;
        ityp    : Integer;
        v       : arrV;
    end record;

    procedure Init(Self : in out Tetromino; typ : Integer; x : sfInt32; y : sfInt32);
    procedure draw(Self : in out Tetromino; render : sfRenderWindow_Ptr);
    procedure rotateLeft(Self : in out Tetromino);
    procedure rotateRight(Self : in out Tetromino);
    function minX(Self : in out Tetromino) return sfInt32;
    function maxX(Self : in out Tetromino) return sfInt32;
    function maxY(Self : in out Tetromino) return sfInt32;
    function checkLeftLimit(Self : in out Tetromino) return Boolean;
    function checkRightLimit(Self : in out Tetromino) return Boolean;
    function checkBottomLimit(Self : in out Tetromino) return Boolean;
    function hitGround(Self : in out Tetromino; board : in arrBoard) return Boolean;
    function Column(Self : in Tetromino) return Integer;

end TetrisShape;
