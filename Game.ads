--
--
--
--
with Sf.Graphics.Color;
with Sf.Graphics; use Sf.Graphics;

package game is

    TOP   : constant := 10;
    LEFT  : constant := 10;
    WIDTH : constant := 480;
    WIN_WIDTH   : constant := 480;
    WIN_HEIGHT  : constant := 560;
    NB_COLUMNS  : constant := 12;
    NB_ROWS     : constant := 20;
    CELL_SIZE   : constant := Integer(WIN_WIDTH / (NB_COLUMNS + 7));

    type arrBoard is array (0..NB_COLUMNS*NB_ROWS) of Integer;
    type arrTetrisColors is array (0..7) of Sf.Graphics.Color.sfColor;

    tetrisColors : arrTetrisColors := 
        (
            Color.fromRGB(red=>0, green => 0, blue => 0),
            Color.fromRGB(red=>16#FF#, green => 16#60#, blue => 16#60#),
            Color.fromRGB(red=>16#60#, green => 16#FF#, blue => 16#60#),
            Color.fromRGB(red=>16#60#, green => 16#60#, blue => 16#FF#),
            Color.fromRGB(red=>16#CC#, green => 16#CC#, blue => 16#60#),
            Color.fromRGB(red=>16#CC#, green => 16#60#, blue => 16#CC#),
            Color.fromRGB(red=>16#60#, green => 16#CC#, blue => 16#CC#),
            Color.fromRGB(red=>16#DA#, green => 16#AA#, blue => 0)
        );

end Game;
