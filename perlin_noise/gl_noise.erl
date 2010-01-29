%%%-------------------------------------------------------------------
%%% File    : gl_noise.erl
%%% Author  : Dan Gudmundsson <dgud@erlang.org>
%%% Description : GLSL Perlin (simplex) noise.
%%%       This a port of Stefan Gustavson (stegu@itn.liu.se) GLSL-noise code.
%%%
%%% Created : 28 Jan 2010 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(gl_noise).
-compile(export_all).
-include_lib("wx/include/gl.hrl").

%%% Creates the two needed tables as binaries.
init() ->
    PermTable = create_table(fun permTexture/3),
    GradTable = create_table(fun gradTexture/3),
    [PermTable, GradTable].

%%%
%%% Sends the two needed tables to opengl
%%% init_gl([Tables]) -> TexIds.
%%%
init_gl() ->
    init_gl(init()).
init_gl([PermTable, GradTable]) ->
    Ids  = [PermId, GradId] = gl:genTextures(2),
    Load = fun(Id, Bin) -> 
		   gl:bindTexture(Id),
		   gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, 256, 256, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Bin),
		   gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
		   gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST)
	   end,
    Load(PermId,PermTable),
    Load(GradId,GradTable),
    
    Ids.

%% Looping noise 
%% Given that F is your function that generates a real
%% number from a point in noise-space, and you want your animation to
%% repeat every t units, you define a new function that loops when z
%% is between 0 and t:

%% Floop(x, y, z) = ( (t - z) * F(x, y, z) + (z) * F(x, y, z - t) ) / (t) 

%%% Creating tiling textures

%% Say you're using a noise function to generate textures for a
%% polygon-based engine. Chances are, you might want the textures to
%% be tileable. You can extend the above example to generate tileable
%% 2-dimensional noise that repeats every w units in the x dimension
%% and every h units in the y dimension. This will require a weighted
%% sum of four calls to the original function (which again, for lack
%% of imagination, we will call F).

%%     Ftileable(x, y) = (
%% 	       F(x, y) * (w - x) * (h - y) +
%% 		   F(x - w, y) * (x) * (h - y) +
%% 		   F(x - w, y - h) * (x) * (y) +
%% 		   F(x, y - h) * (w - x) * (y)
%% 	      ) / (wh) 

%% A pattern should be emerging here, so it shouldn't surprise you
%% that you'll need evaluate a weighted sum of 8 calls to the noise
%% function if you want to have a repeating animation that tiles in
%% the x and y dimenstions. I'm not going to write it out here out of
%% space considerations, and also because it should be evident what
%% the sum will be.


%%%%%%%%%%%%%% Implementation

%% Perlins random numbers
-define(PERM_D, 
	{151,160,137,91,90,15,
	 131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
	 190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
	 88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
	 77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
	 102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
	 135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
	 5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
	 223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
	 129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
	 251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
	 49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
	 138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180}).

-define(PERM(V), element((V)+1, ?PERM_D)).

%% These are Ken Perlin's proposed gradients for 3D noise. I kept them for
%% better consistency with the reference implementation, but there is really
%% no need to pad this to 16 gradients for this particular implementation.
%% If only the "proper" first 12 gradients are used, they can be extracted
%% from the grad4[][] array: grad3[i][j] == grad4[i*2][j], 0<=i<=11, j=0,1,2

-define(GRAD3_D, 
	{{0,1,1},{0,1,-1},{0,-1,1},{0,-1,-1},
	 {1,0,1},{1,0,-1},{-1,0,1},{-1,0,-1},
	 {1,1,0},{1,-1,0},{-1,1,0},{-1,-1,0}, %% 12 cube edges
	 {1,0,-1},{-1,0,-1},{0,-1,1},{0,1,1}}).

-define(GRAD3(X,Y), (element(Y, element((X),?GRAD3_D)))).

%% These are my (Stefans) own proposed gradients for 4D noise. They are the coordinates
%% of the midpoints of each of the 32 edges of a tesseract, just like the 3D
%% noise gradients are the midpoints of the 12 edges of a cube.

-define(GRAD4_D, 
	{{0,1,1,1}, {0,1,1,-1}, {0,1,-1,1}, {0,1,-1,-1}, %% 32 tesseract edges
	 {0,-1,1,1}, {0,-1,1,-1}, {0,-1,-1,1}, {0,-1,-1,-1},
	 {1,0,1,1}, {1,0,1,-1}, {1,0,-1,1}, {1,0,-1,-1},
	 {-1,0,1,1}, {-1,0,1,-1}, {-1,0,-1,1}, {-1,0,-1,-1},
	 {1,1,0,1}, {1,1,0,-1}, {1,-1,0,1}, {1,-1,0,-1},
	 {-1,1,0,1}, {-1,1,0,-1}, {-1,-1,0,1}, {-1,-1,0,-1},
	 {1,1,1,0}, {1,1,-1,0}, {1,-1,1,0}, {1,-1,-1,0},
	 {-1,1,1,0}, {-1,1,-1,0}, {-1,-1,1,0}, {-1,-1,-1,0}}).

-define(GRAD4(X,Y), (element(Y, element((X),?GRAD4_D)))).

create_table(Fun) ->
    Pixels = for_256(Fun, 0,0,array:new(256*256)),
    array:foldl(fun(_,V,Acc) -> <<Acc/binary, V:32>> end, <<>>, Pixels).

%% permTexture - create and load a 2D texture for
%% a combined index permutation and gradient lookup table.
%% This texture is used for 2D and 3D noise, both classic and simplex.

permTexture(I,J,A) ->
    Offset = I*256+J,
    Value  = ?PERM((J+?PERM(I)) band 16#FF),
    V1 = (Value band 16#0F) +1,
    P1 = ?GRAD3(V1,1) * 64+64,
    P2 = ?GRAD3(V1,2) * 64+64,
    P3 = ?GRAD3(V1,3) * 64+64,
    P4 = Value,
    P = (P1 bsl 24) bor (P2 bsl 16) bor (P3 bsl 8) bor P4,
    array:set(Offset, P, A).

gradTexture(I,J,A) ->
    Offset = I*256+J,
    Value  = ?PERM((J+?PERM(I)) band 16#FF),
    V1 = (Value band 16#1F) +1,
    P1 = ?GRAD4(V1,1) * 64+64,
    P2 = ?GRAD4(V1,2) * 64+64,
    P3 = ?GRAD4(V1,3) * 64+64,
    P4 = ?GRAD4(V1,4) * 64+64,
    P = (P1 bsl 24) bor (P2 bsl 16) bor (P3 bsl 8) bor P4,
    array:set(Offset, P, A).


%%%%%%%

for_256(Fun,I,256,Acc) ->
    case I of
	255 -> Acc;
	_ -> for_256(Fun,I+1,0,Acc)
    end;
for_256(Fun,I,J,A) ->
    for_256(Fun,I,J+1, Fun(I,J,A)).


