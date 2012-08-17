%%%-------------------------------------------------------------------
%%% File    : image.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%%           Klas Johansson <klajo@users.sourceforge.net>
%%% Description : Test images (and read/write binaries) to and from opengl
%%%
%%% Created : 12 Jun 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(image).

-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 

-export([start/0]).

-record(s,  {f,w1,w2,w3}).
-record(img,{win, image, bmp}).
-record(gl, {win, data, deg, mat, alpha, text}).

-record(texture, {tid, w, h, minx, miny, maxx, maxy}).

start() ->
    spawn_link(fun() -> init() end).

init() ->
    P = wx:new(),
    Frame = wxFrame:new(P,1, "wxImage Test"),
    Panel = wxPanel:new(Frame),
    %% Menues
    Menu = wxMenu:new(),
    %%wxMenu:append(Menu, ?wxID_OPEN,  "Load Image"),
    wxMenu:append(Menu, ?wxID_ABOUT, "About"),
    wxMenu:append(Menu, ?wxID_EXIT,  "Quit"),
    MB = wxMenuBar:new(),
    wxMenuBar:append(MB,Menu,"File"),
    wxFrame:setMenuBar(Frame, MB),
    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, close_window, [{skip,true}]),
    wxFrame:createStatusBar(Frame,[]),
    %% Sub-Windows
    Opts = [{size, {300,300}}, {style, ?wxSUNKEN_BORDER}],
    W1 = wxWindow:new(Panel, ?wxID_ANY, Opts),
    wxWindow:connect(W1, paint, [{skip, true}]),
    Image   = wxImage:new("Powered.bmp"),
    GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    W2 = wxGLCanvas:new(Panel,Opts ++ GLAttrib),
    W3 = wxWindow:new(Panel, ?wxID_ANY, Opts),
    wxWindow:connect(W3, paint, [{skip, true}]),
    %% Setup sizer
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,1),
    wxSizerFlags:expand(SF),
    wxSizer:add(Sz, W1, SF), 
    wxSizer:addSpacer(Sz,3),
    wxSizer:add(Sz, W2, SF), 
    wxSizer:addSpacer(Sz,3),   
    wxSizer:add(Sz, W3, SF), 
    wxWindow:setSizer(Panel,Sz),
    wxSizer:setSizeHints(Sz,Frame),
    %% Show
    wxFrame:show(Frame),
    wxGLCanvas:setCurrent(W2),
    GL = setup_gl(W2,Image),
    State = screenshot(#s{f=Frame,w1=#img{win=W1,image=Image},
			  w2=GL,w3=#img{win=W3}}),
    loop(State, 0).
		
loop(S,C) ->
    receive 
	#wx{obj=Win,event=#wxPaint{}} ->
	    S1 = redraw(Win,S),
	    loop(S1,C);
	#wx{id=?wxID_ABOUT} ->
	    D = wxMessageDialog:new(wx:null(),"Image and binary testing", 
				    [{style,?wxICON_INFORMATION bor ?wxOK}]),
	    wxMessageDialog:showModal(D),
	    loop(S,C);
	#wx{id=EXIT, event=EV} when EXIT =:= ?wxID_EXIT; is_record(EV, wxClose) ->
	    catch wxWindow:'Destroy'(S#s.f),
	    ok;
	Foo ->
	    io:format("Got ~p~n", [Foo]),	    
	    loop(S,C)
    after 10 ->
	    S1 = update_rotation(S),
	    drawBox(S1#s.w2),
	    S2 = case C rem 50 of
		     0 -> redraw_all(screenshot(S1));
		     _ -> S1
		 end,
	    loop(S2, C+1)
    end.

redraw_all(S0 = #s{w1=#img{win=W1},w3=#img{win=W3}}) ->
    S1 = redraw(W1, S0),
    redraw(W3, S1).

redraw(Win, S=#s{w1=Img0=#img{win=Win}}) ->
    Img = wx:batch(fun() -> redraw2(Img0, "Cube Texture") end),
    S#s{w1=Img};
redraw(Win, S=#s{w3=Img0=#img{win=Win}}) ->
    Img = wx:batch(fun() -> redraw2(Img0, "Opengl to wxImage") end),
    S#s{w3=Img};
redraw(_,S) ->S.

redraw2(Img = #img{image=undefined,bmp=undefined},_) -> Img;
redraw2(Img = #img{bmp=undefined,image=Image},Txt) -> 
    Bmp = wxBitmap:new(Image),
    redraw2(Img#img{bmp=Bmp},Txt);
redraw2(Img = #img{win=Win,bmp=Bmp},Txt) ->
    DC0  = wxClientDC:new(Win),
    DC = wxBufferedDC:new(DC0),
    wxDC:clear(DC),
    wxDC:drawText(DC, Txt, {10,10}),
    wxDC:drawBitmap(DC,Bmp, {10,30}),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    Img.

-define(VS, {{-0.5, -0.5, -0.5},  %1
	     { 0.5, -0.5, -0.5},  %2
	     { 0.5,  0.5, -0.5},   
	     {-0.5,  0.5, -0.5},  %4
	     {-0.5,  0.5,  0.5},
	     { 0.5,  0.5,  0.5},  %6
	     { 0.5, -0.5,  0.5}, 
	     {-0.5, -0.5,  0.5}}).%8

-define(FACES, 
	%% Faces    Normal     U-axis   V-axis 
	[{{1,2,3,4},{0,0,-1},{-1,0,0}, {0,1,0}},  % 
	 {{8,1,4,5},{-1,0,0},{0,0,1},  {0,1,0}},  %
	 {{2,7,6,3},{1,0,0}, {0,0,-1}, {0,1,0}},  %
	 {{7,8,5,6},{0,0,1}, {1,0,0},  {0,1,0}},  %
	 {{4,3,6,5},{0,1,0}, {-1,0,0}, {0,0,1}},  %
	 {{1,2,7,8},{0,-1,0},{1,0,0},  {0,0,1}}]).

-define(COLORS,{{ 0.0,  0.0,  0.0},		
		{ 1.0,  0.0,  0.0},
		{ 1.0,  1.0,  0.0}, 
		{ 0.0,  1.0,  0.0},
		{ 0.0,  1.0,  1.0},
		{ 1.0,  1.0,  1.0},
		{ 1.0,  0.0,  1.0},
		{ 0.0,  0.0,  1.0}}).


update_rotation(S=#s{w2=GL=#gl{deg=Rot}}) ->
    S#s{w2=GL#gl{deg = Rot + 1.0}}.

screenshot(S=#s{w2=#gl{win=Win}, w3=W3=#img{image=OldImg,bmp=OldBmp}}) ->
    {W,H} = wxWindow:getClientSize(Win),
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),
    gl:readBuffer(?GL_FRONT),
    Mem = wx:create_memory(W*H*3),
    gl:readPixels(0, 0, W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin0 = wx:get_memory_bin(Mem),
    WSz = W*3,
    Im = [Row || <<Row:WSz/binary>> <= ImageBin0],
    ImageBin = list_to_binary(lists:reverse(Im)),
    NewImage = wxImage:new(W,H,ImageBin),
    case OldImg of 
	undefined -> ignore;
	_ -> wxImage:destroy(OldImg)
    end,
    case OldBmp of 
	undefined -> ignore;
	_ -> wxBitmap:destroy(OldBmp)
    end,
    S#s{w3=W3#img{image=NewImage,bmp=undefined}}.

%% Needs to setup opengl after window is shown...
%% GL context is created when shown first time.
setup_gl(Win, Image) ->
    {W,H} = wxWindow:getClientSize(Win),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho( -2.0, 2.0, -2.0*H/W, 2.0*H/W, -20.0, 20.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:clearColor(1.0,1.0,1.0,1.0),
    MatTexture = load_texture_by_image(Image),
    ImgTexture = load_texture_by_image(
		   wxImage:new("erlang-drop-shadow-256.png")),
    Font = wxFont:new(32, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    StrTexture = load_texture_by_string(Font, {40, 40, 40}, "Text from wxFont"),
    gl:enable(?GL_TEXTURE_2D),
    #gl{win=Win,data={?FACES,?VS,?COLORS},deg=0.0,
	mat=MatTexture, alpha=ImgTexture, text=StrTexture}.

drawBox(#gl{win=Win,deg=Deg,data={Fs,Vs,Colors},mat=MatT,alpha=ImgA,text=Text}) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:rotatef(Deg, 1.0, 1.0, 1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:bindTexture(?GL_TEXTURE_2D, MatT#texture.tid),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),
    gl:'begin'(?GL_QUADS),
    wx:foreach(fun(Face) -> drawFace(Face,Vs,Colors) end, Fs),
    gl:'end'(),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    enter_2d_mode(Win),
    Move = abs(90 - (trunc(Deg) rem 180)),
    draw_texture(50, 20+Move, Text),
    draw_texture(80, 180-Move, ImgA),
    leave_2d_mode(),
    wxGLCanvas:swapBuffers(Win).

drawFace({{V1,V2,V3,V4},N,_Ut,_Vt}, Cube, Colors) ->
    gl:normal3fv(N),
    gl:color3fv(element(V1, Colors)),
    gl:texCoord2f(0.0, 0.0), gl:vertex3fv(element(V1, Cube)),
    gl:color3fv(element(V2, Colors)),
    gl:texCoord2f(1.0, 0.0), gl:vertex3fv(element(V2, Cube)),
    gl:color3fv(element(V3, Colors)),
    gl:texCoord2f(1.0, 1.0), gl:vertex3fv(element(V3, Cube)),
    gl:color3fv(element(V4, Colors)),
    gl:texCoord2f(0.0, 1.0), gl:vertex3fv(element(V4, Cube)).
    

draw_texture(X, Y,  #texture{tid = TId, w = W, h = H,
			     miny = MinY, minx = MinX,
			     maxx = MaxX, maxy = MaxY}) ->
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:'begin'(?GL_TRIANGLE_STRIP),
    gl:texCoord2f(MinX, MinY), gl:vertex2i(X,   Y  ),
    gl:texCoord2f(MaxX, MinY), gl:vertex2i(X+W div 2, Y  ),
    gl:texCoord2f(MinX, MaxY), gl:vertex2i(X,   Y+H div 2),
    gl:texCoord2f(MaxX, MaxY), gl:vertex2i(X+W div 2, Y+H div 2),
    gl:'end'().

load_texture_by_image(Image) ->
    ImgW = wxImage:getWidth(Image),
    ImgH = wxImage:getHeight(Image),
    W = get_power_of_two_roof(ImgW),
    H = get_power_of_two_roof(ImgH),
    Data = get_data_for_use_with_teximage2d(Image),
    %% Create an OpenGL texture for the image
    [TId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
    Format = case wxImage:hasAlpha(Image) of
		 true  -> ?GL_RGBA;
		 false -> ?GL_RGB
	     end,
    gl:texImage2D(?GL_TEXTURE_2D, 0,
		  Format, W, H, 0,
 		  Format, ?GL_UNSIGNED_BYTE, Data),
    #texture{tid = TId, w = ImgW, h = ImgH, 
	     minx = 0, miny = 0, maxx = ImgW / W, maxy = ImgH / H}.


%% This algorithm (based on http://d0t.dbclan.de/snippets/gltext.html)
%% prints a string to a bitmap and loads that onto an opengl texture.
%% Comments for the createTexture function:
%%
%%    "Creates a texture from the settings saved in TextElement, to be
%%     able to use normal system fonts conviently a wx.MemoryDC is
%%     used to draw on a wx.Bitmap. As wxwidgets device contexts don't
%%     support alpha at all it is necessary to apply a little hack to
%%     preserve antialiasing without sticking to a fixed background
%%     color:
%%
%%     We draw the bmp in b/w mode so we can use its data as a alpha
%%     channel for a solid color bitmap which after GL_ALPHA_TEST and
%%     GL_BLEND will show a nicely antialiased text on any surface.
%% 
%%     To access the raw pixel data the bmp gets converted to a
%%     wx.Image. Now we just have to merge our foreground color with
%%     the alpha data we just created and push it all into a OpenGL
%%     texture and we are DONE *inhalesdelpy*"
load_texture_by_string(Font, Color, String) ->
    DC = wxMemoryDC:new(),
    wxMemoryDC:setFont(DC, Font),
    {StrW, StrH} = wxMemoryDC:getMultiLineTextExtent(DC, String),
    W = get_power_of_two_roof(StrW),
    H = get_power_of_two_roof(StrH),
    Bmp = wxBitmap:new(W, H, [{depth, -1}]),
    wxMemoryDC:selectObject(DC, Bmp),
    wxMemoryDC:setBackground(DC, wxBrush:new({0, 0, 0})),
    wxMemoryDC:clear(DC),
    wxMemoryDC:setTextForeground(DC, {255, 255, 255}),
    wxMemoryDC:drawText(DC, String, {0, 0}),
    Img = wxBitmap:convertToImage(Bmp),
    wxMemoryDC:destroy(DC),
    
    Alpha = wxImage:getData(Img),
    Data = colourize_image(Alpha, Color),
    [TId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, W, H, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Data),

    #texture{tid = TId, w = StrW, h = StrH, 
 	     minx = 0, miny = 0, maxx = StrW / W, maxy = StrH / H}.

colourize_image(Alpha, {R,G,B}) ->
    list_to_binary([<<R,G,B,A>> || <<A,_B,_G>> <= Alpha]).

get_data_for_use_with_teximage2d(Image) ->
    RGB = wxImage:getData(Image),
    case wxImage:hasAlpha(Image) of
	true ->
 	    Alpha = wxImage:getAlpha(Image),
 	    interleave_rgb_and_alpha(RGB, Alpha);
	false ->
	    RGB
    end.

interleave_rgb_and_alpha(RGB, Alpha) ->
    list_to_binary(
      lists:zipwith(fun({R, G, B}, A) ->
			    <<R, G, B, A>>
				end,
		    [{R,G,B} || <<R, G, B>> <= RGB],
		    [A || <<A>> <= Alpha])).


enter_2d_mode(Win) ->
    {W, H} = wxWindow:getClientSize(Win),

    %% Note, there may be other things you need to change,
    %% depending on how you have your OpenGL state set up.
    gl:pushAttrib(?GL_ENABLE_BIT),
    gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_CULL_FACE),
    gl:enable(?GL_TEXTURE_2D),

    %% This allows alpha blending of 2D textures with the scene
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
       
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),

    %% SDL coordinates will be upside-down in the OpenGL world.  We'll
    %% therefore flip the bottom and top coordinates in the orthogonal
    %% projection to correct this.  
    %% Note: We could flip the texture/image itself, but this will
    %% also work for mouse coordinates.
    gl:ortho(0.0, W, H, 0.0, 0.0, 1.0),

    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity().

leave_2d_mode() ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:popAttrib().

get_power_of_two_roof(X) ->
    get_power_of_two_roof_2(1, X).

get_power_of_two_roof_2(N, X) when N >= X -> N;
get_power_of_two_roof_2(N, X)             -> get_power_of_two_roof_2(N*2, X).
    
