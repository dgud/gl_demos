%%%-------------------------------------------------------------------
%%% File    : draw_wings
%%% Author  :  dangud At gmail.com
%%% Description : Test loader of ex1 files it uses VBO buffers and requires good drivers.
%%%               The .wex1 is produced by wings_plugin/wpc_ex1.erl
%%%               plugin code if put in wings-1.0.
%%% Created : Mars 2009 by dangud At gmail.com
%%%-------------------------------------------------------------------

-module(draw_wings).

-compile([export_all, nowarn_export_all]).
-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-compile(inline).

-define(W, 640).
-define(H, 480).

-define(PAN_SPEED, 25).
-define(SHADERS_MENU, 200).
-define(SHADOW_MENU,  201).

-record(cam, 
	{origin,
	 distance,				% From origo.
	 azimuth,
	 elevation,
	 pan_x,					%Panning in X direction.
	 pan_y,					%Panning in Y direction.
	 fov,					%Field of view.
	 hither,				%Near clipping plane.
	 yon,					%Far clipping plane.
	 xs,    %% Prev position
	 ys,    
	 %% Not camera but needed
	 ww,
	 wh,
	 ortho=false
	}).

-record(s,  {frame, canvas, rstate, font, time, cam}).
-record(rs, {mat_shader = false, shadows = false}).

-record(time, {fps=0,      % frames per second
	       fc=0,       % frame counter
	       diff=0,     % Time last frame in ms
	       start = erlang:monotonic_time(),
	       fcst  = erlang:monotonic_time()}).  % frame counter start time


start() ->
    go().

start(Obj) ->
    go(Obj).

go() ->
    go("OldMacLaren.wex1").

go(ObjFile) ->
    {S,Shaders} = initg(),
    Heaven = load_sphere(Shaders),
    Ground = load_plane(Shaders),
    Draw   = load_object(ObjFile, Shaders),
    loop(S, Heaven, [Ground, Draw]),
    wx:destroy(),
    exit(normal).

loop(quit,_,_) ->    ok;
loop(S=#s{frame=F,rstate=RS,canvas=Canvas,cam=Cam,time=T,font=_F}, BG, Render) ->
    %% Setup camera and light
    load_matrices(Cam, fun() -> lights() end),
    %% Draw background  
    %% (should really be before translation but after rotation of the
    %% the camera but this camera doesn't work that way :-)
    BG(T#time.diff, RS),

    %% Render objects
    [Draw(T#time.diff, RS) || Draw <- Render],
    %glf:draw_coord(100),
    
    %% Everything is drawn, show it.
    wxGLCanvas:swapBuffers(Canvas),
    %% Handle events
    Ns = events(S#s{time=fps(F,T)}),
    %% Clear all for the next frame
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    %% Here is a good place to do other calculations
    %% ...

    %% Sync command queue, so we don't choke the driver and get no events 
    _ = wxWindow:getSize(F),
    loop(Ns,BG, Render).

lights() ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.4,0.4,0.4,1.0}),
    gl:enable(?GL_LIGHT0),
    gl:lightfv(?GL_LIGHT0, ?GL_DIFFUSE,  {1.0,1.0,1.0,1.0}),
    gl:lightfv(?GL_LIGHT0, ?GL_SPECULAR, {0.5,0.5,0.5,1.0}),
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {0.71,0.71,0.0,0.0}),
    ok.

events(S=#s{frame=Frame,cam=Cam0}) ->
    receive 
	#wx{event=Mouse=#wxMouse{}} ->
	    {_, Cam} = cam_event(Mouse,Cam0),
	    events(S#s{cam=Cam});
	#wx{event=#wxSize{size={W,H}}} ->
	    gl:viewport(0,0,W,H),
	    events(S#s{cam=Cam0#cam{ww=W,wh=H}});	    
	#wx{event=#wxClose{}} ->
	    quit;
	#wx{id=?wxID_EXIT} ->
	    quit;
	#wx{id=?wxID_ABOUT} ->
	    about_box(Frame),
	    S;
	#wx{id=?SHADERS_MENU} ->	    
	    RS = S#s.rstate,
	    S#s{rstate= RS#rs{mat_shader = not RS#rs.mat_shader}};
	{'_wxe_error_', _, _} -> 
	    %% Canvas is closed
	    quit;
	Other ->
	    io:format("Got ~p ~n",[Other]),
	    events(S)
    after 0 ->
	    S
    end.

initg() ->
    WX = wx:new(),
    Frame = wxFrame:new(WX,1,"Hello 3D-World",[{size, {?W,?H}}]),
    wxFrame:connect(Frame, close_window),
    
    MenuBar  = wxMenuBar:new(),
    FileM    = wxMenu:new([]),
    OptsM    = wxMenu:new([]),
    HelpM    = wxMenu:new([]),    
    wxMenu:append(FileM, ?wxID_EXIT, "&Quit"),    
    wxMenu:append(OptsM, ?SHADERS_MENU, "&Shaders", 
		  [{kind, ?wxITEM_CHECK}, 
		   {help, "Enable Material Shaders"}]), 
    wxMenu:append(OptsM, ?SHADOW_MENU, "&Shadows", 
		  [{kind, ?wxITEM_CHECK}, 
		   {help, "Enable Shadows"}]), 
    wxMenu:append(HelpM, ?wxID_ABOUT,"&About...\tF1"),
    wxMenuBar:append(MenuBar, FileM, "&File"),
    wxMenuBar:append(MenuBar, OptsM, "&Options"),
    wxMenuBar:append(MenuBar, HelpM, "&Help"),    
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    SB = wxFrame:createStatusBar(Frame,[{number,2}]),
    wxStatusBar:setStatusWidths(SB, [-1, 150]),
    SBText = "Use middle mouse button (and Shift or Ctrl) to handle camera",
    wxFrame:setStatusText(Frame, SBText),

    Attrs = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Frame, Attrs),
    Context = wxGLContext:new(Canvas),
    wxFrame:connect(Canvas, size),
    wxFrame:connect(Canvas, motion),
    wxFrame:connect(Canvas, middle_up),
    wxFrame:connect(Canvas, middle_down),
    wxFrame:connect(Canvas, mousewheel),
    
    wxWindow:show(Frame),
    %% Set Current must be called after show and before any opengl call.
    wxGLCanvas:setCurrent(Canvas, Context),

    gl:viewport(0,0,?W,?H),
    
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_CULL_FACE),
    
    gl:enable(?GL_LIGHTING),
    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(0.5,0.5,0.9,1.0),

    Shaders = 
	try 
	    MatSh = load_shader("material"),
	    MatSh
	catch _:_ ->
		undefined 
	end,
    %%DefFont = glfont:load(),
    DefFont = undefined,
    {#s{frame=Frame, canvas=Canvas, font=DefFont, rstate = #rs{},
	time=#time{}, cam=init(?W,?H)},
     Shaders}.

-define(F32, 32/float-native).

enable_shader(#rs{mat_shader=false}, _, _) -> 
    undefined;
enable_shader(#rs{mat_shader=true}, Shader, Fun) ->
    gl:useProgram(Shader),
    Fun(),
    Shader.
disable_shader(undefined) -> ok;
disable_shader(_) ->
    gl:useProgram(0).

load_plane(_Shaders) ->
    fun(Time, _Rs) ->
	    gl:disable(?GL_LIGHTING),
	    gl:color4fv({0.5,0.8,0.4,0.9}),
	    DayTime0 = Time/60000.0,  %% A day in 60 seconds :-)
	    DayTime = DayTime0 - trunc(DayTime0),

	    gl:'begin'(?GL_QUADS),
	    %% Reusing the texture from the sphere
	    gl:texCoord2f(DayTime, 0.03125),
	    gl:vertex3i( 200, 0,  200),
	    gl:vertex3i( 200, 0, -200),
	    gl:vertex3i(-200, 0, -200),
	    gl:vertex3i(-200, 0,  200),
	    gl:'end'(),
	    gl:enable(?GL_LIGHTING)
    end.

%% Let there be a heaven (or hell)
load_sphere(_Shaders) ->
    {Size, DataChunk, [Ns]} = 
	sphere:tris([{subd,3}, {ccw,false}, {binary,true},
		     {scale,100}, {normals,true}]),
    TexCoords = << <<1.0:?F32, Y:?F32>> || 
		    <<_:?F32,Y:?F32,_:?F32>> <= Ns >>,

    StartNormals = size(DataChunk),
    StartTexCoords = StartNormals + size(Ns),
    Data = <<DataChunk/binary, Ns/binary, TexCoords/binary>>,
    
    [Buff] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
    gl:bufferData(?GL_ARRAY_BUFFER, size(Data), Data, ?GL_STATIC_DRAW),

    TexId = load_image("clearSky2.png", [{wrap_s, ?GL_REPEAT}]),
    put(sky_tex, TexId),

    IdMatrix = {1.0,0.0,0.0,0.0, 0.0,1.0,0.0,0.0, 
		0.0,0.0,1.0,0.0, 0.0,0.0,0.0,1.0},
    
    Draw = fun(Time, _Rs) ->
		   %% Setup color and texture
		   gl:disable(?GL_LIGHTING),
		   gl:color4fv({1.0,1.0,1.0,1.0}),
		   gl:enable(?GL_TEXTURE_2D),
		   gl:bindTexture(?GL_TEXTURE_2D, TexId),		   
		   %% Change time in texture
		   gl:matrixMode(?GL_TEXTURE),
		   DayTime0 = Time/60000.0,  %% A day in 60 seconds :-)
		   DayTime = DayTime0 - trunc(DayTime0),
		   gl:loadMatrixf(setelement(1, IdMatrix, DayTime)),

		   %% Setup draw buffers
		   gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
		   gl:vertexPointer(3, ?GL_FLOAT, 0, 0),
		   gl:normalPointer(?GL_FLOAT, 0, StartNormals),
		   gl:texCoordPointer(2,?GL_FLOAT, 0, StartTexCoords),
		   gl:enableClientState(?GL_VERTEX_ARRAY),
		   gl:enableClientState(?GL_NORMAL_ARRAY),
		   gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),

		   %% Draw buffer
		   gl:drawArrays(?GL_TRIANGLES, 0, Size*3),

		   %% Reset texture matrix
		   gl:loadIdentity(),
		   gl:matrixMode(?GL_MODELVIEW),

		   %% Disable these draw buffers
		   gl:bindBuffer(?GL_ARRAY_BUFFER,0),
		   gl:disableClientState(?GL_VERTEX_ARRAY),
		   gl:disableClientState(?GL_NORMAL_ARRAY),
		   gl:enable(?GL_LIGHTING)
	   end,
    Draw.

load_object(ObjF,Shaders) ->
    FBin = read_file(ObjF), 
    %% Parse Contents 
    %% io:format("FileSize of ~s ~p ~n", [ObjF,size(FBin)]),
    <<_Magic:16/little,16#0000:16/little,
     DataSize:32/little,_:120/binary, FBin1/binary>> = FBin,
    %% io:format("Size of ~p Magic ~p ~n", [DataSize,Magic]),
    <<DataChunk:DataSize/binary,MatAndObjs/binary>> = FBin1,
    <<MSz:32/little, MatBlock:MSz/binary, 
     OSz:32/little, ObjBlock:OSz/binary >> = MatAndObjs,    
    ObjRefs = get_objects(ObjBlock,[]),
    Mats    = get_materials(MatBlock, []),
    
    %% Push Data to Gfx card
    [Buff] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
    gl:bufferData(?GL_ARRAY_BUFFER, size(DataChunk), 
		  DataChunk, ?GL_STATIC_DRAW),


    %% Get shader variables if available
    case Shaders of
	undefined -> 
	    TimeShader = ok;
	_ ->	   
	    %% and Heaven on texture unit 0
	    %% Set color texture on texture unit 1

	    gl:useProgram(Shaders),  %% Arrg must be set on ATI.
	    DiffuseTex = gl:getUniformLocation(Shaders, "DiffuseTex"),
	    HeavenTex  = gl:getUniformLocation(Shaders, "HeavenTex"),
	    TimeShader = gl:getUniformLocation(Shaders, "Time"),
	    gl:uniform1i(HeavenTex,  0),
	    gl:uniform1i(DiffuseTex, 1), 
	    gl:useProgram(0),
	    ok
    end,
    Draw = fun(Time, RS) ->
		   R = Time * 360/30000,
		   gl:rotated(R, 0.0, 1.0, 0.0),
		   gl:enableClientState(?GL_VERTEX_ARRAY),
		   gl:enableClientState(?GL_NORMAL_ARRAY),
		   gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
		   gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
		   gl:vertexPointer(3, ?GL_FLOAT, 8*4, 0),
		   gl:normalPointer(?GL_FLOAT, 8*4, 3*4),
		   gl:texCoordPointer(2,?GL_FLOAT, 8*4, 6*4),

		   EnableShader = 
		       fun() -> %% If shader is on do
			       %% A day in 60 seconds :-)
			       DayTime0 = Time/60000.0,  
			       DayTime = DayTime0 - trunc(DayTime0),
			       gl:uniform1f(TimeShader, DayTime),
			       SkyTex = get(sky_tex),
			       gl:activeTexture(?GL_TEXTURE0),
			       gl:bindTexture(?GL_TEXTURE_2D, SkyTex),
			       gl:activeTexture(?GL_TEXTURE1)
		       end,
		   SHEnable = enable_shader(RS, Shaders, EnableShader),

		   lists:foreach(fun({Mat,First,Sz}) ->
					 set_mat(Mat,Mats),
					 gl:drawArrays(?GL_TRIANGLES,First,Sz)
				 end, ObjRefs),

		   %% Reset ...
		   gl:bindTexture(?GL_TEXTURE_2D, 0),
		   gl:activeTexture(?GL_TEXTURE0),
		   gl:bindBuffer(?GL_ARRAY_BUFFER,0),
		   gl:disableClientState(?GL_TEXTURE_COORD_ARRAY),
		   gl:disableClientState(?GL_VERTEX_ARRAY),
		   gl:disableClientState(?GL_NORMAL_ARRAY),
		   disable_shader(SHEnable),
		   ok
	   end,
    Draw.
	  
set_mat(Mat,Mats) ->
    case lists:keysearch(Mat,1,Mats) of
	{value, {Mat,Diff,Amb,Spec,Emission,Shine,Maps}} -> 
	    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_DIFFUSE, Diff),
	    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_AMBIENT, Amb),
	    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_SPECULAR,Spec), 
	    gl:materialf(?GL_FRONT_AND_BACK, ?GL_SHININESS, Shine*128),
	    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_EMISSION, Emission),
	    case Maps of
		[] -> 
		    gl:disable(?GL_TEXTURE_2D),
		    gl:bindTexture(?GL_TEXTURE_2D, 0);
		[{_Type,TexId}|_] ->
		    gl:enable(?GL_TEXTURE_2D),
		    gl:bindTexture(?GL_TEXTURE_2D, TexId)
	    end,
	    ok;
	false ->
	    io:format("Couldn't find ~p in ~p~n", [Mat,Mats])
    end.

get_materials(<<NSz:32/little, Name:NSz/binary, 
	       DR:32/float,DG:32/float,DB:32/float,DA:32/float,
	       AR:32/float,AG:32/float,AB:32/float,AA:32/float,
	       SR:32/float,SG:32/float,SB:32/float,SA:32/float,
	       ER:32/float,EG:32/float,EB:32/float,EA:32/float,
	       Sh:32/float,NoOfImages:32/little,Rest/binary>>, Acc) ->
    {Maps, Next} = get_maps(NoOfImages, Rest, []),
    Mat = {Name,{DR,DG,DB,DA},{AR,AG,AB,AA},
	   {SR,SG,SB,SA},{ER,EG,EB,EA},Sh,
	   Maps},
    %% io:format("Mat ~s with Maps ~p~n", [binary_to_list(Name),NoOfImages]),    
    get_materials(Next, [Mat|Acc]);
get_materials(<<>>, Acc) ->
    Acc.

get_maps(0, Next, Acc) -> {Acc,Next};
get_maps(I, <<TSz:32/little,Type:TSz/binary,
	     FSz:32/little,File0:FSz/binary, Next/binary>>, Acc) ->    
    %% io:format("Map ~s ~s~n", [binary_to_list(Type),binary_to_list(File0)]),
    FileName = lists:reverse(tl(lists:reverse(binary_to_list(File0)))),
    File = filename:rootname(filename:basename(FileName)),
    case get({texid,File}) of
	undefined -> 
	    TexId = load_image(File ++ ".png"),
	    %%TexId = load_image("Powered.bmp"),
	    put({texid,File}, TexId);
	TexId ->
	    TexId
    end,
    get_maps(I-1, Next, [{Type,TexId}|Acc]).

get_objects(<<BSz:32/little,ObjBlock:BSz/binary,Next/binary>>,Acc) ->
    Object = get_object(ObjBlock),
    get_objects(Next,Object++Acc);
get_objects(<<>>,Acc) ->
    Acc.

get_object(<<NameSz:32/little,_Name:NameSz/binary,
	    NoMeshes:32/little,Meshes/binary>>) ->
    %% io:format("Obj ~s ~p ~n",[binary_to_list(Name),NoMeshes]),
    MeshPtrs = get_mesh_ptr(NoMeshes,Meshes,[]),
    %% io:format("   meshes ~p ~n",[MeshPtrs]),
    MeshPtrs.

get_mesh_ptr(0, _, Acc) ->
    lists:reverse(Acc);
get_mesh_ptr(I,<<MSz:32/little,Mat:MSz/binary,Start:32/little,Verts:32/little,Next/binary>>,Acc) ->
    io:format("Mesh ~s ~p Verts ~p~n", [binary_to_list(Mat),Start,Verts]),
    get_mesh_ptr(I-1,Next,[{Mat,Start,Verts}|Acc]).

%%%%%%%%%%%% Camera 

init(W,H) ->
    #cam{origin={0.0,0.0,0.0},
	 azimuth=-45.0,elevation=25.0,
	 distance=50,
	 pan_x=0.0,pan_y=-2.0,
	 fov=45.0,
	 hither=0.1,
	 yon=10000.0,
	 ww=W,
	 wh=H}.


load_matrices(Cam) ->
    load_matrices(Cam,false).
load_matrices(Cam,IncludeLights) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    projection(Cam),
    modelview(Cam, IncludeLights).

projection(#cam{distance=D,fov=Fov,hither=Hither,yon=Yon,
		ww=W,wh=H,ortho=Ortho}) ->
    Aspect = W/H,
    case Ortho of
	false ->
	    glu:perspective(Fov, Aspect, Hither, Yon);
	true ->
	    Sz = D*math:tan(Fov*math:pi()/180/2),
	    gl:ortho(-Sz*Aspect, Sz*Aspect, -Sz, Sz, Hither, Yon)
    end.

modelview(Cam) ->
    modelview(Cam,false).

modelview(#cam{origin=Origin,distance=Dist,azimuth=Az,
	       elevation=El,pan_x=PanX,pan_y=PanY},
	  Lights) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    if
	is_function(Lights) -> 	Lights();
	true -> ok
    end,
    gl:translatef(float(PanX), float(PanY), float(-Dist)),
    gl:rotatef(El, 1.0, 0.0, 0.0),
    gl:rotatef(Az, 0.0, 1.0, 0.0),
    {OX,OY,OZ} = Origin,
    gl:translatef(OX, OY, OZ),
    ok.
	   
%%% Camera events 
%%% Stolen from wings (blender mode)
cam_event(#wxMouse{type=motion, middleDown=false},Cam) -> 
    {[], Cam};
cam_event(#wxMouse{type=mousewheel, wheelRotation=Rot},Cam=#cam{distance=Dist}) ->
    case Rot > 0 of 
	true ->  {[],Cam#cam{distance=Dist+1}};
	false -> {[],Cam#cam{distance=Dist-1}}
    end;
cam_event(#wxMouse{type=motion, middleDown=true,shiftDown=true,x=X,y=Y}, 
	  Cam=#cam{pan_x=PanX0,pan_y=PanY0,distance=D,xs=Xs,ys=Ys}) ->
    %% PAN
    S = D*(1/20)/(101-float(?PAN_SPEED)),
    Dx = (X-Xs)*S,
    Dy = (Y-Ys)*S,
    PanX = PanX0 + Dx,
    PanY = PanY0 - Dy,
    {[],Cam#cam{pan_x=PanX,pan_y=PanY,xs=X,ys=Y}};
cam_event(#wxMouse{type=motion, middleDown=true,controlDown=true,x=X, y=Y}, 
	  Cam=#cam{distance=Dist, ys=Ys}) ->
    %% ZOOM (Dy)
    {[],Cam#cam{distance=Dist+(Y-Ys)/10,xs=X,ys=Y}};
cam_event(#wxMouse{type=motion, middleDown=true,x=X,y=Y}, 
	  Cam=#cam{azimuth=Az0,elevation=El0,xs=Xs,ys=Ys}) ->
    %% Rotate
    Az = Az0 + (X-Xs),
    El = El0 + (Y-Ys),
    {[], Cam#cam{azimuth=Az,elevation=El, xs=X,ys=Y}};
cam_event(#wxMouse{type=middle_down, x=X,y=Y},Cam) -> 
    {[],Cam#cam{xs=X,ys=Y}};
cam_event(Ev,Cam) ->
    %% io:format("Ev ~p~n",[Ev]),
    {Ev,Cam}.

%%%%%%%%%

fps(Frame, T) ->   fps(Frame, T, 500).
fps(Frame, T0 = #time{fcst=FCSt,start=Start,fc=FC},Interval) ->
    Now = erlang:monotonic_time(),
    Diff = tdiff(Now,Start),
    Time = tdiff(Now,FCSt), 
    if Time > Interval ->
	    Fps = round(1000*FC / Time),
	    wxFrame:setStatusText(Frame, io_lib:format("FPS: ~p",[Fps]), 
				  [{number,1}]), %% Zero numbered suddenly
	    T0#time{fc=0,fps=Fps,diff=Diff,fcst=Now};
       true ->
	    T0#time{fc=FC+1,diff=Diff}
    end.

tdiff(A,B) ->
    erlang:convert_time_unit(A-B, native, millisecond).

%%%%%%%%%% File loading

load_image(File) -> 
    load_image(File, []).
load_image(File, Options) ->
    Image = wxImage:new(File),
    true = wxImage:ok(Image),
    IW = wxImage:getWidth(Image),
    IH = wxImage:getHeight(Image),
    Data0 = wxImage:getData(Image),
    %% wxImage is upside/down for OpenGL
    RS = IW*3,
    Split = [Row || <<Row:RS/binary>> <= Data0],
    Data = << <<Row:RS/binary>> || Row <- lists:reverse(Split) >>,

    [Tid] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, Tid),
    Mag = proplists:get_value(mag_filter, Options, ?GL_LINEAR),
    Min = proplists:get_value(min_filter, Options, ?GL_LINEAR),
    WS  = proplists:get_value(wrap_s, Options, ?GL_CLAMP),
    WT  = proplists:get_value(wrap_t, Options, ?GL_CLAMP),    
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, Mag),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, Min),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, WS),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, WT),

    gl:texImage2D(?GL_TEXTURE_2D, 0, 3, IW, IH, 0, ?GL_RGB, 
		  ?GL_UNSIGNED_BYTE, Data),
    wxImage:destroy(Image),
    Tid.

read_file(ObjF) ->
    case find_file(ObjF) of
	{File, ".wex1"} ->
	    {ok, Bin} = file:read_file(File),
	    Bin;
	{File, ".gz"} ->
	    {ok, Bin} = file:read_file(File),
	    zlib:gunzip(Bin)
    end.
	   
find_file(ObjF) ->
    File0 = filename:absname(ObjF),
    Ext = filename:extension(ObjF),
    case file:read_file_info(File0) of
	{error, _} when Ext =:= ".wex1" ->
	    find_file(ObjF ++ ".gz");
	{error, _} ->
	    io:format("Can not find file ~s or ~s.gz~n",[ObjF, ObjF]),
	    exit({file_not_found, ObjF});
	{ok, _} ->
	    {ObjF,Ext}
    end.

load_shader(Shader) ->
    {ok, VsBin} = file:read_file(Shader ++ ".vs"),
    {ok, FsBin} = file:read_file(Shader ++ ".fs"),
  
    VS = gl:createShader(?GL_VERTEX_SHADER),
    FS = gl:createShader(?GL_FRAGMENT_SHADER),
    
    gl:shaderSource(VS, [VsBin]),
    gl:shaderSource(FS, [FsBin]),
    
    gl:compileShader(VS),
    gl:compileShader(FS),
    printShaderInfoLog(Shader, VS),
    printShaderInfoLog(Shader, FS),
    
    Prog = gl:createProgram(),
    gl:attachShader(Prog, VS),
    gl:attachShader(Prog, FS),

    gl:linkProgram(Prog),

    case gl:getProgramiv(Prog, ?GL_INFO_LOG_LENGTH) of	
	Length when Length > 1 ->
	    io:format("  ~s: ~s~n",[Shader,gl:getProgramInfoLog(Prog,Length)]);
	_ ->  ok
    end,
    
    gl:useProgram(0),
    Prog.

printShaderInfoLog(File, Shader) ->
    Length  = gl:getShaderiv(Shader, ?GL_INFO_LOG_LENGTH),
    case Length > 1 of
	true ->
	    Log = gl:getShaderInfoLog(Shader, Length),
	    io:format("  ~s: ~s~n",[File, Log]);
	false ->
	    ok
    end.

%%%%%%%%%

about_box(Frame) ->
    Env = wx:get_env(),
    OsInfo = [wx_misc:getOsDescription(),gl:getString(?GL_VENDOR),
	      gl:getString(?GL_RENDERER),gl:getString(?GL_VERSION)],

    spawn(fun() ->
		  wx:set_env(Env),
		  Str = "An OpenGL demo showing how to load "
		      "models from wings3D\n with textures and "
		      "using other basic opengl commands.\n\n"
		      "The model is exported by using the example exporter:\n"
		      "   http://www.erlang.org/~dgud/wings/wpc_ex1.erl.html\n\n",
		  
		  Info = io_lib:format("Os:         ~s~n~nGL Vendor:     ~s~n"
				       "GL Renderer:   ~s~nGL Version:    ~s~n",
				       OsInfo), 
		  MD = wxMessageDialog:new(Frame, Str ++ Info, 
					   [{style, ?wxOK}, 
					    {caption, "Opengl Example"}]),
		  wxDialog:showModal(MD),
		  wxDialog:destroy(MD)
	  end),
    ok.
