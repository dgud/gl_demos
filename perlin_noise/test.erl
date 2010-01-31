%%%-------------------------------------------------------------------
%%% File    : test.erl
%%% Author  : Dan Gudmundsson <dgud@erlang.org>
%%% Description : Test the gl_noise function.
%%%
%%% Created : 29 Jan 2010 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(test).

-compile(export_all).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-record(time, {fps=0,      % frames per second
	       fc=0,       % frame counter
	       diff=0,     % Time last frame in ms
	       start =erlang:now(),
	       fcst  =erlang:now()}).  % frame counter start time

-record(s,  {frame, canvas, shader, draw, time}).


-define(W, 800).
-define(H, 600).

-define(SHADERS_MENU, 200).
-define(SHADOW_MENU,  201).

-define(F32, 32/float-native).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go() ->
    start().

start() ->
    State0 = initg(),
    State1 = init_shader(State0),
    State  = load_sphere(State1),
    loop(State),
    wx:destroy(),
    ok.

loop(quit) ->    ok;
loop(State=#s{frame=F, canvas=Canvas, draw=Draw, time=T}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:perspective(40, ?W/?H, 0.1, 1000),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    glu:lookAt(0.0,0.0,10.0, 0.0,0.0,0.0, 0.0,1.0,0.0),
    Draw(T#time.diff, foo),
    Ns = events(State#s{time=fps(F,T)}),
    wxGLCanvas:swapBuffers(Canvas),
    %% Sync command queue, so we don't choke the driver and get no events 
    _ = wxWindow:getSize(F),
    loop(Ns).

events(S=#s{frame=Frame}) ->
    receive 
	#wx{event=#wxClose{}} ->
	    quit;
	#wx{id=?wxID_EXIT} ->
	    quit;
	#wx{id=?wxID_ABOUT} ->
	    about_box(Frame),
	    S;
	#wx{id=?SHADERS_MENU} ->	    
	    %% RS = S#s.rstate,
	    %% S#s{rstate= RS#rs{mat_shader = not RS#rs.mat_shader}};
	    S;
	{'_wxe_error_', _, _} -> 
	    %% Canvas is closed
	    quit;
	Other ->
	    io:format("Got ~p ~n",[Other]),
	    events(S)
    after 0 ->
	    S
    end.

init_shader(S) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, VsBin} = file:read_file(filename:join(Dir, "noise_test.vs")),
    {ok, FsBin0} = file:read_file(filename:join(Dir, "glsl_noise.fs")),

    Main = <<"
     uniform float time; // Used for texture animation
     /*
      * Both 2D and 3D texture coordinates are defined, for testing purposes.
      */
     varying vec2 v_texCoord2D;
     varying vec3 v_texCoord3D;
     varying vec4 v_color;

     void main( void )
     {
      //float n = noise(v_texCoord2D * 32.0 + 240.0);
      //float n = snoise(v_texCoord2D * 16.0);
      //float n = noise(vec3(4.0 * v_texCoord3D.xyz * (2.0 + sin(0.25 * time))));
      float n = snoise(vec3(2.0 * v_texCoord3D.xyz * (2.0 + sin(0.10 * time))));
      //float n = noise(vec4(8.0 * v_texCoord3D.xyz, 0.5 * time));
      //float n = snoise(vec4(4.0 * v_texCoord3D.xyz, 0.5 * time));
      gl_FragColor = v_color * vec4(0.5 + 0.5 * vec3(n, n, n), 1.0);
     }
    ">>,
    FsBin = <<FsBin0/binary, Main/binary>>,
    VS = gl:createShader(?GL_VERTEX_SHADER),
    FS = gl:createShader(?GL_FRAGMENT_SHADER),
    
    gl:shaderSource(VS, [VsBin]),
    gl:shaderSource(FS, [FsBin]),
    
    gl:compileShader(VS),
    gl:compileShader(FS),
    printShaderInfoLog("vertex_shader", VS),
    printShaderInfoLog("fragment_shader", FS),
    
    Prog = gl:createProgram(),
    gl:attachShader(Prog, VS),
    gl:attachShader(Prog, FS),

    gl:linkProgram(Prog),

    case gl:getProgramiv(Prog, ?GL_INFO_LOG_LENGTH) of	
	Length when Length > 1 ->
	    Info = gl:getProgramInfoLog(Prog,Length),
	    io:format("  ~s: ~s~n",["GLSLPerlin", Info]);
	_ ->  ok
    end,
    
    S#s{shader=Prog}.

printShaderInfoLog(File, Shader) ->
    Length  = gl:getShaderiv(Shader, ?GL_INFO_LOG_LENGTH),
    case Length > 1 of
	true ->
	    Log = gl:getShaderInfoLog(Shader, Length),
	    io:format("  ~s: ~s~n",[File, Log]);
	false ->
	    ok
    end.

initg() ->
    WX = wx:new(),
    Frame = wxFrame:new(WX,1,"Perlin Noise (from GLSL) Demo",
			[{size, {?W,?H}}]),
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

    Attrs = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Frame, Attrs),
    %% wxFrame:connect(Canvas, size),

    wxWindow:show(Frame),
    %% Set Current must be called after show and before any opengl call.
    wxGLCanvas:setCurrent(Canvas),

    gl:viewport(0,0,?W,?H),

    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_CULL_FACE),

    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(0.2,0.3,0.2,1.0),

    #s{frame=Frame, canvas=Canvas, time=#time{}}.


load_sphere(State = #s{shader=Prog}) ->
    {Size, Data, _} = sphere:tris([{subd,5}, {ccw,false}, {binary,true}, {scale,3}]),
    
    [Buff] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
    gl:bufferData(?GL_ARRAY_BUFFER, size(Data), Data, ?GL_STATIC_DRAW),
    
    gl:useProgram(Prog),
    {ActivateGLSL, _Destroy} = gl_noise:init_gl(Prog, 0, 1),
    TimeLoc = gl:getUniformLocation(Prog, "time"),

    Draw = fun(Time, _Rs) ->
		   gl:color3ub(150,70,70),
		   gl:useProgram(Prog),
		   ActivateGLSL(),
		   gl:uniform1f(TimeLoc, Time/1000),
		   %% Setup draw buffers
		   gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
		   gl:vertexPointer(3, ?GL_FLOAT, 0, 0),
		   gl:enableClientState(?GL_VERTEX_ARRAY),

		   %% Draw buffer
		   gl:drawArrays(?GL_TRIANGLES, 0, Size*3),
		   
		   %% Disable these draw buffers
		   gl:bindBuffer(?GL_ARRAY_BUFFER,0),
		   gl:disableClientState(?GL_VERTEX_ARRAY)
	   end,
    State#s{draw=Draw}.

%%%%%%%%%

fps(Frame, T) ->   fps(Frame, T, 500).
fps(Frame, T0 = #time{fcst=FCSt,start=Start,fc=FC},Interval) ->
    Now = erlang:now(),
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

tdiff({A2,B2,C2},{A1,B1,C1}) ->
    (A2-A1)*1000000+(B2-B1)*1000 + (C2-C1) / 1000.


%%%%%%%%%%%%%%%%%%%%%%%%%%
about_box(Frame) ->
    Env = wx:get_env(),
    OsInfo = [wx_misc:getOsDescription(),gl:getString(?GL_VENDOR),
	      gl:getString(?GL_RENDERER),gl:getString(?GL_VERSION)],
    
    spawn(fun() ->
		  wx:set_env(Env),
		  Str = "An OpenGL demo showing how to use "
		      " generate and use perlin noise ",

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
