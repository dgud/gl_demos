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
	       start =erlang:monotonic_time(),
	       fcst  =erlang:monotonic_time()}).  % frame counter start time

-record(s,  {frame, canvas, shader, draw, time, active=4}).

-define(W, 800).
-define(H, 600).

-define(NOISE_SHADER, 200).

-define(F32, 32/float-native).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go() ->
    spawn_link(fun() -> start() end).

start() ->
    State0 = initg(),
    State1 = init_shader(State0),
    State  = load_sphere(State1),
    loop(State),
    wx:destroy(),
    ok.

loop(quit) ->    ok;
loop(State=#s{frame=F, canvas=Canvas, draw=Draw, time=T, shader=Shs, active=Active}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    Shader = lists:nth(Active, Shs),
    case Shader of 
	{_,2,_} ->
	    gl:matrixMode(?GL_PROJECTION),
	    gl:loadIdentity(),
	    glu:ortho2D(0.0,float(?W),0.0,float(?H)),
	    gl:matrixMode(?GL_MODELVIEW),
	    gl:loadIdentity(),
	    ok;
	{_,3,_} ->
	    gl:matrixMode(?GL_PROJECTION),
	    gl:loadIdentity(),
	    glu:perspective(40.0, ?W/?H, 0.1, 1000.0),
	    gl:matrixMode(?GL_MODELVIEW),
	    gl:loadIdentity(),
	    glu:lookAt(0.0,0.0,10.0, 0.0,0.0,0.0, 0.0,1.0,0.0)
    end,
    Draw(T#time.diff, lists:nth(Active, Shs)),
    Ns = events(State#s{time=fps(F,T)}),
    wxGLCanvas:swapBuffers(Canvas),
    %% Sync command queue, so we don't choke the driver and get no events 
    _ = wxWindow:getSize(F),
    timer:sleep(10),
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
	#wx{id=What} when What >= ?NOISE_SHADER -> 
	    Active = What - ?NOISE_SHADER,
	    try 
		lists:nth(Active, options()),
		S#s{active=Active}
	    catch _:_ ->
		    S
	    end;
	{'_wxe_error_', _, _} -> 
	    %% Canvas is closed
	    quit;
	Other ->
	    io:format("Got ~p ~n",[Other]),
	    events(S)
    after 0 ->
	    S
    end.

options() ->
    [
     {classicnoise2D,  2, "float n = cnoise(v_texCoord2D.xy * 16.0 + sin(0.5 * time));" },
     {noise2D,         2, "float n = snoise(v_texCoord2D.xy * 16.0 + sin(0.5 * time));"},
     {classicnoise3D,  3, "float n = cnoise(vec3(4.0 * v_texCoord3D.xyz * (1.0 + sin(0.25 * time))));"},
     {noise3D,         3, "float n = snoise(vec3(2.0 * v_texCoord3D.xyz * (1.0 + sin(0.25 * time))));"},
     {classicnoise4D,  3, "float n = cnoise(vec4(8.0 * v_texCoord3D.xyz, 0.5 * time));"},
     {noise4D,         3, "float n = snoise(vec4(4.0 * v_texCoord3D.xyz, 0.5 * time));"}
    ].

init_shader(S) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, VsBin} = file:read_file(filename:join(Dir, "noise_test.vs")),

    Main0 = "
     uniform float time; // Used for texture animation
     /*
      * Both 2D and 3D texture coordinates are defined, for testing purposes.
      */
     varying vec2 v_texCoord2D;
     varying vec3 v_texCoord3D;
     varying vec4 v_color;

     void main( void )
     {
       ~s
       gl_FragColor = v_color * vec4(0.5 + 0.5 * vec3(n, n, n), 1.0);
     }
    ",
    
    Create = fun({Name, Type, Str}) ->
		     Main = io_lib:format(Main0, [Str]),
		     File = filename:join(Dir, atom_to_list(Name) ++ ".glsl"),
		     io:format("Reading file ~p~n",[File]),
		     {ok, FsBin0} = file:read_file(File),
		     FsBin = <<FsBin0/binary, (list_to_binary(Main))/binary>>,
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
		     {Name, Type, Prog}
	     end,
    S#s{shader=lists:map(Create, options())}.

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
    lists:foldl(fun({Name, _, _}, Id) ->
			wxMenu:append(OptsM, Id, atom_to_list(Name)),
			Id+1
		end, ?NOISE_SHADER+1, options()),
			
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
    Context = wxGLContext:new(Canvas),
    %% wxFrame:connect(Canvas, size),

    wxWindow:show(Frame),
    timer:sleep(500),
    %% Set Current must be called after show and before any opengl call.
    wxGLCanvas:setCurrent(Canvas, Context),

    gl:viewport(0,0,?W,?H),

    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_CULL_FACE),

    gl:shadeModel(?GL_SMOOTH),
    gl:clearColor(0.2,0.3,0.2,1.0),

    #s{frame=Frame, canvas=Canvas, time=#time{}}.


load_sphere(State = #s{shader=[{_,_,AProg}|_]}) ->
    {Size, Data, _} = sphere:tris([{subd,5}, {ccw,false}, {binary,true}, {scale,3}]),
    
    [Buff] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
    gl:bufferData(?GL_ARRAY_BUFFER, size(Data), Data, ?GL_STATIC_DRAW),
    
    gl:useProgram(AProg),
    %% Nothing to do with the example
%%    test_create_texture(1),
    test_create_texture(0, 8),
    test_create_texture(1, 16),
    test_create_texture(2, 32),
    test_create_texture(3, 64),

    Draw = fun(Time, {_,Type, Prog}) ->
		   gl:color3ub(150,70,70),
		   gl:useProgram(Prog),
		   %%ActivateGLSL(),
		   TimeLoc = gl:getUniformLocation(Prog, "time"),
		   gl:uniform1f(TimeLoc, Time/1000),
		   case Type of 
		       2 -> 
			   gl:'begin'(?GL_QUADS),
			   gl:texCoord2f(0,0), gl:vertex2i(0,0),
			   gl:texCoord2f(1,0), gl:vertex2i(?W,0),
			   gl:texCoord2f(1,1), gl:vertex2i(?W,?H),
			   gl:texCoord2f(0,1), gl:vertex2i(0,?H),
			   gl:'end'();
		       3 ->
			   %% Setup draw buffers
			   gl:bindBuffer(?GL_ARRAY_BUFFER,Buff),
			   gl:vertexPointer(3, ?GL_FLOAT, 0, 0),
			   gl:enableClientState(?GL_VERTEX_ARRAY),
			   
			   %% Draw buffer
			   gl:drawArrays(?GL_TRIANGLES, 0, Size*3),
		   
			   %% Disable these draw buffers
			   gl:bindBuffer(?GL_ARRAY_BUFFER,0),
			   gl:disableClientState(?GL_VERTEX_ARRAY)
		   end
	   end,
    State#s{draw=Draw}.

test_create_texture(Id0, Scale) ->
    H = W = 256,
    TexId = gl_noise:create_tex2D(W,H,Scale),
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),  %% Unnecessary?
    Mem = wx:create_memory(W*H*4),
    gl:bindTexture(?GL_TEXTURE_2D, TexId), 
    gl:getTexImage(?GL_TEXTURE_2D, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Mem),
    ImageBin0 = wx:get_memory_bin(Mem),
    ImageBin = << <<R:8, G:8, B:8>> || <<R:8, G:8, B:8, _A:8>> <= ImageBin0 >>,
    Image = wxImage:new(W,H,ImageBin),
    Id = Id0 div 2,
    Frame = wxFrame:new(wx:null(), ?wxID_ANY,
			"Noise: " ++ integer_to_list(Scale),
			[{size, {W*3+10, H*3+10}},
			 {pos, {(W*3+10)*Id*(Id rem 2),
				(H*3+40)*(Id0 rem 2)}}
			]),
    Panel = wxPanel:new(Frame),
    Paint = fun(_,_) ->
		    DC=wxPaintDC:new(Panel),
		    Bmp = wxBitmap:new(Image),
		    [ [ wxDC:drawBitmap(DC, Bmp, {5+X*W,5+Y*H}) || X <- [0,1,2] ]
                      || Y <- [0,1,2]],
		    wxPaintDC:destroy(DC),
		    wxBitmap:destroy(Bmp)
	    end,
    wxFrame:connect(Panel, paint, [{callback, Paint}]),
    wxFrame:show(Frame).

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
