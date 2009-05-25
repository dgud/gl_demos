%%%-------------------------------------------------------------------
%%% File    : cubes.erl
%%% Author  : Dan Gudmundsson
%%% Description : Playing around  (testing shadows and shaders)
%%%
%%% Created : 30 Oct 2008 by 
%%%-------------------------------------------------------------------
-module(cubes).
-compile(export_all).

-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 

-define(error(), error(?LINE)).
-define(F32, 32/native-float).
-define(GSz, 10).  
-define(TEXTURE_SIZE, 512).

-define(FPS, 10000). %% Frame time in micro-sec.
-record(ds, {t0,   % Start time
	     tn,   % Prev frame
	     td=0, % Sleep to next frame	     
	     fc, 
	     light_pos = {-15,10,30},
	     depth,
	     shader,
	     bpi, canvas, data}).

-define(VS, {{ 0.4,  1.0, -0.4},  %1
	     { 0.4,  0.0, -0.4},  %2
	     {-0.4,  0.0, -0.4},   
	     {-0.4,  1.0, -0.4},  %4
	     {-0.4,  1.0,  0.4},
	     { 0.4,  1.0,  0.4},  %6
	     { 0.4,  0.0,  0.4}, 
	     {-0.4,  0.0,  0.4}}).%8

-define(FACES, 
	%% Faces    Normal     U-axis   V-axis 
	[{[1,2,3,4],{0,0,-1},{-1,0,0}, {0,1,0}},  % 
	 {[3,8,5,4],{-1,0,0},{0,0,1},  {0,1,0}},  %
	 {[1,6,7,2],{1,0,0}, {0,0,-1}, {0,1,0}},  %
	 {[6,5,8,7],{0,0,1}, {1,0,0},  {0,1,0}},  %
	 {[6,1,4,5],{0,1,0}, {-1,0,0}, {0,0,1}},  %
	 {[7,8,3,2],{0,-1,0},{1,0,0},  {0,0,1}}]).

start() ->
    init().

init() ->
    WX = wx:new(),
    Frame   = wxFrame:new(WX,1,"Dancing cubes",[{size, {800,600}}]),
    ok = wxFrame:connect(Frame, close_window),
    wxFrame:createStatusBar(Frame,[]),
    setup_menus(Frame),
    GLAttrs = [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0],
    Canvas = wxGLCanvas:new(Frame, [{attribList, GLAttrs}]),
    wxWindow:connect(Canvas, motion),
    Data = setup_vs(),
    wxWindow:show(Frame),   %% Must show to initilize context.
    wxGLCanvas:setCurrent(Canvas),
    State0 = initGL(Canvas,Data),
    loop(Frame, State0),
    wx:destroy().

loop(Frame,State = #ds{td=Rem, fc=FC}) ->
    receive 
	#wx{event=#wxClose{}} ->	    
	    io:format("~p Destroying window ~n",[self()]),
	    %%wxWindow:close(Frame,[]),
	    State;
	#wx{id=11} ->
	    wxWindow:close(Frame,[]),
	    io:format("~p Closing window ~n",[self()]),
	    State;
	#wx{event=#wxMouse{type=motion,x=X,y=Y}} ->
	    Str = lists:flatten(io_lib:format("Mouse {~p,~p}", [X,Y])),
	    wxFrame:setStatusText(Frame, Str,[]),
	    loop(Frame,calc_rem(State));
	Wx = #wx{} ->
	    io:format("~p Received wx record ~p~n",[self(), Wx]),
	    loop(Frame,calc_rem(State));
	Kalle ->
	    io:format("Received ~p~n",[Kalle]),
	    loop(Frame,calc_rem(State))
    after Rem ->
	    New = drawCanvas(State),
	    case FC rem 100 of
		0 ->
		    Ms = timer:now_diff(now(), State#ds.t0) div 1000,
		    Str = lists:flatten(io_lib:format(" FPS: ~p Sleep: ~p ms", 
						      [1000*FC div Ms, Rem])),
		    wxFrame:setStatusText(Frame, Str,[]);
		_ ->
		    ok
	    end,
	    ?error(),
	    loop(Frame,calc_rem(New))
    end.
 
error(Line) ->
    case gl:getError() of
	0 -> ok;
	Err -> 
	    %% io:format("~p:~p ~n",[Line,glu:errorString(Err)])
	    io:format("~p:~p (~.16X) ~n",[Line,Err,Err,"16#"])
    end.

setup_menus(Frame) ->
    MenuBar = wxMenuBar:new(),
    Menu    = wxMenu:new([]),
    true = wxMenuBar:append(MenuBar, Menu, "&File"),
    wxMenu:append(Menu, 10, "&About", []),
    wxMenu:append(Menu, 11, "Exit", []),
    
    ok = wxFrame:connect(Frame, command_menu_selected), 
    ok = wxFrame:setMenuBar(Frame,MenuBar).

initGL(Canvas, Data = {Vs,Ns}) ->
    {W,H} = wxWindow:getClientSize(Canvas),
    ?error(),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:perspective(30, W/H, 15.0, 80.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),  
    glu:lookAt(15,15,15, 0,0,0, 0,1,0),

    gl:hint(?GL_POINT_SMOOTH_HINT, ?GL_NICEST),   gl:enable(?GL_POINT_SMOOTH), 
    gl:hint(?GL_LINE_SMOOTH_HINT, ?GL_NICEST),    gl:enable(?GL_LINE_SMOOTH), 
    gl:hint(?GL_POLYGON_SMOOTH_HINT, ?GL_NICEST), gl:enable(?GL_POLYGON_SMOOTH), 
    gl:enable(?GL_DEPTH_TEST),
    gl:depthFunc(?GL_LESS),
    gl:clearColor(0.5,0.0,0.5,1.0),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    gl:vertexPointer(3, ?GL_FLOAT, 0, Vs),
    gl:enableClientState(?GL_NORMAL_ARRAY),
    gl:normalPointer(?GL_FLOAT, 0, Ns),
    gl:shadeModel(?GL_SMOOTH),

    Prog = init_shaders(true),
    BumpPosI   = gl:getUniformLocation(Prog, "BumpPosition"),
    LightPosI  = gl:getUniformLocation(Prog, "LightPosition"),
    LightMatI  = gl:getUniformLocation(Prog, "lightMVmatrix"),
    ShadowMapI = gl:getUniformLocation(Prog, "ShadowMap"),

    DepthBuffs  = setup_fbo(),
    ?error(),
    Now = now(),
    #ds{t0=Now, tn=Now, td=?FPS, fc=0, 
	bpi    = {BumpPosI,LightPosI,LightMatI,ShadowMapI}, 
	depth  = DepthBuffs, shader = Prog,
	canvas = Canvas, data = Data}.
    
drawCanvas(#ds{t0=T0,fc=Fc, light_pos={LX,LY,LZ}, 
	       depth = {DepthFBO, DepthTex, _ColorTex},
	       shader = _Prog,
	       canvas=Canvas, bpi={BPI,LPI,LMI,SMI}
	      } = State) ->
    %% setup where the hole is
    {X,Z} = circle(timer:now_diff(now(),T0) div 1000),
    gl:uniform3f(BPI,X,1.0,Z),
    %% Light Position
    gl:uniform4f(LPI,LX,LY,LZ,1.0),
    begin     %% 1st pass draw from light_pos to depth buffer
  	gl:bindFramebuffer(?GL_FRAMEBUFFER, DepthFBO),
	gl:viewport(0,0,?TEXTURE_SIZE,?TEXTURE_SIZE),
	gl:clear(?GL_DEPTH_BUFFER_BIT),
	gl:matrixMode(?GL_PROJECTION),
	gl:pushMatrix(),
 	gl:loadIdentity(),	
 	glu:perspective(60, 1.0, 15.0, 80.0),
	gl:matrixMode(?GL_MODELVIEW),
	gl:pushMatrix(),
	gl:loadIdentity(),  
	glu:lookAt(LX,LY,LZ, 0,0,0, 0,1,0),
	LightMVMat   = gl:getFloatv(?GL_MODELVIEW_MATRIX),
	LightProjMat = gl:getFloatv(?GL_PROJECTION_MATRIX),	
	gl:drawBuffer(?GL_NONE),
	gl:readBuffer(?GL_NONE),
	gl:drawArrays(?GL_QUADS, 0, 24*(2*?GSz+1)*(2*?GSz+1)),
 	gl:bindFramebuffer(?GL_FRAMEBUFFER, 0), %% Deactivate
	gl:popMatrix(),
	gl:matrixMode(?GL_PROJECTION),
	gl:popMatrix(),
	{W,H} = wxWindow:getClientSize(Canvas),
	gl:viewport(0,0,W,H)
    end,
    %% Second Pass 
    %% Draw it and use the depthTex we have filled with data
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    %% Send down the Light Matrix
    SPMV  = mul([scale(),list_to_tuple(LightProjMat),list_to_tuple(LightMVMat)]),
    gl:uniformMatrix4fv(LMI,0,[SPMV]),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, DepthTex),
    gl:uniform1i(SMI, 0),  %% Setup ShaderMap is on textureunit 0
    gl:drawArrays(?GL_QUADS, 0, 24*(2*?GSz+1)*(2*?GSz+1)),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D),
    
    ?error(),    
    wxGLCanvas:swapBuffers(Canvas),
    %% Sync command queue, so we don't choke the driver and get no events 
    _ = wxWindow:getSize(Canvas),

    State#ds{fc=Fc+1}.

circle(T0) ->
    T = T0 / (500*math:pi()),
    R = ?GSz div 2,
    {R * math:sin(T), R * math:cos(T)}.

calc_rem(#ds{tn=T0} = State)->
    T1 = now(),
    Tdiff = timer:now_diff(T1, T0),
    Sleep = ?FPS - Tdiff,
    case Sleep =< 1000 of
	true -> 
	    State#ds{tn=T1,td=0};
	false ->
	    State#ds{tn=T1,td=Sleep div 1000}
    end.
    
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_vs() ->
    setup_vs(-?GSz, -?GSz, <<>>, <<>>).

setup_vs(GX, GY, VsA, NA) when GX =< ?GSz ->
    VPos = fun(Vertex,PX,PY) ->
		   {X,Y,Z} = element(Vertex, ?VS),
		   <<(PX+X):?F32,Y:?F32,(PY+Z):?F32>>
	   end,
    Cube = 
	<< %% For all faces in the cube       
	 << (<< %% For all vertices in the face
	      << (VPos(Id,GX,GY)):12/binary >> || Id <- Vs >>):48/binary >> 
	 || {Vs,_,_,_} <- ?FACES >>,
    Ns = << %% For all faces in the cube  
	  << 
	   (<< %% For all vertices in the face
	     <<X:?F32,Y:?F32,Z:?F32>> 
	     || _ <- Vs >>):48/binary >>
	  || {Vs,{X,Y,Z},_,_} <- ?FACES >>,
    setup_vs(GX+1,GY, 
	     <<VsA/binary,Cube:288/binary>>, 
	     <<NA/binary, Ns:288/binary>>);

setup_vs(_GX, GY, Vs, Ns) when GY < ?GSz ->
    setup_vs(-?GSz, GY+1, Vs, Ns);
setup_vs(_, _, Vs, Ns) -> 
    {Vs,Ns}.
    
init_shaders(true) ->
    {ok, VsBin} = file:read_file("cubes.vs"),
    {ok, FsBin} = file:read_file("cubes.fs"),

    VS = gl:createShader(?GL_VERTEX_SHADER),
    FS = gl:createShader(?GL_FRAGMENT_SHADER),
    
    gl:shaderSource(VS, [VsBin]),
    gl:shaderSource(FS, [FsBin]),
    
    gl:compileShader(VS),
    gl:compileShader(FS),
    printShaderInfoLog(VS),
    printShaderInfoLog(FS),
    
    Prog = gl:createProgram(),
    gl:attachShader(Prog, VS),
    gl:attachShader(Prog, FS),

    gl:linkProgram(Prog),

    Length = gl:getProgramiv(Prog, ?GL_INFO_LOG_LENGTH),
    io:format("  ~s~n",[gl:getProgramInfoLog(Prog, Length)]),
    gl:useProgram(Prog),
    Prog.

printShaderInfoLog(Shader) ->
    Length  = gl:getShaderiv(Shader, ?GL_INFO_LOG_LENGTH),
    case Length > 0 of
	true ->
	    Log = gl:getShaderInfoLog(Shader, Length),
	    io:format("  ~s~n",[Log]);
	false ->
	    io:format("Something strange ~p~n",[Length])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Framebuffer object setup
%%%%%%%%% FBO stuff for shadows
setup_fbo() ->    
    [Depth] = gl:genTextures(1),    
    gl:bindTexture(?GL_TEXTURE_2D, Depth),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_DEPTH_COMPONENT24_ARB,
		  ?TEXTURE_SIZE, ?TEXTURE_SIZE, 0,
		  ?GL_DEPTH_COMPONENT, ?GL_UNSIGNED_INT,
		  0),    
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T, ?GL_CLAMP),    
    gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_MODE,
		     ?GL_COMPARE_R_TO_TEXTURE),
    gl:texParameteri(?GL_TEXTURE_2D, 
		     ?GL_TEXTURE_COMPARE_FUNC, ?GL_LEQUAL),    
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    
    [FB] = gl:genFramebuffers(1),
    gl:bindFramebuffer(?GL_FRAMEBUFFER, FB),
    
    gl:framebufferTexture2D(?GL_FRAMEBUFFER,
			    ?GL_DEPTH_ATTACHMENT,  
			    ?GL_TEXTURE_2D, Depth, 0),    
    gl:bindFramebuffer(?GL_FRAMEBUFFER, 0),
    check_fbo_status(FB),
    {FB,Depth, 0}. 

check_fbo_status(FB) ->
    case gl:checkFramebufferStatus(?GL_FRAMEBUFFER) of
	?GL_FRAMEBUFFER_COMPLETE ->
	    FB;
	?GL_FRAMEBUFFER_UNSUPPORTED ->
	    io:format("GL_FRAMEBUFFER_UNSUPPORTED~n",[]),
	    false;
	?GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER~n",[]),
	    false
    end.

mul([A,B|List]) ->
    mul([mul(A,B)|List]);
mul([Mat]) -> Mat.    

mul({B_a,B_b,B_c,B_d,B_e,B_f,B_g,B_h,B_i,B_j,B_k,B_l,B_tx,B_ty,B_tz,B_w},
    {A_a,A_b,A_c,A_d,A_e,A_f,A_g,A_h,A_i,A_j,A_k,A_l,A_tx,A_ty,A_tz,A_w})
  when is_float(A_a), is_float(A_b), is_float(A_c), is_float(A_d),
       is_float(A_e), is_float(A_f), is_float(A_g), is_float(A_h),
       is_float(A_i), is_float(A_j), is_float(A_k), is_float(A_l),
       is_float(A_tx),is_float(A_ty), is_float(A_tz), is_float(A_w) ->
    {A_a*B_a + A_b*B_e + A_c*B_i + A_d*B_tx,
     A_a*B_b + A_b*B_f + A_c*B_j + A_d*B_ty,
     A_a*B_c + A_b*B_g + A_c*B_k + A_d*B_tz,
     A_a*B_d + A_b*B_h + A_c*B_l + A_d*B_w,

     A_e*B_a + A_f*B_e + A_g*B_i + A_h*B_tx,
     A_e*B_b + A_f*B_f + A_g*B_j + A_h*B_ty,
     A_e*B_c + A_f*B_g + A_g*B_k + A_h*B_tz,
     A_e*B_d + A_f*B_h + A_g*B_l + A_h*B_w,

     A_i*B_a + A_j*B_e + A_k*B_i + A_l*B_tx,
     A_i*B_b + A_j*B_f + A_k*B_j + A_l*B_ty,
     A_i*B_c + A_j*B_g + A_k*B_k + A_l*B_tz,
     A_i*B_d + A_j*B_h + A_k*B_l + A_l*B_w,

     A_tx*B_a + A_ty*B_e + A_tz*B_i + A_w*B_tx,
     A_tx*B_b + A_ty*B_f + A_tz*B_j + A_w*B_ty,
     A_tx*B_c + A_ty*B_g + A_tz*B_k + A_w*B_tz,
     A_tx*B_d + A_ty*B_h + A_tz*B_l + A_w*B_w};
mul({A,B,C,Q0,D,E,F,Q1,G,H,I,Q2,Tx,Ty,Tz,Q3}, {X,Y,Z,W})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz),
       is_float(Q0), is_float(Q1), is_float(Q2), is_float(Q3),
       is_float(X), is_float(Y), is_float(Z) ->
    {X*A + Y*D + Z*G + W*Tx,
     X*B + Y*E + Z*H + W*Ty,
     X*C + Y*F + Z*I + W*Tz,
     X*Q0 + Y*Q1 + Z*Q2 + W*Q3}.

scale() ->
    {0.5,0.0,0.0,0.0,
     0.0,0.5,0.0,0.0,
     0.0,0.0,0.5,0.0,
     0.5,0.5,0.5,1.0}.

test_mat(Proj, MV) ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadMatrixf(scale()),
    gl:multMatrixf(Proj),
    printMatrix("GL S*P",gl:getFloatv(?GL_MODELVIEW_MATRIX)),
    gl:multMatrixf(MV),
    printMatrix("GL S*P*MV",gl:getFloatv(?GL_MODELVIEW_MATRIX)),
    gl:popMatrix(),    

    printMatrix("My S*P", mul(scale(), Proj)),
    printMatrix("My S*P*MV", mul(mul(scale(), Proj), MV)),

    printMatrix("T1 S*P*MV", mul(scale(), mul(Proj, MV))),
    printMatrix("T2 S*P*MV", mul([scale(), Proj, MV])),
    ok.

printMatrix(Text, Mat) ->
    io:format("~p: ~n", [Text]),
    printMatrix(Mat),
    io:format("~n").

printMatrix([A,B,C,D|Mat]) ->
    io:format("  ~6.2f ~6.2f ~6.2f ~6.2f~n", [A,B,C,D]),
    printMatrix(Mat);
printMatrix([]) -> ok;
printMatrix(Tuple) when is_tuple(Tuple) ->
    printMatrix(tuple_to_list(Tuple)).


