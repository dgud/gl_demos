%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2010, Dan Gudmundsson
%%% @doc GLSL Perlin (simplex) noise.
%%%       This a port of Stefan Gustavson (stegu@itn.liu.se) GLSL-noise code
%%%       with some additions for creating textures and other tools. 
%%% @end
%%% Created :  2 Feb 2010 by Dan Gudmundsson <dgud@erlang.org>

-module(gl_noise).
-export([create_tex2D/3]).

%% -compile(export_all). %Debug
-include_lib("wx/include/gl.hrl").

%%--------------------------------------------------------------------
%% @doc  Create a perlin noise 2D texture.
%%   Creates a tileable 2Dtexture with the given dimensions Width and Heigth
%%   Starts with on Octave level of noise. 1 small noise ~ 32 very noise.
%%   Returns a texture id which should be used by gl:bindTexture(Id).
%% @spec (::integer(), ::integer(), ::integer(), ::fun()) -> TextureId::integer()
%% @end
%%--------------------------------------------------------------------
create_tex2D(W, H, Octave) 
  when W > 0, H > 0, Octave > 0 ->
    [Fbo] = gl:genFramebuffers(1),
    [Col] = gl:genTextures(1),
    [PX,PY,PW,PH|_] = gl:getIntegerv(?GL_VIEWPORT),
    setup_fbo(Fbo, Col, W, H),    

    Shader = setup_shader_2D(Octave, W, H),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    gl:useProgram(Shader),

    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0,0), gl:vertex2i(0,0),
    gl:texCoord2f(1.0,0), gl:vertex2i(W,0),
    gl:texCoord2f(1.0,1.0), gl:vertex2i(W,H),
    gl:texCoord2f(0,1.0), gl:vertex2i(0,H),
    gl:'end'(),

    gl:flush(),
    gl:useProgram(0),
    gl:bindFramebuffer(?GL_FRAMEBUFFER_EXT, 0),
    gl:deleteFramebuffers([Fbo]),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:popMatrix(),
    gl:viewport(PX,PY,PW,PH),
    Col.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Texture generation helpers

setup_fbo(Fbo, Col, W, H) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER_EXT, Fbo),
    gl:bindTexture(?GL_TEXTURE_2D, Col), 
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA8, W, H, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, 0),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameterf(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameterf(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),

    gl:framebufferTexture2D(?GL_FRAMEBUFFER_EXT, ?GL_COLOR_ATTACHMENT0_EXT,
			    ?GL_TEXTURE_2D, Col, 0),

    %% Assert
    case gl:checkFramebufferStatus(?GL_FRAMEBUFFER_EXT) of
	?GL_FRAMEBUFFER_COMPLETE_EXT -> ok;
	Error ->
	    gl:bindFramebuffer(?GL_FRAMEBUFFER_EXT, 0),
	    gl:deleteTextures([Col]),
	    gl:deleteFramebuffers([Fbo]),
	    exit({error, {frameBufferStatus, Error}})
    end,
    ok.

setup_shader_2D(Scale, W, H) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, FsBin0} = file:read_file(filename:join(Dir, "classicnoise2D.glsl")),
    Main = list_to_binary(io_lib:format(create_tex2d_str(), [W, H,Scale])),
    FsBin = <<FsBin0/binary, Main/binary>>,
    VS = gl:createShader(?GL_VERTEX_SHADER),
    FS = gl:createShader(?GL_FRAGMENT_SHADER),
    gl:shaderSource(FS, [FsBin]),
    gl:shaderSource(VS, [vertex_shader()]),
    gl:compileShader(FS),
    gl:compileShader(VS),
    Print = fun(Sh) ->
		    case gl:getShaderiv(Sh, ?GL_INFO_LOG_LENGTH) of
			Len when Len > 1 ->
			    Log = gl:getShaderInfoLog(Sh, Len),
			    io:format("  Message: ~s~n",[Log]);
			_ ->
			    ok
		    end
	    end,
    Print(FS), Print(VS),

    Prog = gl:createProgram(),
    gl:attachShader(Prog, FS),
    gl:attachShader(Prog, VS),
    gl:linkProgram(Prog),
    case gl:getProgramiv(Prog, ?GL_INFO_LOG_LENGTH) of	
	Length when Length > 1 ->
	    Info = gl:getProgramInfoLog(Prog,Length),
	    io:format("  ~s: ~s~n",["GLSLPerlin", Info]);
	_ ->  ok
    end,
    Prog.

create_tex2d_str() ->
    "
varying vec2 texCoord2D;

void main( void )\n
{\n

 float w = ~p.0, h = ~p.0;
 vec2 pos = vec2(texCoord2D.x*w, texCoord2D.y*h);
 float o = ~p.0;
 float div = 1.0/o;
 float res = pnoise(pos*div, vec2(w/o, h/o));

 gl_FragColor = vec4(0.5+0.5*vec3(res, res, res), 1.0);

}".

vertex_shader() ->
    "
varying vec2 texCoord2D;
void main(void) 
{
  texCoord2D = gl_MultiTexCoord0.xy;
  gl_Position = ftransform();
}
".
