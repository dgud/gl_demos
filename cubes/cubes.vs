//                   -*- mode:glsl; coding:iso-latin-1 -*-
//
const float SpecCont = 0.3;
const float DiffCont = 1.0 - SpecCont;
const float PI   = 3.141592;
const float PI15 = 4.712388; // PI *1.5
varying float LightIntensity;
varying vec3 Pos3D;
varying vec3 Norm;

uniform vec3 BumpPosition;
uniform vec4 LightPosition; 

uniform mat4 lightMVmatrix;

void main(void) 
{
    vec4 pos        = gl_Vertex;
    vec3 dist       = vec3(pos); 
    vec4 ShadowCoord;

    mat4 scaleMat = mat4(0.5,0.0,0.0,0.0,
			 0.0,0.5,0.0,0.0,
			 0.0,0.0,0.5,0.0,
			 0.5,0.5,0.5,1.0);
    mat4 projMatrix;


    // Vertical displacement around the hole, 
    // for the top face vertices
    if (pos.y > 0.5) { 
	// Get the center
 	dist = sign(dist)*floor(abs(dist)+0.5);	
        dist.x = distance(dist, BumpPosition); 
	dist.x = min(dist.x, PI15);
	pos.y  += cos(dist.x + PI) * 0.65;
    }

    // Standard Lighting calcs
    vec4 ecPosition = gl_ModelViewMatrix * pos;
    vec3 tnorm      = normalize(gl_NormalMatrix * gl_Normal);
    vec3 lightvec   = vec3(gl_ModelViewMatrix * LightPosition);
    lightvec  = normalize(lightvec - vec3(ecPosition));
    vec3 reflVec    = reflect(-lightvec, tnorm);
    vec3 viewVec    = normalize(-vec3(ecPosition));
    float diffuse   = max(dot(lightvec,tnorm), 0.0);
    float spec      = 0.0;
    
    if (diffuse > 0.0) {
	spec = max(dot(reflVec, viewVec), 0.0);
	spec = pow(spec, 16.0);
    }
    LightIntensity = DiffCont * diffuse + SpecCont * spec + 0.30;

    // Shadows coord calc 
    ShadowCoord = (lightMVmatrix * pos); 
    gl_TexCoord[1] = ShadowCoord / ShadowCoord.w;
       
    Pos3D = pos.xyz;
    Norm = gl_Normal;
    gl_Position = gl_ModelViewProjectionMatrix * pos;
}


