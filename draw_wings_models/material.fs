//                   -*- mode:glsl; coding:iso-latin-1 -*-

varying float shininess, reflectY;
varying vec3  normal, lightVec, halfVec;

uniform float Time;
uniform sampler2D DiffuseTex;
uniform sampler2D HeavenTex;

void main(void) 
{       
    float mixer;
    vec3  n,halfV;
    float NdotL,NdotHV;
    
    n = normalize(normal);    
    halfV = normalize(halfVec);
    float spec = clamp(dot(halfV, n), 0.0, 1.0);
    vec4 specular = vec4(1.0, 0.941, 0.898,1.0) 
	* pow(spec, shininess);

    vec4 color = gl_Color;
    color *= texture2D(DiffuseTex, gl_TexCoord[0].st);
    vec4 skyColor = texture2D(HeavenTex, vec2(Time, reflectY));
    
    mixer = shininess / 128.0;
    if (reflectY > 0.0) 
       mixer = mixer * mixer;
    else 
       mixer = mixer * 0.1;
    
    gl_FragColor = specular + mix(color, skyColor, mixer);

}
