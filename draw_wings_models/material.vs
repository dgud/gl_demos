//                   -*- mode:glsl; coding:iso-latin-1 -*-
//

varying vec3 normal, lightVec, halfVec;
varying float shininess, reflectY;

void main(void) 
{
    vec3 viewVec;
    vec3 lightpos  = vec3(gl_LightSource[0].position);
    
    // Standard Lighting calcs
    vec3 ecPosition = vec3(gl_ModelViewMatrix * gl_Vertex);
    normal     = normalize(gl_NormalMatrix * gl_Normal);

    lightVec   = normalize(lightpos);
    viewVec    = normalize(-ecPosition);
    halfVec    = normalize(viewVec + lightVec);
    
    shininess = float(gl_FrontMaterial.shininess);
    float nDotLv = max(0.0,dot(normal,lightVec));
    
    reflectY = dot(reflect(normal, viewVec), 
		   vec3(gl_ModelViewMatrix * vec4(0.0,1.0,0.0,0.0)));

    gl_FrontColor  = gl_LightSource[0].ambient +
	gl_LightSource[0].diffuse  * nDotLv;    
    gl_TexCoord[0] = gl_MultiTexCoord0;
    gl_Position = ftransform();
}


