//                   -*- mode:c; coding:iso-latin-1 -*-
// Brick fragment shader

varying vec3 Pos3D;
varying vec3 Norm;
varying vec2 UV;
varying float LightIntensity;

uniform sampler2DShadow ShadowMap;

const float Epsilon = -0.005;

float shadow(float x, float y)
{
    vec3 displace = vec3(x,y,Epsilon);
    float depth = shadow2D(ShadowMap,gl_TexCoord[1].xyz + displace).x;
    return depth != 1.0 ? 0.40 : 1.0;
}

void main(void) 
{
    // Brick pattern calc
    vec3 BrickColor  = vec3(0.8,0.3,0.3);
    vec3 MortarColor = vec3(0.7,0.7,0.7);
    vec3 BrickSize   = vec3(0.3,0.15,0.3);
    vec3 BrickPct    = vec3(0.025,0.0375,0.025);	    

    float temp = 1.0;
    vec3 color, normal, step = vec3(0.0,0.0,1.0);
    vec3 position, useBrick;
    vec4 limit = vec4(0.66,0.90,0.67, 0.005);
    position = Pos3D / BrickSize;
    
    normal = abs(normalize(Norm));
    if((fract(position.y *0.5) > 0.5))
	step = vec3(1.0,0.0,0.0);
    step = step * vec3(0.5,0.0,0.5);
    if(normal.y > limit.y)
    	step = vec3(0.0,0.0,0.0);

    position += step;
    
    position = fract(position);
    useBrick = smoothstep(BrickPct, BrickPct+0.03, position);
    useBrick -= smoothstep(1.0-(BrickPct+0.03),(1.0-BrickPct),position);
    useBrick = useBrick + smoothstep(vec3(limit),vec3(limit)+0.01,normal);
    useBrick = min(useBrick, 1.0); // clamp

    // Draw AA-mortar att Y-limit
    limit.x = abs(limit.y - normal.y);
    useBrick.y *= smoothstep(BrickPct.y*0.10,BrickPct.y*0.20, limit.x);
    
    color = mix(MortarColor, BrickColor, useBrick.x*useBrick.y*useBrick.z);
    // Ok we have the color multiply with light and shadow
    gl_FragColor = vec4(color * min(LightIntensity,shadow(0.0,0.0)), 1);
}

