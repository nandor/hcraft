//------------------------------------------------------------------------------
// HCraft
//------------------------------------------------------------------------------
#version 430

layout (location = 0) out vec4 out_color;

uniform sampler2D u_random;
uniform sampler2D u_diffuse;

in vec4 v_pos;
in vec3 v_normal;
in vec2 v_st;

void main( )
{
  out_color = vec4(v_st, 0.0, 0.0);
}
