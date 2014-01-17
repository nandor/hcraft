//------------------------------------------------------------------------------
// HCraft
//------------------------------------------------------------------------------
#version 410

layout (location = 0) in vec3 in_pos;
layout (location = 1) in vec3 in_normal;
layout (location = 2) in vec2 in_st;

uniform mat4 u_proj;
uniform mat4 u_view;
uniform mat4 u_mdl;

out vec4 v_mpos;
out vec4 v_vpos;
out vec3 v_normal;
out vec2 v_st;

void main( )
{
  v_mpos = vec4(in_pos, 1.0);
  v_vpos = u_mdl * v_mpos;
  v_normal = in_normal;
  v_st = in_st;

  gl_Position = u_proj * u_view * v_vpos;
}
