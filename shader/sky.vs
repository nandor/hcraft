//------------------------------------------------------------------------------
// HCraft
//------------------------------------------------------------------------------
#version 410

layout (location = 0) in vec3 in_pos;

uniform mat4 u_proj;
uniform mat4 u_view;

out vec3 v_vertex;

void main( )
{
  v_vertex = in_pos;
  gl_Position = u_proj * u_view * vec4( in_pos, 1.0 );
}
