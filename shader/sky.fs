//------------------------------------------------------------------------------
// HCraft
//------------------------------------------------------------------------------
#version 430

layout (location = 0) out vec4 out_color;

uniform samplerCube u_diffuse;
in vec3 v_vertex;

void main( )
{
  out_color = texture(u_diffuse, v_vertex);
}
