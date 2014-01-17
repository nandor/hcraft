//------------------------------------------------------------------------------
// HCraft
//------------------------------------------------------------------------------
#version 410
precision highp float;

layout (location = 0) out vec4 out_color;

uniform sampler2D u_random;
uniform sampler2D u_diffuse;
uniform usampler3D u_blocks;
uniform float u_time;

in vec4 v_pos;
in vec4 v_mpos;
in vec3 v_normal;
in vec2 v_st;

uint getBlock()
{
  vec3 grid;

  grid = v_mpos.zyx + 0.00001;
  grid -= max(vec3(0.0), ceil(v_normal.zyx));

  return texelFetch(u_blocks, ivec3(mod(grid + 16, 16)), 0).r;
}

void main( )
{
  uint i = getBlock();

  out_color = vec4(float(i) / 2.0);
}
