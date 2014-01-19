//------------------------------------------------------------------------------
// HCraft
//------------------------------------------------------------------------------
#version 410
precision highp float;

layout (location = 0) out vec4 out_color;

uniform sampler2D u_random;
uniform sampler2D u_diffuse;
uniform usampler3D u_blocks;
uniform samplerCubeArray u_textures;
uniform float u_time;

in vec4 v_pos;
in vec4 v_mpos;
in vec3 v_normal;
in vec2 v_st;

void main( )
{
  vec3 grid;
  vec4 stpl;

  // Get the position of the block
  grid = v_mpos.zyx + 0.00001;
  grid -= max(vec3(0.0), ceil(v_normal.zyx));
  grid = floor(mod(grid + 16, 16));

  // The layer is read from the texture which specifies block types
  stpl.w = float(texelFetch(u_blocks, ivec3(grid), 0).r) - 1.0;
  stpl.zyx = normalize(v_mpos.zyx - grid - vec3(0.5));

  out_color = texture(u_textures, stpl);
}
