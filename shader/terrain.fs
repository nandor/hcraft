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
uniform vec3 u_pos;

in vec4 v_vpos;
in vec4 v_mpos;
in vec3 v_normal;
in vec2 v_st;

void main( )
{
  // Get the position of the block
  vec3 grid = v_mpos.zyx + 0.00001;
  grid -= max(vec3(0.0), ceil(v_normal.zyx));
  grid = floor(mod(grid + 32, 32));

  // The layer is read from the texture which specifies block types
  vec4 stpl;
  stpl.w = float(texelFetch(u_blocks, ivec3(grid), 0).r) - 1.0;
  stpl.zyx = normalize(v_mpos.zyx - grid - vec3(0.5));

  // Shade the pixel
  vec3 lightDir = normalize(u_pos - v_vpos.xyz);
  float angle = max(0.0, dot(lightDir, normalize(v_normal)));

  out_color = texture(u_textures, stpl) * (0.2 + angle);
}
