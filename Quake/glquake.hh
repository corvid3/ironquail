/*
Copyright (C) 1996-2001 Id Software, Inc.
Copyright (C) 2002-2009 John Fitzgibbons and others
Copyright (C) 2007-2008 Kristian Duske
Copyright (C) 2010-2014 QuakeSpasm developers

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifndef GLQUAKE_H
#define GLQUAKE_H

#include "client.hh"
#include "gl_model.hh"
#include "gl_texmgr.hh"
#include "render.hh"

void
GL_BeginRendering(int* x, int* y, int* width, int* height);
void
GL_EndRendering(void);
void
GL_Set2D(void);

extern int glx, gly, glwidth, glheight;

// r_local.h -- private refresh defs

#define ALIAS_BASE_SIZE_RATIO (1.0 / 11.0)
// normalizing factor so player model works out to about
//  1 pixel per triangle
#define MAX_LBM_HEIGHT 480

#define BACKFACE_EPSILON 0.01

void
R_TimeRefresh_f(void);
void
R_ReadPointFile_f(void);
texture_t*
R_TextureAnimation(texture_t* base, int frame);

typedef enum
{
  pt_static,
  pt_grav,
  pt_slowgrav,
  pt_fire,
  pt_explode,
  pt_explode2,
  pt_blob,
  pt_blob2
} ptype_t;

// !!! if this is changed, it must be changed in d_ifacea.h too !!!
typedef struct particle_s
{
  // driver-usable fields
  vec3_t org;
  byte color;
  // drivers never touch the following fields
  byte type;
  float spawn;
  float die;
  vec3_t vel;
  float ramp;
} particle_t;

//====================================================

extern int r_visframecount; // ??? what difs?
extern int r_framecount;
extern mplane_t frustum[4];

extern qboolean use_simd;

//
// view origin
//
extern vec3_t vup;
extern vec3_t vpn;
extern vec3_t vright;
extern vec3_t r_origin;

//
// screen size info
//
extern refdef_t r_refdef;
extern mleaf_t *r_viewleaf, *r_oldviewleaf;
extern int d_lightstylevalue[256]; // 8.8 fraction of base light value

extern cvar_t r_norefresh;
extern cvar_t r_drawentities;
extern cvar_t r_drawworld;
extern cvar_t r_drawviewmodel;
extern cvar_t r_speeds;
extern cvar_t r_pos;
extern cvar_t r_waterwarp;
extern cvar_t r_fullbright;
extern cvar_t r_lightmap;
extern cvar_t r_wateralpha;
extern cvar_t r_lavaalpha;
extern cvar_t r_telealpha;
extern cvar_t r_slimealpha;
extern cvar_t r_litwater;
extern cvar_t r_dynamic;
extern cvar_t r_novis;
extern cvar_t r_scale;

extern cvar_t gl_clear;
extern cvar_t gl_polyblend;
extern cvar_t gl_nocolors;
extern cvar_t gl_finish;

extern cvar_t gl_playermip;

extern int gl_stencilbits;
extern qboolean gl_buffer_storage_able;
extern qboolean gl_multi_bind_able;
extern qboolean gl_bindless_able;
extern qboolean gl_clipcontrol_able;

extern const char* gl_vendor;
extern const char* gl_renderer;
extern const char* gl_version;

//==============================================================================

void
GL_BeginGroup(const char* name);
void
GL_EndGroup(void);

//==============================================================================

// Note: in order to simplify state management we impose a few restrictions:
// - if a shader uses N vertex attributes their indices must be [0..N-1]
// - instanced vertex attributes, if any, must come first

// Total number of vertex attributes used (instanced + non-instanced)
#define GLS_ATTRIBS(n) ((n) << GLS_ATTRIBS_SHIFT)

// Number of instanced vertex attributes used
#define GLS_INSTANCED_ATTRIBS(n) ((n) << GLS_INSTANCED_ATTRIBS_SHIFT)

typedef enum
{
  GLS_NO_ZTEST = 1 << 0,
  GLS_NO_ZWRITE = 1 << 1,

  GLS_BLEND_OPAQUE = 0 << 2,
  GLS_BLEND_ALPHA = 1 << 2,
  GLS_BLEND_ALPHA_OIT = 2 << 2,
  GLS_BLEND_MULTIPLY = 3 << 2,
  GLS_MASK_BLEND = 3 << 2,

  GLS_CULL_BACK = 0 << 4,
  GLS_CULL_NONE = 1 << 4,
  GLS_CULL_FRONT = 2 << 4,
  GLS_MASK_CULL = 3 << 4,

  GLS_ATTRIBS_BITS = 3,
  GLS_ATTRIBS_SHIFT = 6,
  GLS_ATTRIBS_MAXCOUNT = (1 << GLS_ATTRIBS_BITS) - 1,
  GLS_MASK_ATTRIBS = GLS_ATTRIBS_MAXCOUNT << GLS_ATTRIBS_SHIFT,

  GLS_INSTANCED_ATTRIBS_SHIFT = GLS_ATTRIBS_SHIFT + GLS_ATTRIBS_BITS,
  GLS_MASK_INSTANCED_ATTRIBS = GLS_ATTRIBS_MAXCOUNT
                               << GLS_INSTANCED_ATTRIBS_SHIFT,

  GLS_DEFAULT_STATE = GLS_BLEND_OPAQUE | GLS_CULL_BACK | GLS_ATTRIBS(1),
} glstatebits_t;

extern unsigned glstate;
void
GL_SetState(unsigned mask);
void
GL_ResetState(void);

extern GLint ssbo_align; // SSBO alignment - 1
extern GLint ubo_align;  // UBO alignment - 1

// johnfitz -- anisotropic filtering
#define GL_TEXTURE_MAX_ANISOTROPY_EXT 0x84FE
#define GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT 0x84FF
extern float gl_max_anisotropy;
extern qboolean gl_anisotropy_able;

// johnfitz -- polygon offset
#define OFFSET_BMODEL 1
#define OFFSET_NONE 0
#define OFFSET_DECAL -1
#define OFFSET_FOG -2
#define OFFSET_SHOWTRIS -3
void
GL_PolygonOffset(int);

typedef enum
{
  ZRANGE_FULL,
  ZRANGE_VIEWMODEL,
  ZRANGE_NEAR,
} zrange_t;
void
GL_DepthRange(zrange_t range);

// johnfitz -- rendering statistics
extern int rs_brushpolys, rs_aliaspolys, rs_skypolys;
extern int rs_dynamiclightmaps, rs_brushpasses, rs_aliaspasses, rs_skypasses;

// johnfitz -- track developer statistics that vary every frame
extern cvar_t devstats;
typedef struct
{
  int packetsize;
  int edicts;
  int visedicts;
  int efrags;
  int tempents;
  int beams;
  int dlights;
  int gpu_upload;
} devstats_t;
extern devstats_t dev_stats, dev_peakstats;

// ohnfitz -- reduce overflow warning spam
typedef struct
{
  double packetsize;
  double efrags;
  double beams;
  double varstring;
} overflowtimes_t;
extern overflowtimes_t
  dev_overflows; // this stores the last time overflow messages were displayed,
                 // not the last time overflows occured
#define CONSOLE_RESPAM_TIME 3 // seconds between repeated warning messages

// johnfitz -- moved here from r_brush.c
extern int gl_lightmap_format, lightmap_bytes;

#define LMBLOCK_WIDTH                                                          \
  256 // FIXME: make dynamic. if we have a decent card there's no real reason
      // not to use 4k or 16k (assuming there's no lightstyles/dynamics that
      // need uploading...)
#define LMBLOCK_HEIGHT                                                         \
  256 // Alternatively, use texture arrays, which would avoid the need to switch
      // textures as often.

typedef struct lightmap_s
{
  int xofs;
  int yofs;
} lightmap_t;
extern lightmap_t* lightmaps;
extern int lightmap_count; // allocated lightmaps

extern qboolean r_fullbright_cheatsafe, r_lightmap_cheatsafe,
  r_drawworld_cheatsafe; // johnfitz

extern float map_wateralpha, map_lavaalpha, map_telealpha,
  map_slimealpha; // ericw
extern float
  map_fallbackalpha; // spike -- because we might want r_wateralpha to apply to
                     // teleporters while water itself wasn't watervised

#define NUMVERTEXNORMALS 162
extern const float r_avertexnormals[NUMVERTEXNORMALS][3];

// johnfitz -- fog functions called from outside gl_fog.c
void
Fog_ParseServerMessage(void);
float*
Fog_GetColor(void);
float
Fog_GetDensity(void);
void
Fog_EnableGFog(void);
void
Fog_DisableGFog(void);
void
Fog_SetupFrame(void);
void
Fog_NewMap(void);
void
Fog_Init(void);

extern float r_matview[16];
extern float r_matproj[16];
extern float r_matviewproj[16];

void
R_NewGame(void);

#define LIGHT_TILES_X 32
#define LIGHT_TILES_Y 16
#define LIGHT_TILES_Z 32

typedef struct gpulight_s
{
  float pos[3];
  float radius;
  float color[3];
  float minlight;
} gpulight_t;

typedef struct gpulightbuffer_s
{
  float lightstyles[MAX_LIGHTSTYLES];
  gpulight_t lights[MAX_DLIGHTS];
} gpulightbuffer_t;

typedef struct gpuframedata_s
{
  float viewproj[16];
  float fogdata[4];
  float skyfogdata[4];
  vec3_t winddir;
  float windphase;
  vec3_t eyepos;
  float time;
  float zlogscale;
  float zlogbias;
  int numlights;
  int padding;
} gpuframedata_t;

extern gpulightbuffer_t r_lightbuffer;
extern gpuframedata_t r_framedata;

void
R_AnimateLight(void);
void
R_MarkSurfaces(void);
qboolean
R_CullBox(vec3_t emins, vec3_t emaxs);
qboolean
R_CullModelForEntity(entity_t* e);
void
R_EntityMatrix(float matrix[16],
               vec3_t origin,
               vec3_t angles,
               unsigned char scale);

void
R_InitParticles(void);
void
R_DrawParticles(qboolean alpha);
void
R_DrawParticles_ShowTris(void);
void
CL_RunParticles(void);
void
R_ClearParticles(void);

void
R_TranslatePlayerSkin(int playernum);
void
R_TranslateNewPlayerSkin(int playernum); // johnfitz -- this handles cases when
                                         // the actual texture changes

void
R_UploadFrameData(void);

void
R_DrawBrushModels(entity_t** ents, int count);
void
R_DrawBrushModels_Water(entity_t** ents, int count, qboolean translucent);
void
R_DrawBrushModels_SkyLayers(entity_t** ents, int count);
void
R_DrawBrushModels_SkyCubemap(entity_t** ents, int count);
void
R_DrawBrushModels_SkyStencil(entity_t** ents, int count);
void
R_DrawAliasModels(entity_t** ents, int count);
void
R_DrawSpriteModels(entity_t** ents, int count);
void
R_DrawBrushModels_ShowTris(entity_t** ents, int count);
void
R_DrawAliasModels_ShowTris(entity_t** ents, int count);
void
R_DrawSpriteModels_ShowTris(entity_t** ents, int count);

entity_t**
R_GetVisEntities(modtype_t type, qboolean translucent, int* outcount);

void
R_ClearBoundingBoxes(void);

#define MAX_BMODEL_DRAWS 4096
#define MAX_BMODEL_INSTANCES 1024

typedef struct bmodel_draw_indirect_s
{
  GLuint count;
  GLuint instanceCount;
  GLuint firstIndex;
  GLuint baseVertex;
  GLuint baseInstance;
} bmodel_draw_indirect_t;

typedef struct bmodel_gpu_leaf_s
{
  vec3_t mins;
  GLuint firstsurf;
  vec3_t maxs;
  GLuint surfcountsky; // bit 0=sky; bits 1..31=surfcount
} bmodel_gpu_leaf_t;

typedef struct bmodel_gpu_surf_s
{
  vec4_t plane;
  GLuint framecount;
  GLuint texnum;
  GLuint numedges;
  GLuint firstvert;
} bmodel_gpu_surf_t;

void
GL_BuildLightmaps(void);

void
GL_DeleteBModelBuffers(void);
void
GL_BuildBModelVertexBuffer(void);
void
GL_BuildBModelMarkBuffers(void);
void
GLMesh_LoadVertexBuffer(qmodel_t* m, aliashdr_t* hdr);
void
GLMesh_LoadVertexBuffers(void);
void
GLMesh_DeleteVertexBuffers(void);

int
R_LightPoint(vec3_t p, float ofs, lightcache_t* cache);

#define WORLDSHADER_SOLID 0
#define WORLDSHADER_ALPHATEST 1
#define WORLDSHADER_WATER 2

#define ALIASSHADER_STANDARD 0
#define ALIASSHADER_DITHER 1
#define ALIASSHADER_NOPERSP 2

typedef struct glprogs_s
{
  /* 2d */
  GLuint gui;
  GLuint viewblend;
  GLuint warpscale[2];   // [warp]
  GLuint postprocess[3]; // [palettize:off/dithered/direct]
  GLuint oit_resolve[2]; // [msaa]

  /* 3d */
  GLuint world[2][3][3]; // [OIT][dither][mode:solid/alpha test/water]
  GLuint water[2][2];    // [OIT][dither]
  GLuint skystencil;
  GLuint skylayers[2];      // [dither]
  GLuint skycubemap[2][2];  // [anim][dither]
  GLuint skyboxside[2];     // [dither]
  GLuint alias[2][3][2][2]; // [OIT][mode:standard/dithered/noperspective][alpha
                            // test][md5]
  GLuint sprites[2];        // [dither]
  GLuint particles[2][2];   // [OIT][dither]
  GLuint debug3d;

  /* compute */
  GLuint clear_indirect;
  GLuint gather_indirect;
  GLuint cull_mark;
  GLuint cluster_lights;
  GLuint palette_init[3]; // [metric:naive/riemersma/oklab]
  GLuint palette_postprocess;
} glprogs_t;

extern glprogs_t glprogs;

void
GL_UseProgram(GLuint program);
void
GL_ClearCachedProgram(void);
void
GL_CreateShaders(void);
void
GL_DeleteShaders(void);

typedef struct glframebufs_s
{
  GLint max_color_tex_samples;
  GLint max_depth_tex_samples;
  GLint max_samples; // lowest of max_color_tex_samples/max_depth_tex_samples

  struct
  {
    GLint samples;
    GLuint color_tex;
    GLuint depth_stencil_tex;
    GLuint fbo;
  } scene;

  struct
  {
    GLuint color_tex;
    GLuint fbo;
  } resolved_scene;

  struct
  {
    GLuint color_tex;
    GLuint depth_stencil_tex;
    GLuint fbo;
  } composite;

  struct
  {
    union
    {
      GLuint mrt[2];
      struct
      {
        GLuint accum_tex;
        GLuint revealage_tex;
      };
    };
    GLuint fbo_scene;
    GLuint fbo_composite;
  } oit;
} glframebufs_t;

extern glframebufs_t framebufs;

void
GL_CreateFrameBuffers(void);
void
GL_DeleteFrameBuffers(void);

void
GLLight_CreateResources(void);
void
GLLight_DeleteResources(void);

void
GLPalette_CreateResources(void);
void
GLPalette_DeleteResources(void);
void
GLPalette_UpdateLookupTable(void);
int
GLPalette_Postprocess(void);

void
GL_MakeAliasModelDisplayLists(qmodel_t* m, aliashdr_t* hdr);

typedef struct skybox_s
{
  char name[256];
  float wind_dist;
  float wind_yaw;
  float wind_pitch;
  float wind_period;
  gltexture_t* textures[6];
  gltexture_t* cubemap;
  byte* cubemap_pixels;
  void* cubemap_offsets[6];
} skybox_t;

extern skybox_t* skybox;

void
Sky_Init(void);
void
Sky_ClearAll(void);
void
Sky_DrawSky(void);
void
Sky_NewMap(void);
void
Sky_LoadTexture(qmodel_t* m, texture_t* mt);
void
Sky_LoadTextureQ64(qmodel_t* m, texture_t* mt);
void
Sky_LoadSkyBox(const char* name);
void
Sky_SetupFrame(void);
qboolean
Sky_IsAnimated(void);

void
GL_BindBuffer(GLenum target, GLuint buffer);
void
GL_BindBufferRange(GLenum target,
                   GLuint index,
                   GLuint buffer,
                   GLintptr offset,
                   GLsizeiptr size);
void
GL_BindBuffersRange(GLenum target,
                    GLuint first,
                    GLsizei count,
                    const GLuint* buffers,
                    const GLintptr* offsets,
                    const GLsizeiptr* sizes);
GLuint
GL_CreateBuffer(GLenum target,
                GLenum usage,
                const char* name,
                size_t size,
                const void* data);
void
GL_DeleteBuffer(GLuint buffer);
void
GL_ClearBufferBindings(void);

void
GL_CreateFrameResources(void);
void
GL_DeleteFrameResources(void);
void
GL_Upload(GLenum target,
          const void* data,
          size_t numbytes,
          GLuint* outbuf,
          GLbyte** outofs);
void
GL_ReserveDeviceMemory(GLenum target,
                       size_t numbytes,
                       GLuint* outbuf,
                       size_t* outofs);
void
GL_AcquireFrameResources(void);
void
GL_ReleaseFrameResources(void);
void
GL_AddGarbageBuffer(GLuint handle);

qboolean
GL_NeedsSceneEffects(void);
qboolean
GL_NeedsPostprocess(void);
void
GL_PostProcess(void);

float
GL_WaterAlphaForTextureType(textype_t type);

#endif /* GLQUAKE_H */
