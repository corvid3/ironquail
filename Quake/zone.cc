/*
Copyright (C) 1996-2001 Id Software, Inc.
Copyright (C) 2002-2009 John Fitzgibbons and others
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
// zone.c

#include "cmd.hh"
#include "common.hh"
#include "console.hh"
#include "gl_texmgr.hh"
#include "q_stdinc.hh"
#include "sys.hh"
#include "zone.hh"
#include <SDL2/SDL.h>

#define DYNAMIC_SIZE                                                           \
  (4 * 1024 * 1024) // ericw -- was 512KB (64-bit) / 384KB (32-bit)

#define ZONEID 0x1d4a11
#define MINFRAGMENT 64

void
Cache_FreeLow(int new_low_hunk);

/*
==============================================================================

                                                ZONE MEMORY ALLOCATION

There is never any space between memblocks, and there will never be two
contiguous free memblocks.

The rover can be left pointing at a non-empty block

The zone calls are pretty much only used for small strings and structures,
all big things are allocated on the hunk.

replaced with stdlib mem calls -crow
==============================================================================
*/

typedef struct
{
  unsigned size;
  char data[];
} Z_alloc_t;

// 4 megs max mem size
unsigned Z_max_mem_size = 1024 * 1024 * 4;
int Z_zone_usage = 0;

unsigned
Z_GetMaxMemSize()
{
  return Z_max_mem_size;
}

unsigned
Z_GetUsage()
{
  return Z_zone_usage;
}

/*
========================
Z_Free
========================
*/
void
Z_Free(void* ptr)
{
  Z_alloc_t* alloc = &static_cast<Z_alloc_t*>(ptr)[-1];

  if (ptr == NULL)
    Sys_Error("Z_Free: NULL pointer");

  Z_zone_usage -= alloc->size;

  free(alloc);
}

/*
========================
Z_Malloc
========================
*/
void*
Z_Malloc(int size)
{
  Z_alloc_t* buf = (Z_alloc_t*)malloc(sizeof(Z_alloc_t) + size);

  if (!buf)
    Sys_Error("Z_Malloc: failed on allocation of %i bytes", size);

  __builtin_memset(buf, 0, sizeof(Z_alloc_t) + size);
  buf->size = size;

  Z_zone_usage += size;

  return &buf->data;
}

/*
========================
Z_Realloc
========================
*/
void*
Z_Realloc(void* ptr, int size)
{
  Z_alloc_t* buf = (ptr == NULL ? NULL : &static_cast<Z_alloc_t*>(ptr)[-1]);
  if (buf != NULL)
    Z_zone_usage -= buf->size;
  buf = (Z_alloc_t*)realloc(buf, sizeof(Z_alloc_t) + size);

  if (buf == NULL)
    Sys_Error("Z_Realloc: failed on allocation of %i bytes", size);

  buf->size = size;
  Z_zone_usage += size;

  return &buf->data;
}

char*
Z_Strdup(const char* s)
{
  size_t sz = strlen(s) + 1;
  char* ptr = (char*)Z_Malloc(sz);
  memcpy(ptr, s, sz);
  return ptr;
}

/*
========================
Z_Print
========================
*/

typedef struct memzone_t
{
} memzone_t;

// vestigital
void
Z_Print(__attribute__((unused)) memzone_t* memzone)
{
}

/*
===============================================================================

CACHE MEMORY

===============================================================================
*/

#define CACHENAME_LEN 32
typedef struct cache_system_s
{
  int size; // including this header
  cache_user_t* user;
  char name[CACHENAME_LEN];
  struct cache_system_s *prev, *next;
  struct cache_system_s *lru_prev, *lru_next; // for LRU flushing
} cache_system_t;

cache_system_t*
Cache_TryAlloc(int size, qboolean nobottom);

cache_system_t cache_head;

/*
===========
Cache_Move
===========
*/
void
Cache_Move(cache_system_t* c)
{
  cache_system_t* new_cs;

  // we are clearing up space at the bottom, so only allocate it late
  new_cs = Cache_TryAlloc(c->size, true);
  if (new_cs) {
    //		Con_Printf ("cache_move ok\n");

    Q_memcpy(new_cs + 1, c + 1, c->size - sizeof(cache_system_t));
    new_cs->user = c->user;
    Q_memcpy(new_cs->name, c->name, sizeof(new_cs->name));
    Cache_Free(c->user, false); // johnfitz -- added second argument
    new_cs->user->data = (void*)(new_cs + 1);
  } else {
    //		Con_Printf ("cache_move failed\n");

    Cache_Free(c->user,
               true); // tough luck... //johnfitz -- added second argument
  }
}

//============================================================================

// vestigital
__attribute__((unused)) static void
Memory_InitZone(__attribute__((unused)) memzone_t* zone,
                __attribute__((unused)) int size)
{
}

/*
========================
Memory_Init
========================
*/
void
Memory_Init(void*, int)
{
  int p;

  // hunk_segments[0] = (hunkseg_t*)buf;
  // hunk_segments[0]->base = 0;
  // hunk_segments[0]->size = size - sizeof(hunkseg_t);
  // hunk_numsegments = 1;
  // hunk_low_used = 0;

  // Cache_Init();
  p = COM_CheckParm("-memsize");

  if (p) {
    if (p < com_argc - 1)
      Z_max_mem_size = Q_atoi(com_argv[p + 1]) * 1024;
    else
      Sys_Error("Memory_Init: you must specify a size in KB after -zone");
  }

  // mainzone = (memzone_t*)Hunk_AllocName(zonesize, "zone");
  // Memory_InitZone(mainzone, zonesize);

  // Cmd_AddCommand("hunk_print", Hunk_Print_f); // johnfitz
}
