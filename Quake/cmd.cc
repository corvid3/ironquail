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
// cmd.c -- Quake script command processing module

#include "client.hh"
#include "cmd.hh"
#include "common.hh"
#include "console.hh"
#include "q_stdinc.hh"
#include "quakedef.hh"
#include "server.hh"
#include "str.hh"
#include "sys.hh"
#include <SDL2/SDL.h>
#include <algorithm>
void
Cmd_ForwardToServer(void);

#define MAX_ALIAS_NAME 32

#define CMDLINE_LENGTH 256 // johnfitz -- mirrored in common.c

typedef struct cmdalias_s
{
  q_str<> name;
  q_str<> value;

  bool operator==(cmdalias_s const& rhs) const { return name == rhs.name; }
} cmdalias_t;

q_list<cmdalias_t> cmd_alias;

qboolean cmd_wait;

//=============================================================================

/*
============
Cmd_Wait_f

Causes execution of the remainder of the command buffer to be delayed until
next frame.  This allows commands like:
bind g "impulse 5 ; +attack ; wait ; -attack ; impulse 2"
============
*/
void
Cmd_Wait_f(void)
{
  cmd_wait = true;
}

/*
===============
Cmd_CfgMarker_f
===============
*/
static qboolean in_cfg_exec = false;
static void
Cmd_CfgMarker_f(void)
{
  in_cfg_exec = false;
}

/*
=============================================================================

                                                COMMAND BUFFER

=============================================================================
*/

sizebuf_t cmd_text;

/*
============
Cbuf_Init
============
*/
void
Cbuf_Init(void)
{
  SZ_Alloc(&cmd_text,
           1 << 18); // space for commands and script files. spike -- was 8192,
                     // but modern configs can be _HUGE_, at least if they
                     // contain lots of comments/docs for things.
}

/*
============
Cbuf_AddText

Adds command text at the end of the buffer
============
*/
void
Cbuf_AddText(const char* text)
{
  int l;

  l = Q_strlen(text);

  if (cmd_text.cursize + l >= cmd_text.maxsize) {
    Con_Printf("Cbuf_AddText: overflow\n");
    return;
  }

  SZ_Write(&cmd_text, text, Q_strlen(text));
}
void
Cbuf_AddTextLen(const char* text, int l)
{
  if (cmd_text.cursize + l >= cmd_text.maxsize) {
    Con_Printf("Cbuf_AddText: overflow\n");
    return;
  }

  SZ_Write(&cmd_text, text, l);
}

/*
============
Cbuf_InsertText

Adds command text immediately after the current command
Adds a \n to the text
FIXME: actually change the command buffer to do less copying
============
*/
void
Cbuf_InsertText(const char* text)
{
  char* temp;
  int templen;

  // copy off any commands still remaining in the exec buffer
  templen = cmd_text.cursize;
  if (templen) {
    temp = (char*)Z_Malloc(templen);
    Q_memcpy(temp, cmd_text.data, templen);
    SZ_Clear(&cmd_text);
  } else
    temp = NULL; // shut up compiler

  // add the entire text of the file
  Cbuf_AddText(text);
  SZ_Write(&cmd_text, "\n", 1);
  // add the copied off data
  if (templen) {
    SZ_Write(&cmd_text, temp, templen);
    Z_Free(temp);
  }
}

// Spike: for renderer/server isolation
void
Cbuf_Waited(void)
{
  cmd_wait = false;
}

/*
============
Cbuf_Execute

Spike: reworked 'wait' for renderer/server rate independance
============
*/
void
Cbuf_Execute(void)
{
  int constexpr max_line_len = 1023;

  int i;
  char* text;
  char line[max_line_len + 1];
  int quotes, comment;

  while (cmd_text.cursize && !cmd_wait) {
    // find a \n or ; line break
    text = (char*)cmd_text.data;

    quotes = 0;
    comment = 0;
    for (i = 0; i < cmd_text.cursize; i++) {
      if (text[i] == '"')
        quotes++;
      if (text[i] == '/' && text[i + 1] == '/')
        comment = true;
      if (!(quotes & 1) && !comment && text[i] == ';')
        break; // don't break if inside a quoted string
      if (text[i] == '\n')
        break;
    }

    if (i > (int)sizeof(line) - 1) {
      memcpy(line, text, max_line_len);
      line[max_line_len] = 0;
    } else {
      memcpy(line, text, i);
      line[i] = 0;
    }

    // delete the text from the command buffer and move remaining commands down
    // this is necessary because commands (exec, alias) can insert data at the
    // beginning of the text buffer

    if (i == cmd_text.cursize)
      cmd_text.cursize = 0;
    else {
      i++;
      cmd_text.cursize -= i;
      memmove(text, text + i, cmd_text.cursize);
    }

    // execute the command line
    Cmd_ExecuteString(line, src_command);
  }
}

/*
==============================================================================

                                                SCRIPT COMMANDS

==============================================================================
*/

/*
===============
Cmd_StuffCmds_f -- johnfitz -- rewritten to read the "cmdline" cvar, for use
with dynamic mod loading

Adds command line parameters as script statements
Commands lead with a +, and continue until a - or another +
quake +prog jctest.qp +cmd amlev1
quake -nosound +cmd amlev1
===============
*/
void
Cmd_StuffCmds_f(void)
{
  extern cvar_t cmdline;
  char cmds[CMDLINE_LENGTH];
  int i, j, plus;

  plus = false; // On Unix, argv[0] is command name

  for (i = 0, j = 0; cmdline.string[i]; i++) {
    if (cmdline.string[i] == '+') {
      plus = true;
      if (j > 0) {
        cmds[j - 1] = ';';
        cmds[j++] = ' ';
      }
    } else if (cmdline.string[i] == '-' &&
               (i == 0 ||
                cmdline.string[i - 1] ==
                  ' ')) // johnfitz -- allow hypenated map names with +map
      plus = false;
    else if (plus)
      cmds[j++] = cmdline.string[i];
  }
  cmds[j] = 0;

  Cbuf_InsertText(cmds);
}

/* id1/pak0.pak from 2021 re-release doesn't have a default.cfg
 * embedding Quakespasm's customized default.cfg for that...  */
#include "default_cfg.hh"

/*
===============
Cmd_Exec_f
===============
*/
void
Cmd_Exec_f(void)
{
  const char* path;
  const char* f;
  int mark;

  if (Cmd_Argc() < 2) {
    Con_Printf("exec <filename> [pls] : execute a script file\n");
    return;
  }

  mark = Hunk_LowMark();
  path = Cmd_Argv(1);

  // HACK:
  // "exec config.cfg" will execute ironwail.cfg
  // "exec config.cfg pls" will execute config.cfg
  if (Cmd_Argc() == 2 && !strcmp(path, "config.cfg")) {
    f = (const char*)COM_LoadHunkFile(CONFIG_NAME, NULL);
    if (f) {
      path = CONFIG_NAME;
      goto exec;
    }
  }

  f = (const char*)COM_LoadHunkFile(path, NULL);
  if (!f && !strcmp(Cmd_Argv(1), "default.cfg")) {
    f = default_cfg; /* see above.. */
  }
  if (!f) {
    Con_Printf("couldn't exec %s\n", path);
    return;
  }
exec:
  Con_Printf("execing %s\n", path);

  if (!in_cfg_exec) {
    in_cfg_exec = true;
    // Note: this will be executed *after* the config
    Cbuf_InsertText("__cfgmarker");
  }
  Cbuf_InsertText(f);
  if (f != default_cfg) {
    Hunk_FreeToLowMark(mark);
  }
}

/*
===============
Cmd_Echo_f

Just prints the rest of the line to the console
===============
*/
void
Cmd_Echo_f(void)
{
  int i;

  for (i = 1; i < Cmd_Argc(); i++)
    Con_Printf("%s ", Cmd_Argv(i));
  Con_Printf("\n");
}

/*
===============
Cmd_Alias_f -- johnfitz -- rewritten

Creates a new command that executes a command string (possibly ; seperated)
===============
*/
void
Cmd_Alias_f(void)
{
  q_str<> cmd;
  int i, c;
  const char* s;

  switch (Cmd_Argc()) {
    case 1: // list all aliases
      i = 0;

      for (auto const& alias : cmd_alias)
        Con_SafePrintf("   %s: %s", alias.name.data(), alias.value.data()), i++;
      if (i)
        Con_SafePrintf("%i alias command(s)\n", i);
      else
        Con_SafePrintf("no alias commands found\n");
      break;
    case 2: // output current alias string
      for (auto const& alias : cmd_alias)
        if (alias.name == Cmd_Argv(1))
          Con_Printf("   %s: %s", alias.name.data(), alias.value.data());
      break;
    default: // set alias string
      s = Cmd_Argv(1);
      if (strlen(s) >= MAX_ALIAS_NAME) {
        Con_Printf("Alias name is too long\n");
        return;
      }

      auto alias =
        std::find_if(cmd_alias.begin(),
                     cmd_alias.end(),
                     [s](cmdalias_t const& alias) { return alias.name == s; });

      // if the alias already exists, reuse it
      // if not, initialize a new one
      if (alias == cmd_alias.end()) {
        cmd_alias.push_back({});
        alias = cmd_alias.end();
        alias--;
        alias->name = s;
      }

      // copy the rest of the command line
      cmd[0] = 0; // start out with a null string
      c = Cmd_Argc();
      for (i = 2; i < c; i++) {
        cmd += Cmd_Argv(i);
        if (i != c - 1)
          cmd += " ";
      }

      cmd += "\n";

      alias->value = cmd;
      break;
  }
}

/*
===============
Cmd_Unalias_f -- johnfitz
===============
*/
void
Cmd_Unalias_f(void)
{
  switch (Cmd_Argc()) {
    default:
    case 1:
      Con_Printf("unalias <name> : delete alias\n");
      break;
    case 2:

      for (auto a = cmd_alias.begin(); a != cmd_alias.end(); a++) {
        if (a->name == Cmd_Argv(1)) {
          cmd_alias.erase(a);
          return;
        }
      }
      Con_Printf("No alias named %s\n", Cmd_Argv(1));
      break;
  }
}

qboolean
Cmd_AliasExists(const char* aliasname)
{
  return std::find_if(cmd_alias.begin(),
                      cmd_alias.end(),
                      [aliasname](cmdalias_t const& alias) {
                        return aliasname == alias.name;
                      }) != cmd_alias.end();
}

/*
===============
Cmd_Unaliasall_f -- johnfitz
===============
*/
void
Cmd_Unaliasall_f(void)
{
  cmd_alias.clear();
}

/*
=============================================================================

                                        COMMAND EXECUTION

=============================================================================
*/

#define MAX_ARGS 80

static int cmd_argc;
static q_str<> cmd_argv[MAX_ARGS];
static char cmd_null_string[] = "";
static const char* cmd_args = NULL;

cmd_source_t cmd_source;

// johnfitz -- better tab completion
// static	cmd_function_t	*cmd_functions;		// possible commands to
// execute
cmd_function_t* cmd_functions; // possible commands to execute
// johnfitz

/*
============
Cmd_IsReservedName

Returns true if name starts with 2 underscores
============
*/
qboolean
Cmd_IsReservedName(const char* name)
{
  return name[0] == '_' && name[1] == '_';
}

/*
============
Cmd_List_f -- johnfitz
============
*/
void
Cmd_List_f(void)
{
  cmd_function_t* cmd;
  const char* partial;
  int len, count;

  if (Cmd_Argc() > 1) {
    partial = Cmd_Argv(1);
    len = Q_strlen(partial);
  } else {
    partial = NULL;
    len = 0;
  }

  count = 0;
  for (cmd = cmd_functions; cmd; cmd = cmd->next) {
    if (cmd->srctype == src_server)
      continue;
    if (Cmd_IsReservedName(cmd->name))
      continue;
    if (partial && Q_strncmp(partial, cmd->name, len))
      continue;
    Con_SafePrintf("   %s\n", cmd->name);
    count++;
  }

  Con_SafePrintf("%i commands", count);
  if (partial) {
    Con_SafePrintf(" beginning with \"%s\"", partial);
  }
  Con_SafePrintf("\n");
}

/*
============
Cmd_ListAllContaining

scans through each command and cvar names+descriptions for the given substring
we don't support descriptions, so this isn't really all that useful, but even
without the sake of consistency it still combines cvars+commands under a single
command.
============
*/
static void
Cmd_ListAllContaining(const char* substr)
{
  char tmpbuf[256];
  int hits = 0;
  cmd_function_t* cmd;
  cvar_t* var;
  const char* plural;

  for (cmd = cmd_functions; cmd; cmd = cmd->next) {
    if (cmd->srctype != src_server && q_strcasestr(cmd->name, substr) &&
        !Cmd_IsReservedName(cmd->name)) {
      hits++;
      Con_SafePrintf(
        "   %s\n",
        COM_TintSubstring(cmd->name, substr, tmpbuf, sizeof(tmpbuf)));
    }
  }

  for (var = Cvar_FindVarAfter("", 0); var; var = var->next) {
    if (q_strcasestr(var->name, substr)) {
      hits++;
      Con_SafePrintf(
        "   %s (current value: \"%s\")\n",
        COM_TintSubstring(var->name, substr, tmpbuf, sizeof(tmpbuf)),
        var->string);
    }
  }

  plural = (hits == 1) ? "" : "s";
  if (!hits)
    Con_SafePrintf("no cvars/commands contain '%s'\n", substr);
  else
    Con_SafePrintf(
      "%d cvar%s/command%s containing '%s'\n", hits, plural, plural, substr);
}

/*
============
Cmd_Apropos_f
============
*/
void
Cmd_Apropos_f(void)
{
  const char* substr = Cmd_Argv(1);
  if (!*substr) {
    Con_SafePrintf("%s <substring> : search through commands and cvars for the "
                   "given substring\n",
                   Cmd_Argv(0));
    return;
  }
  Cmd_ListAllContaining(substr);
}

/*
============
Cmd_Init
============
*/
void
Cmd_Init(void)
{
  Cmd_AddCommand("cmdlist", Cmd_List_f);          // johnfitz
  Cmd_AddCommand("unalias", Cmd_Unalias_f);       // johnfitz
  Cmd_AddCommand("unaliasall", Cmd_Unaliasall_f); // johnfitz

  Cmd_AddCommand("stuffcmds", Cmd_StuffCmds_f);
  Cmd_AddCommand("exec", Cmd_Exec_f);
  Cmd_AddCommand("echo", Cmd_Echo_f);
  Cmd_AddCommand("alias", Cmd_Alias_f);
  Cmd_AddCommand("cmd", Cmd_ForwardToServer);
  Cmd_AddCommand("wait", Cmd_Wait_f);

  Cmd_AddCommand("apropos", Cmd_Apropos_f);
  Cmd_AddCommand("find", Cmd_Apropos_f);

  Cmd_AddCommand("__cfgmarker", Cmd_CfgMarker_f);
}

/*
============
Cmd_Argc
============
*/
int
Cmd_Argc(void)
{
  return cmd_argc;
}

/*
============
Cmd_Argv
============
*/
const char*
Cmd_Argv(int arg)
{
  if (arg < 0 || arg >= cmd_argc)
    return cmd_null_string;
  return cmd_argv[arg].data();
}

/*
============
Cmd_Args
============
*/
const char*
Cmd_Args(void)
{
  return cmd_args;
}

/*
============
Cmd_AddArg
============
*/
void
Cmd_AddArg(const char* arg)
{
  if (cmd_argc < MAX_ARGS) {
    cmd_argv[cmd_argc] = arg;
    cmd_argc++;
  }
}

/*
============
Cmd_TokenizeString

Parses the given string into command line tokens.
============
*/
void
Cmd_TokenizeString(const char* text)
{
  int i;

  // clear the args from the last string
  for (i = 0; i < cmd_argc; i++)
    cmd_argv[i].clear();

  cmd_argc = 0;
  cmd_args = NULL;

  while (1) {
    // skip whitespace up to a /n
    while (*text && *text <= ' ' && *text != '\n') {
      text++;
    }

    if (*text == '\n') { // a newline seperates commands in the buffer
      text++;
      break;
    }

    if (!*text)
      return;

    if (cmd_argc == 1)
      cmd_args = text;

    text = COM_Parse(text);
    if (!text)
      return;

    Cmd_AddArg(com_token);
  }
}

/*
============
Cmd_AddCommand

spike -- added an extra arg for client (also renamed and made a macro)
============
*/
cmd_function_t*
Cmd_AddCommand2(const char* cmd_name,
                xcommand_t function,
                cmd_source_t srctype,
                qboolean qcinterceptable)
{
  cmd_function_t* cmd;
  cmd_function_t *cursor, *prev; // johnfitz -- sorted list insert

  // fail if the command is a variable name
  if (Cvar_VariableString(cmd_name)[0]) {
    Con_Printf("Cmd_AddCommand: %s already defined as a var\n", cmd_name);
    return NULL;
  }

  // fail if the command already exists
  for (cmd = cmd_functions; cmd; cmd = cmd->next) {
    if (!Q_strcmp(cmd_name, cmd->name) && cmd->srctype == srctype) {
      if (cmd->function != function && function)
        Con_Printf("Cmd_AddCommand: %s already defined\n", cmd_name);
      return NULL;
    }
  }

  if (host_initialized) {
    cmd = (cmd_function_t*)malloc(sizeof(*cmd) + strlen(cmd_name) + 1);
    if (!cmd)
      Sys_Error("Cmd_AddCommand2: out of memory (%s)", cmd_name);
    cmd->name = strcpy((char*)(cmd + 1), cmd_name);
    cmd->dynamic = true;
  } else {
    cmd = (cmd_function_t*)Hunk_Alloc(sizeof(*cmd));
    cmd->name = cmd_name;
    cmd->dynamic = false;
  }
  cmd->function = function;
  cmd->srctype = srctype;
  cmd->qcinterceptable = qcinterceptable;

  // johnfitz -- insert each entry in alphabetical order
  if (cmd_functions == NULL ||
      strcmp(cmd->name, cmd_functions->name) < 0) // insert at front
  {
    cmd->next = cmd_functions;
    cmd_functions = cmd;
  } else // insert later
  {
    prev = cmd_functions;
    cursor = cmd_functions->next;
    while ((cursor != NULL) && (strcmp(cmd->name, cursor->name) > 0)) {
      prev = cursor;
      cursor = cursor->next;
    }
    cmd->next = prev->next;
    prev->next = cmd;
  }
  // johnfitz

  return cmd;
}
void
Cmd_RemoveCommand(cmd_function_t* cmd)
{
  cmd_function_t** link;
  for (link = &cmd_functions; *link; link = &(*link)->next) {
    if (*link == cmd) {
      *link = cmd->next;
      free(cmd);
      return;
    }
  }
  Sys_Error("Cmd_RemoveCommand unable to remove command %s", cmd->name);
}

/*
============
Cmd_Find
============
*/
cmd_function_t*
Cmd_FindCommand(const char* cmd_name)
{
  cmd_function_t* cmd;

  for (cmd = cmd_functions; cmd; cmd = cmd->next)
    if (!q_strcasecmp(cmd_name, cmd->name))
      return cmd;

  return NULL;
}

/*
============
Cmd_Exists
============
*/
qboolean
Cmd_Exists(const char* cmd_name)
{
  return Cmd_FindCommand(cmd_name) != NULL;
}

/*
============
Cmd_ExecuteString

A complete command line has been parsed, so try to execute it
FIXME: lookupnoadd the token to speed search?
============
*/
qboolean
Cmd_ExecuteString(const char* text, cmd_source_t src)
{
  cmd_function_t* cmd;

  cmd_source = src;
  Cmd_TokenizeString(text);

  // execute the command line
  if (!Cmd_Argc())
    return true; // no tokens

  // check functions
  for (cmd = cmd_functions; cmd; cmd = cmd->next) {
    if (!caseins_streq(cmd_argv[0], cmd->name)) {
      if (src == src_client && cmd->srctype != src_client)
        Con_DPrintf("%s tried to %s\n",
                    host_client->name,
                    text); // src_client only allows client commands
      else if (src == src_command && cmd->srctype == src_server)
        continue; // src_command can execute anything but server commands (which
                  // it ignores, allowing for alternative behaviour)
      else if (src == src_server && cmd->srctype != src_server)
        continue; // src_server may only execute server commands (such commands
                  // must be safe to parse within the context of a network
                  // message, so no disconnect/connect/playdemo/etc)
      else if (cmd->function)
        cmd->function();
      else
        Con_Printf("gamecode not running, cannot \"%s\"\n", Cmd_Argv(0));
      return true;
    }
  }

  if (src == src_client) { // spike -- please don't execute similarly named
                           // aliases, nor custom cvars...
    Con_DPrintf("%s tried to %s\n", host_client->name, text);
    return false;
  }
  if (src != src_command)
    return false;

  // check alias
  for (auto const& a : cmd_alias) {
    if (!caseins_streq(cmd_argv[0], a.name)) {
      Cbuf_InsertText(a.value.data());
      return true;
    }
  }

  // check cvars
  if (!Cvar_Command()) {
    if (in_cfg_exec)
      Con_Printf("Unknown command \"%s\"\n", Cmd_Argv(0));
    else
      Cmd_ListAllContaining(Cmd_Argv(0));
  }

  return true;
}

/*
===================
Cmd_ForwardToServer

Sends the entire command line over to the server
===================
*/
void
Cmd_ForwardToServer(void)
{
  if (cls.state != ca_connected) {
    Con_Printf("Can't \"%s\", not connected\n", Cmd_Argv(0));
    return;
  }

  if (cls.demoplayback)
    return; // not really connected

  MSG_WriteByte(&cls.message, clc_stringcmd);
  if (q_strcasecmp(Cmd_Argv(0), "cmd") != 0) {
    SZ_Print(&cls.message, Cmd_Argv(0));
    SZ_Print(&cls.message, " ");
  }
  if (Cmd_Argc() > 1)
    SZ_Print(&cls.message, Cmd_Args());
  else
    SZ_Print(&cls.message, "\n");
}

/*
================
Cmd_CheckParm

Returns the position (1 to argc-1) in the command's argument list
where the given parameter apears, or 0 if not present
================
*/

int
Cmd_CheckParm(const char* parm)
{
  int i;

  if (!parm)
    Sys_Error("Cmd_CheckParm: null input\n");

  for (i = 1; i < Cmd_Argc(); i++)
    if (!q_strcasecmp(parm, Cmd_Argv(i)))
      return i;

  return 0;
}
