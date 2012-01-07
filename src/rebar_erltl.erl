%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% 
%% -------------------------------------------------------------------
%% Derived from https://github.com/basho/rebar/blob/master/src/rebar_erlydtl_compiler.erl
%% October 2011 - Matthias Horn (matthias.horn@acm.org)
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com),
%%                    Bryan Fink (bryan@basho.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------

%% The rebar_erltl_compiler module is a plugin for rebar that compiles
%% ErlTL templates.  By default, it compiles all pages/*.erltl
%% to ebin/*.beam.
%%
%% Configuration options should be placed in rebar.config under
%% 'erltl_opts'.  Available options include:
%%
%%  doc_root: where to find templates to compile
%%            "pages" by default
%%
%%  out_dir: where to put compiled template beam files
%%           "ebin" by default
%%
%%  source_ext: the file extension the template sources have
%%              ".erltl" by default
%%
%% For example, if you had:
%%   /t_src/
%%          base.html
%%          foo.html
%%
%% And you wanted them compiled to:
%%   /priv/
%%         base.beam
%%         foo.beam
%%
%% You would add to your rebar.config:
%%   {erltl_opts, [
%%               {doc_root,   "t_src"},
%%               {out_dir,    "priv"},
%%               {source_ext, ".html"},
%%              ]}.
%%
%% The default settings are the equivalent of:
%%   {erltl_opts, [
%%               {doc_root,   "pages"},
%%               {out_dir,    "ebin"},
%%               {source_ext, ".erltl"},
%%              ]}.
-module(rebar_erltl).

-export([compile/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    % io:format("~s:~b ~~ ~s:compile(~w,~w).",[?FILE,?LINE,?MODULE,Config,_AppFile]),
    ErlTLOpts = erltl_opts(Config),
    ErlcOpts = erlc_opts(Config),
    ?RES(
    rebar_base_compiler:run(Config, [],
                            option(doc_root, ErlTLOpts),
                            option(source_ext, ErlTLOpts),
                            option(out_dir, ErlTLOpts++ErlcOpts),
                            option(out_ext, ErlTLOpts),
                            fun compile_tl/3, [{check_last_mod, false}])).


%% ===================================================================
%% Internal functions
%% ===================================================================

erltl_opts(Config) ->
    rebar_config:get(Config, erltl_opts, []).
erlc_opts(Config) ->
    rebar_config:get(Config, erlc_opts, []).

option(Opt, ErlTLOpts) ->
    proplists:get_value(Opt, ErlTLOpts, default(Opt)).

default(doc_root) -> "pages";
default(out_dir)  -> "ebin";
default(source_ext) -> ".erltl";
default(out_ext) -> "_erltl.beam".

compile_tl(Source, Target, Config) ->
            case needs_compile(Source, Target, Config) of
                true ->
                    do_compile(Source, Target, Config);
                false ->
                    skipped
            end.

do_compile(Source, Target, Config) ->
    %% TODO: Check last mod on target and referenced ErlTLs here..
    ?RES({?MODULE,Source,Target},begin
    ErlTLOpts = erltl_opts(Config),
    ErlcOpts = erlc_opts(Config),
    OutDir = option(out_dir, ErlTLOpts++ErlcOpts),
    Module = list_to_atom(re:replace(string:substr(filename:rootname(Target),length(OutDir)+2),"/",".",[global])),
    %% ensure that doc_root and out_dir are defined,
    %% using defaults if necessary
    Opts = [{outdir, OutDir}, report,return],
    case erltl:compile(Source,Module, Opts++ErlTLOpts++ErlcOpts) of
        ok -> ok;
        {ok,_,Bin,_} ->
           filelib:ensure_dir(Target),
           file:write_file(Target,Bin);
        Reason ->
            ?CONSOLE("Compiling template ~s failed:~n  ~p~n",
                     [Source, Reason]),
            ?FAIL
    end
  end).

needs_compile(Source, Target, _Config) ->
    LM = filelib:last_modified(Source),
    LC = filelib:last_modified(Target),
    LM>LC.



