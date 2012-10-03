%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2012 Christopher Meiklejohn (cmeiklejohn@basho.com)
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

%% The rebar_js_stylus_plugin module is a plugin for rebar that
%% preprocesses stylus stylesheet files.
%%
%% Configuration options should be placed in rebar.config under
%% 'js_stylus'.  Available options include:
%%
%%  stylus_path: path to the uglify executable
%%               "/usr/local/bin/stylus" by default
%%
%%  doc_root: where to find .styl files to compile
%%            "priv/assets/stylesheets" by default
%%
%%  out_dir: where to put preprocessed css files
%%           "priv/www/stylesheets" by default
%%
%%  stylesheets: stylus files to compile
%%           [] by default
%%
%% The default settings are the equivalent of:
%%   {js_stylus, [
%%               {stylus,       "/usr/local/bin/stylus"},
%%               {doc_root,     "priv/assets/stylesheets"},
%%               {out_dir,      "priv/www/stylesheets"}
%%               {stylesheets,  []}
%%              ]}.
%%

-module(rebar_js_stylus_plugin).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([compile/2,
         clean/2]).

%% ===================================================================
%% Public API
%% ===================================================================


compile(Config, _AppFile) ->
    Options = options(Config),
    DocRoot = option(doc_root, Options),
    Stylesheets = option(stylesheets, Options),
    Targets = [normalize_path(Stylesheet, DocRoot)
                || Stylesheet <- Stylesheets],
    process(Targets, Options).

process([], _Options) ->
    ok;
process(Targets, Options) ->
    OutDir = option(out_dir, Options),
    DocRoot = option(doc_root, Options),
    Stylus = option(stylus_path, Options),
    case stylus_is_present(Stylus) of
        true ->
            Cmd = lists:flatten([Stylus, " -o ", OutDir, " ",
                                 string:join(Targets, " ")]),
            ShOpts = [{use_stdout, false}, return_on_error],
            case rebar_utils:sh(Cmd, ShOpts) of
                {ok, _} ->
                    io:format("Creating stylesheet assets in ~s~n", [OutDir]),
                    ok;
                {error, Reason} ->
                    rebar_log:log(error, "Stylus asset processing failed failed:~n  ~p~n",
                           [Reason]),
                    rebar_utils:abort()
            end,
            ok;
        false ->
            rebar_log:log(warn,
                "Bypassing stylesheet processing of ~s: stylus missing.~n", [DocRoot]),
            ok
    end.

clean(Config, _AppFile) ->
    Options = options(Config),
    OutDir = option(out_dir, Options),
    case rebar_utils:find_files(OutDir, ".*\\.css$") of
        [] ->
            ok;
        Targets ->
            delete_each(Targets)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

options(Config) ->
    rebar_config:get_local(Config, js_stylus, []).

option(Option, Options) ->
    proplists:get_value(Option, Options, default(Option)).

default(doc_root)     -> "priv/assets/stylesheets";
default(out_dir)      -> "priv/www/stylesheets";
default(stylus_path)  -> "/usr/local/bin/stylus";
default(stylesheets)  -> [].

delete_each([]) ->
    ok;
delete_each([First | Rest]) ->
    case file:delete(First) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            rebar_log:log(error, "Failed to delete ~s: ~p\n", [First, Reason])
    end,
    delete_each(Rest).

normalize_path(Path, Basedir) -> filename:join([Basedir, Path]).

stylus_is_present(Stylus) -> filelib:is_file(Stylus).
