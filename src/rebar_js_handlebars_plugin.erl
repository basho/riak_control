%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2012 Christopher Meiklejohn (christopher.meiklejohn@gmail.com)
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

%% The rebar_js_handlebars_plugin module is a plugin for rebar that
%% wraps templates in compiler statements.
%%
%% Configuration options should be placed in rebar.config under
%% 'js_handlebars'.  Available options include:
%%
%%  doc_root: where to find javascript files to concatenate
%%            "priv/assets/javascripts" by default
%%
%%  out_dir: where to put concatenated javascript files
%%           "priv/www/javascripts" by default
%%
%%  templates: list of tuples of format {Destination, [Sources]}
%%             empty list by default.
%%
%%  source_ext: file extension to truncate to derive module name
%%              ".hbs" by default
%%
%% The default settings are the equivalent of:
%%
%%   {js_handlebars, [
%%       {doc_root,  "priv/assets/javascripts"},
%%       {out_dir,   "priv/www/javascripts"},
%%       {target,    "Ember.TEMPLATES"},
%%       {compiler,  "Ember.Handlebars.compile"},
%%       {source_ext,".hbs"},
%%       {templates, []}
%%   ]}.
%%
%% An example of compiling a series of templates files:
%%
%%   {js_handlebars, [
%%       {doc_root,  "priv/assets/javascripts"},
%%       {out_dir,   "priv/www/javascripts"},
%%       {target,    "Ember.TEMPLATES"},
%%       {compiler,  "Ember.Handlebars.compile"},
%%       {templates, [{"templates.js", ["sidebar.hbs", "application.hbs"]}]}
%%   ]}.
%%

-module(rebar_js_handlebars_plugin).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([compile/2,
         clean/2]).

-export([handlebars/4]).

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    Options = options(Config),
    OutDir = option(out_dir, Options),
    DocRoot = option(doc_root, Options),
    Templates = option(templates, Options),
    Targets = [{normalize_path(Destination, OutDir),
        normalize_paths(Sources, DocRoot), Options} || {Destination, Sources} <- Templates],
    build_each(Targets).

clean(Config, _AppFile) ->
    Options = options(Config),
    OutDir = option(out_dir, Options),
    Templates = option(templates, Options),
    Targets = [normalize_path(Destination, OutDir) || {Destination, _} <- Templates],
    delete_each(Targets).

%% @spec handlebars(list(), list(), list(), list()) -> binary()
%% @doc Generate a handlebars compiler line.
handlebars(Name, Body, Target, Compiler) ->
    Targeted = lists:flatten([Target, "['" ++ ensure_list(Name) ++ "']"]),
    Compiled = lists:flatten([Compiler, "('" ++ ensure_list(Body) ++ "');\n"]),
    list_to_binary(lists:flatten([Targeted, " = " ++ Compiled])).

%% ===================================================================
%% Internal functions
%% ===================================================================

options(Config) ->
    rebar_config:get_local(Config, js_handlebars, []).

option(Option, Options) ->
    proplists:get_value(Option, Options, default(Option)).

default(doc_root)  -> "priv/assets/javascripts";
default(out_dir)   -> "priv/www/javascripts";
default(target)    -> "Ember.TEMPLATES";
default(compiler)  -> "Ember.Handlebars.compile";
default(source_ext)-> ".hbs";
default(templates) -> [].

ensure_list(Object) ->
    case is_binary(Object) of
        true ->
            binary_to_list(Object);
        false ->
            Object
    end.

read(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
           list_to_binary(re:replace(binary_to_list(Binary), "\\n+", "",
                    [global]));
        {error, Reason} ->
           rebar_log:log(error, "Reading asset ~s failed during compilation:~n  ~p~n",
                   [File, Reason]),
           rebar_utils:abort()
    end.

normalize_paths(Paths, Basedir) ->
    lists:foldr(fun(X, Acc) -> [normalize_path(X, Basedir) | Acc] end, [], Paths).
normalize_path(Path, Basedir) ->
    filename:join([Basedir, Path]).

build_each([]) ->
    ok;
build_each([{Destination, Sources, Options} | Rest]) ->
    Target = option(target, Options),
    Compiler = option(compiler, Options),
    SourceExt = option(source_ext, Options),
    Contents = [handlebars(filename:basename(Source, SourceExt), read(Source), Target, Compiler)
                    || Source <- Sources],
    Concatenated = rebar_js_concatenator_plugin:concatenate(Contents),
    case file:write_file(Destination, Concatenated, [write]) of
        ok ->
            io:format("Compiled handlebars asset ~s~n", [Destination]);
        {error, Reason} ->
            rebar_log:log(error, "Handlebars compliation of ~s failed:~n  ~p~n",
                   [Destination, Reason]),
            rebar_utils:abort()
    end,
    build_each(Rest).

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

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

handlebars_test() ->
    Expected = <<"Ember.TEMPLATES['foo'] = Ember.Handlebars.compile('<h1>bar</h1>');\n">>,
    ListName = "foo",
    ListBody = "<h1>bar</h1>",
    ListTarget = "Ember.TEMPLATES",
    ListCompiler = "Ember.Handlebars.compile",
    ListOutput = handlebars(ListName, ListBody, ListTarget, ListCompiler),
    ?assertEqual(Expected, ListOutput),

    BinaryName = list_to_binary(ListName),
    BinaryBody = list_to_binary(ListBody),
    BinaryTarget = list_to_binary(ListTarget),
    BinaryCompiler = list_to_binary(ListCompiler),
    BinaryOutput = handlebars(BinaryName, BinaryBody, BinaryTarget, BinaryCompiler),
    ?assertEqual(Expected, BinaryOutput).

-endif.
