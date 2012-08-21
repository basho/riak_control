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

%% The rebar_js_minispade_plugin module is a plugin for rebar that
%% wraps files in closures and registers them with minispade.
%%
%% Configuration options should be placed in rebar.config under
%% 'js_minispade'.  Available options include:
%%
%%  doc_root: where to find javascript files to concatenate
%%            "priv/assets/javascripts" by default
%%
%%  out_dir: where to put concatenated javascript files
%%           "priv/www/javascripts" by default
%%
%%  modules: files to build into minispade reigstered modules
%%           empty list by default.
%%
%% The default settings are the equivalent of:
%%   {js_minispade, [
%%       {out_dir,  "priv/assets/javascripts"},
%%       {doc_root, "priv/www/javascripts"},
%%       {modules,  []}
%%   ]}.
%%
%% An example of compiling a series of javascript files:
%%
%%   {js_minispade, [
%%       {out_dir,  "priv/assets/javascripts"},
%%       {doc_root, "priv/www/javascripts"},
%%       {modules,  ["models", "controllers"]
%%       ]}
%%   ]}.
%%

-module(rebar_js_minispade_plugin).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([compile/2,
         clean/2]).

-export([minispade/2]).

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    Options = options(Config),
    Modules = option(modules, Options),
    OutDir = option(out_dir, Options),
    DocRoot = option(doc_root, Options),
    Targets = [{Module, normalize_path(Module, OutDir), normalize_path(Module, DocRoot)}
               || Module <- Modules],
    build_each(Targets).

clean(Config, _AppFile) ->
    Options = options(Config),
    Modules = option(modules, Options),
    OutDir = option(out_dir, Options),
    Targets = [normalize_path(Module, OutDir) || Module <- Modules],
    delete_each(Targets).

%% @spec minispade(list(), list()) -> binary()
%% @doc Given a module name and a JavaScript function, return Minispade.
minispade(Module, Contents) ->
    Functionized = lists:flatten(["function() {", Contents, "}"]),
    list_to_binary(lists:flatten(["minispade.register('", Module, "', ", Functionized, ")"])).

%% ===================================================================
%% Internal functions
%% ===================================================================

options(Config) ->
    rebar_config:get_local(Config, js_minispade, []).

option(Option, Options) ->
    proplists:get_value(Option, Options, default(Option)).

default(doc_root) -> "priv/assets/javascripts";
default(out_dir)  -> "priv/www/javascripts";
default(modules)  -> [].

normalize_path(Path, Basedir) ->
    filename:join([Basedir, Path ++ ".js"]).

read(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
           Binary;
        {error, Reason} ->
           rebar_log:log(error, "Reading asset ~s failed during minispade:~n  ~p~n",
                   [File, Reason]),
           rebar_utils:abort()
    end.

build_each([]) ->
    ok;
build_each([{Module, Destination, Source} | Rest]) ->
    Contents = read(Source),
    Minispade = minispade(Module, Contents),
    case file:write_file(Destination, Minispade, [write]) of
        ok ->
            io:format("Minispade asset ~s~n", [Destination]);
        {error, Reason} ->
            rebar_log:log(error, "Minispade asset ~s failed:~n  ~p~n",
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

minispade_test() ->
    ListModule = "alerter",
    ListFunction = "alert('zomg');",
    ListOutput = minispade(ListModule, ListFunction),
    ?assertEqual(<<"minispade.register('alerter', function() {alert('zomg');})">>, ListOutput),
    BinaryModule = <<"alerter">>,
    BinaryFunction = <<"alert('zomg');">>,
    BinaryOutput = minispade(BinaryModule, BinaryFunction),
    ?assertEqual(<<"minispade.register('alerter', function() {alert('zomg');})">>, BinaryOutput).

-endif.
