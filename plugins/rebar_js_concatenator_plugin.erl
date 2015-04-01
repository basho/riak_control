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

%% The rebar_js_concatenator_plugin module is a plugin for rebar that concatenates
%% javascript files.
%%
%% Configuration options should be placed in rebar.config under
%% 'js_concatenator'.  Available options include:
%%
%%  doc_root: where to find javascript files to concatenate
%%            "priv/assets/javascripts" by default
%%
%%  out_dir: where to put concatenated javascript files
%%           "priv/assets/javascripts" by default
%%
%%  concatenations: list of tuples describing each transformation.
%%                  empty list by default
%%
%% The default settings are the equivalent of:
%%   {js_concatenator, [
%%                {out_dir, "priv/assets/javascripts"},
%%                {doc_root, "priv/assets/javascripts"},
%%                {concatenations, []}
%%               ]}.
%%
%% An example of compiling a series of javascript files:
%%
%%   {js_concatenator, [
%%       {out_dir, "priv/assets/javascripts"},
%%       {doc_root, "priv/assets/javascripts"},
%%       {concatenations, [
%%           {"vendor.js", ["ember.js", "jquery.js"], []},
%%           {"application.js", ["models.js", "controllers.js"], []}
%%       ]}
%%   ]}.
%%

-module(rebar_js_concatenator_plugin).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([compile/2,
         clean/2]).

-export([concatenate/1,
         concatenate_files/1]).

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    Options = options(Config),
    Concatenations = option(concatenations, Options),
    OutDir = option(out_dir, Options),
    DocRoot = option(doc_root, Options),
    Targets = [{normalize_path(Destination, OutDir),
                normalize_paths(Sources, DocRoot),
                ConcatOptions} || {Destination, Sources, ConcatOptions} <- Concatenations],
    build_each(Targets).

clean(Config, _AppFile) ->
    Options = options(Config),
    Concatenations = option(concatenations, Options),
    OutDir = option(out_dir, Options),
    Targets = [normalize_path(Destination, OutDir) ||
               {Destination, _Sources, _ConcatOptions} <- Concatenations],
    delete_each(Targets).

%% @spec concatenate(list()) -> binary()
%% @doc Given a list of sources, concatenate and return.
concatenate(Sources) ->
    ListSources = [case is_binary(Source) of true ->
                binary_to_list(Source); false -> Source end || Source <- Sources],
    list_to_binary(lists:flatten(ListSources)).

%% @spec concatenate_files(list()) -> list()
%% @doc Given a list of source files, concatenate and return.
concatenate_files(Sources) ->
    concatenate([read(Source) || Source <- Sources]).

%% ===================================================================
%% Internal functions
%% ===================================================================

options(Config) ->
    rebar_config:get_local(Config, js_concatenator, []).

option(Option, Options) ->
    proplists:get_value(Option, Options, default(Option)).

default(doc_root) -> "priv/assets/javascripts";
default(out_dir)  -> "priv/assets/javascripts";
default(concatenations) -> [].

normalize_paths(Paths, Basedir) ->
    lists:foldr(fun(X, Acc) -> [normalize_path(X, Basedir) | Acc] end, [], Paths).
normalize_path(Path, Basedir) ->
    filename:join([Basedir, Path]).

build_each([]) ->
    ok;
build_each([{Destination, Sources, ConcatOptions} | Rest]) ->
    case any_needs_concat(Sources, Destination) of
        true ->
            Contents = concatenate_files(Sources),
            case file:write_file(Destination, Contents, [write]) of
                ok ->
                    io:format("Built asset ~s~n", [Destination]),
                    case lists:member(uglify, ConcatOptions) of
                        true ->
                            uglify(Destination);
                        false ->
                            ok
                    end;
                {error, Reason} ->
                    rebar_log:log(error, "Building asset ~s failed:~n  ~p~n",
                           [Destination, Reason]),
                    rebar_utils:abort()
            end;
        false ->
            ok
    end,
    build_each(Rest).

uglify(Source) ->
    Destination = uglify_destination(Source),
    rebar_js_uglifier_plugin:compress(Source, Destination, []).

uglify_destination(Source) ->
    Outdir = filename:dirname(Source),
    Basename = filename:basename(Source, ".js"),
    normalize_path(lists:flatten(Basename ++ ".min.js"), Outdir).

read(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
           Binary;
        {error, Reason} ->
           rebar_log:log(error, "Reading asset ~s failed during concatenation:~n  ~p~n",
                   [File, Reason]),
           rebar_utils:abort()
    end.

any_needs_concat(Sources, Destination) ->
    lists:any(fun(X) -> needs_concat(X, Destination) end, Sources).
needs_concat(Source, Destination) ->
    filelib:last_modified(Destination) < filelib:last_modified(Source).

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

-ifdef(TEST).

concatenate_test() ->
    ListSource1 = "ping",
    ListSource2 = "pong",
    ListOutput = concatenate([ListSource1, ListSource2]),
    ?assertEqual(<<"pingpong">>, ListOutput),
    BinarySource1 = <<"ping">>,
    BinarySource2 = <<"pong">>,
    BinaryOutput = concatenate([BinarySource1, BinarySource2]),
    ?assertEqual(<<"pingpong">>, BinaryOutput).

-endif.
