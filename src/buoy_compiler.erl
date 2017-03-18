-module(buoy_compiler).
-include("buoy_internal.hrl").

-export([
    pool_utils/0
]).

%% public
-spec pool_utils() ->
    ok.

pool_utils() ->
    Pools = ets:tab2list(?ETS_TABLE_POOL),
    Forms = pool_utils_forms(Pools),
    compile_and_load_forms(Forms),
    ok.

%% private
compile_and_load_forms(Forms) ->
    {ok, Module, Bin} = compile:forms(Forms, [debug_info]),
    code:purge(Module),
    Filename = atom_to_list(Module) ++ ".erl",
    {module, Module} = code:load_binary(Module, Filename, Bin),
    ok.

pool_utils_forms(Pools) ->
    Module = erl_syntax:attribute(erl_syntax:atom(module),
        [erl_syntax:atom(buoy_pool_utils)]),
    ExportList = [erl_syntax:arity_qualifier(erl_syntax:atom(name),
        erl_syntax:integer(3))],
    Export = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list(ExportList)]),
    Function = erl_syntax:function(erl_syntax:atom(name),
        name_clauses(Pools)),
    [erl_syntax:revert(X) || X <- [Module, Export, Function]].

name_clause(Protocol, Hostname, Port, Name) ->
    Hostname2 = binary_to_list(Hostname),
    BinaryField = [erl_syntax:binary_field(erl_syntax:string(Hostname2))],
    Var1 = erl_syntax:atom(Protocol),
    Var2 = erl_syntax:binary(BinaryField),
    Var3 = erl_syntax:integer(Port),
    Body = erl_syntax:atom(Name),
    erl_syntax:clause([Var1, Var2, Var3], [], [Body]).

name_clause_anon() ->
    Var = erl_syntax:variable("_"),
    Body = erl_syntax:atom(undefined),
    erl_syntax:clause([Var, Var, Var], [], [Body]).

name_clauses([]) ->
    [name_clause_anon()];
name_clauses([{{Protocol, Hostname, Port}, Name} | T]) ->
    [name_clause(Protocol, Hostname, Port, Name) | name_clauses(T)].
