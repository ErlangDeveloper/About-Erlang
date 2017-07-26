%%%-------------------------------------------------------------------
%%% @author deadr
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 八月 2016 11:04
%%%-------------------------------------------------------------------
-module(concurrent).
-author("baymaxzhou").

-export([division_link/1, division_link/2]).

division_link(Fun)when is_function(Fun)->
	division_link(Fun, []).
division_link(Fun, Args) when is_function(Fun)->
	Fun1 = fun()->
		try
			apply(Fun, Args)
		catch
			_Error:Reason  ->
				{error, Reason}
		end
	end,
	proc_lib:spawn_link(Fun1).
