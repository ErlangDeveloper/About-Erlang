-module(algorithms).
-export([merge_sort/1]).

%% -------------
%% 合并排序/归并排序
%% -------------
merge_sort([]) -> [];
merge_sort([T]) -> [T];
merge_sort(L) ->
    M = trunc(length(L)  / 2),
    {RawLeft, RawRight} = lists:split(M, L),
    Left = merge_sort(RawLeft),
    Right = merge_sort(RawRight),
    merge(Left, Right).

merge(Left, Right) ->
    merge(Left, Right, []).

merge([T1|L1], [T2|_L2]=L2, R) when T1 < T2 -> merge(L1, L2, R++[T1]);
merge([_|_]=L1, [T2|L2], R) -> merge(L1, L2, R++[T2]);
merge([], L2, R) -> R++L2;
merge(L1, [], R) -> R++L1;
merge([], [], R) -> R.


%% -------------
%% 最大子数组
%% -------------
max_child_list([])->
	[];
max_child_list([X])->
	[X];
max_child_list(List)->
	M = trunc(length(List) / 2),
	{AList, BList} = lists:split(M, List),
	MaxA = max_child_list(AList),
	MaxB = max_child_list(BList),
	MaxC = divide(lists:reverse(AList), [], []) ++ lists:reverse(divide(BList, [], [])),
	A = lists:sum(MaxA),
	B = lists:sum(MaxB),
	C = lists:sum(MaxC),
	[_, _, {_, Max}] = lists:keysort(1, [{A, MaxA}, {B, MaxB}, {C, MaxC}]),
	Max.
  
  divide([], _, {_, Result})->
	Result;
divide([H|T], [], _)->
	NewKey = H,
	NewList = [H],
	divide(T, [{NewKey, NewList}], {NewKey, NewList});
divide([H|T], X = [{Key, LH}|_], {Max, Result})->
	NewKey =  Key + H,
	NewList = [H|LH],
	case NewKey > Max of
		true->
			divide(T, [{Key + H, NewList}|X], {NewKey, NewList});

		_ ->
			divide(T, [{Key + H, NewList}|X], {Max, Result})
	end.
