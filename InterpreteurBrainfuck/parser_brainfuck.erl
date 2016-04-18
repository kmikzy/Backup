-module(brainfuck).
-export([parser/1]).

%% Parser générique
parser(String) -> parser(String, []).

%% Parser des opérations +
parser([$+|T], [{op, ID}|TCC]) -> 
	Acc = case (ID+1) of 0 -> TCC; N -> [{op, N}|TCC] end,
	parser(T, Acc);
parser([$+|T], Acc) -> parser(T, [{op, 1}|Acc]);

%% Parser des opérations -
parser([$-|T], [{op, ID}|TCC]) -> 
	Acc = case (ID-1) of 0 -> TCC; N -> [{op, N}|TCC] end,
	parser(T, Acc);
parser([$-|T], Acc) -> parser(T, [{op, -1}|Acc]);

%% Parser des opérations >
parser([$>|T], [{pt, ID}|TCC]) -> 
	Acc = case (ID+1) of 0 -> TCC; N -> [{pt, N}|TCC] end,
	parser(T, Acc);
parser([$>|T], Acc) -> parser(T, [{pt, 1}|Acc]);

%% Parser des opérations <
parser([$<|T], [{pt, ID}|TCC]) -> 
	Acc = case (ID-1) of 0 -> TCC; N -> [{pt, N}|TCC] end,
	parser(T, Acc);
parser([$<|T], Acc) -> parser(T, [{pt, -1}|Acc]);

%% Parser des Io's
parser([$.|T], Acc) -> parser(T, [{output}|Acc]);
parser([$,|T], Acc) -> parser(T, [{input}|Acc]);

%% Parser des boucles
parser([$[|T], Acc) ->
	{Loop, RestLoop} = parser(T),
	parser(RestLoop, [{loop, Loop}|Acc]);
parser([$]|T], Acc) -> {lists:reverse(Acc), T};

%% Cas triviaux
parser([], Acc) -> lists:reverse(Acc);
parser([_|T], Acc) -> parser(T, Acc).
