-module(jfile).

-export([read/1, read/2]).

% Reads a file into a list of lines quickly.
read(FileName) ->
    read(FileName, "\n").

read(FileName, Tokeniser) ->
  {ok, Binary} = file:read_file(FileName),
    string:tokens(binary_to_list(Binary), Tokeniser).
