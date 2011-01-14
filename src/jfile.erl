-module(jfile).

-export([readfile/1, readfile/2]).

% Reads a file into a list of lines quickly.
read(FileName) ->
    readfile(FileName, "\n").

read(FileName, Tokeniser) ->
  {ok, Binary} = file:read_file(FileName),
    string:tokens(binary_to_list(Binary), Tokeniser).
