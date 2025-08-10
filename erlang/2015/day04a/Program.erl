#!/usr/bin/env escript

main(Args) ->
  case Args of
    [Key] -> io:format("result = ~p~n", [process(Key)]);
    _ -> usage()
  end.

usage() ->
  io:format(standard_error, "usage: Program.erl <key>~n", []),
  halt(1).

process(Key) ->
  find_value(Key, 1).

find_value(Key, N) ->
  TryKey = Key ++ integer_to_list(N),
  Digest = crypto:hash(md5, TryKey),
  HexDigest = binary:encode_hex(Digest, lowercase),
  case string:prefix(HexDigest, "00000") of
    nomatch -> find_value(Key, N + 1);
    _ -> N
  end.
