% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ifconfig_ascii).

% API
-export([
  ip/1
]).

% API

-spec ip(binary()) -> binary().
ip(Ip) ->
  Ip2 = unicode:characters_to_list(Ip),
  ip(Ip2, 1, <<>>).

% Private

-spec ip(string(), pos_integer(), binary()) -> binary().
ip(_Ip, Count, <<"\n", Acc/binary>>) when Count > 5 ->
  Acc;
ip(Ip, Count, Acc) ->
  Line = [[to_ascii(Char, Count), " "] || Char <- Ip],
  Line2 = unicode:characters_to_binary(Line),
  Acc2 = <<Acc/binary, "\n", Line2/binary>>,
  ip(Ip, Count + 1, Acc2).

to_ascii($0, 1) -> "####";
to_ascii($0, 2) -> "#  #";
to_ascii($0, 3) -> "#  #";
to_ascii($0, 4) -> "#  #";
to_ascii($0, 5) -> "####";

to_ascii($1, 1) -> "#";
to_ascii($1, 2) -> "#";
to_ascii($1, 3) -> "#";
to_ascii($1, 4) -> "#";
to_ascii($1, 5) -> "#";

to_ascii($2, 1) -> "####";
to_ascii($2, 2) -> "   #";
to_ascii($2, 3) -> "####";
to_ascii($2, 4) -> "#   ";
to_ascii($2, 5) -> "####";

to_ascii($3, 1) -> "####";
to_ascii($3, 2) -> "   #";
to_ascii($3, 3) -> " ###";
to_ascii($3, 4) -> "   #";
to_ascii($3, 5) -> "####";

to_ascii($4, 1) -> "#  #";
to_ascii($4, 2) -> "#  #";
to_ascii($4, 3) -> "####";
to_ascii($4, 4) -> "   #";
to_ascii($4, 5) -> "   #";

to_ascii($5, 1) -> "####";
to_ascii($5, 2) -> "#   ";
to_ascii($5, 3) -> "####";
to_ascii($5, 4) -> "   #";
to_ascii($5, 5) -> "####";

to_ascii($6, 1) -> "####";
to_ascii($6, 2) -> "#   ";
to_ascii($6, 3) -> "####";
to_ascii($6, 4) -> "#  #";
to_ascii($6, 5) -> "####";

to_ascii($7, 1) -> "####";
to_ascii($7, 2) -> "   #";
to_ascii($7, 3) -> "   #";
to_ascii($7, 4) -> "   #";
to_ascii($7, 5) -> "   #";

to_ascii($8, 1) -> "####";
to_ascii($8, 2) -> "#  #";
to_ascii($8, 3) -> "####";
to_ascii($8, 4) -> "#  #";
to_ascii($8, 5) -> "####";

to_ascii($9, 1) -> "####";
to_ascii($9, 2) -> "#  #";
to_ascii($9, 3) -> "####";
to_ascii($9, 4) -> "   #";
to_ascii($9, 5) -> "####";

to_ascii($a, 1) -> "####";
to_ascii($a, 2) -> "#  #";
to_ascii($a, 3) -> "####";
to_ascii($a, 4) -> "#  #";
to_ascii($a, 5) -> "#  #";

to_ascii($b, 1) -> "### ";
to_ascii($b, 2) -> "#  #";
to_ascii($b, 3) -> "### ";
to_ascii($b, 4) -> "#  #";
to_ascii($b, 5) -> "### ";

to_ascii($c, 1) -> "####";
to_ascii($c, 2) -> "#   ";
to_ascii($c, 3) -> "#   ";
to_ascii($c, 4) -> "#   ";
to_ascii($c, 5) -> "####";

to_ascii($d, 1) -> "### ";
to_ascii($d, 2) -> "#  #";
to_ascii($d, 3) -> "#  #";
to_ascii($d, 4) -> "#  #";
to_ascii($d, 5) -> "### ";

to_ascii($e, 1) -> "####";
to_ascii($e, 2) -> "#   ";
to_ascii($e, 3) -> "### ";
to_ascii($e, 4) -> "#   ";
to_ascii($e, 5) -> "####";

to_ascii($f, 1) -> "####";
to_ascii($f, 2) -> "#   ";
to_ascii($f, 3) -> "### ";
to_ascii($f, 4) -> "#   ";
to_ascii($f, 5) -> "#   ";

to_ascii($:, 1) -> " ";
to_ascii($:, 2) -> " ";
to_ascii($:, 3) -> "#";
to_ascii($:, 4) -> " ";
to_ascii($:, 5) -> "#";

to_ascii($., 1) -> " ";
to_ascii($., 2) -> " ";
to_ascii($., 3) -> " ";
to_ascii($., 4) -> " ";
to_ascii($., 5) -> "#".
