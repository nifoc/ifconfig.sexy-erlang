% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ifconfig_hooks).

% API
-export([
  request/1
]).

% API

-spec request(cowboy_req:req()) -> cowboy_req:req().
request(Req) ->
  {Path, Req2} = cowboy_req:path(Req),
  case filename:extension(Path) of
    <<".txt">> -> rewrite_accept(Path, <<"text/plain">>, Req2);
    <<".json">> -> rewrite_accept(Path, <<"application/json">>, Req2);
    <<".jsonp">> -> rewrite_accept(Path, <<"application/javascript">>, Req2);
    <<".xml">> -> rewrite_accept(Path, <<"application/xml">>, Req2);
    <<".yaml">> -> rewrite_accept(Path, <<"application/x-yaml">>, Req2);
    <<".edn">> -> rewrite_accept(Path, <<"application/x-edn">>, Req2);
    <<".ascii">> -> rewrite_accept(Path, <<"text/x-ascii-art">>, Req2);
    _ -> Req2
  end.

% Private

-spec rewrite_accept(binary(), binary(), cowboy_req:req()) -> cowboy_req:req().
rewrite_accept(Path, ContentType, Req) ->
  Path2 = {path, filename:rootname(Path)},
  Headers = cowboy_req:get(headers, Req),
  Headers2 = case lists:keyfind(<<"accept">>, 1, Headers) of
    {<<"accept">>, _Type} -> lists:keyreplace(<<"accept">>, 1, Headers, {<<"accept">>, ContentType});
    false -> [{<<"accept">>, ContentType} | Headers]
  end,
  Headers3 = {headers, Headers2},
  cowboy_req:set([Path2, Headers3], Req).
