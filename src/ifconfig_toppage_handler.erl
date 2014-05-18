% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ifconfig_toppage_handler).
-behaviour(cowboy_http_handler).

% HTTP
-export([
  init/3,
  handle/2,
  terminate/3
]).

% HTTP

init(_Transport, Req, _Opts) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {ok, Req3} = response(Method, Req2),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.

% Private

response(<<"GET">>, Req) ->
  StaticDir = ifconfig_utils:static_dir(?MODULE),
  IndexFile = filename:join(StaticDir, "index.html"),
  {ok, Body} = file:read_file(IndexFile),
  cowboy_req:reply(200, [{<<"content-type">>, "text/html"}], Body, Req);
response(_Method, Req) ->
  cowboy_req:reply(405, Req).
