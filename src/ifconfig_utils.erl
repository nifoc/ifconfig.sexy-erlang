% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ifconfig_utils).

% API
-export([
  priv_dir/1,
  static_dir/1,
  peer_ip/1,
  format/3
]).

% API

-spec priv_dir(module()) -> string().
priv_dir(Mod) ->
  Ebin = filename:dirname(code:which(Mod)),
  Root = filename:dirname(Ebin),
  Path = case string:substr(Root, 1, 1) of
    "." ->
      {ok, Cwd} = file:get_cwd(),
      EunitTest = string:str(Cwd, ".eunit") > 0,
      CommonTest = string:str(Cwd, "ct_run") > 0,
      if
        EunitTest -> filename:dirname(Cwd);
        CommonTest -> filename:dirname(filename:dirname(Cwd));
        true -> Root
      end;
    _ -> Root
  end,
  filename:join(Path, "priv").

-spec static_dir(module()) -> string().
static_dir(Mod) ->
  PrivDir = priv_dir(Mod),
  filename:join(PrivDir, "static").

-spec peer_ip(cowboy_req:req()) -> {binary(), cowboy_req:req()}.
peer_ip(Req) ->
  {{Ip, _Port}, Req2} = cowboy_req:peer(Req),
  Ip2 = unicode:characters_to_binary(inet_parse:ntoa(Ip)),
  {Ip2, Req2}.

-spec format(binary(), binary(), cowboy_req:req()) -> {ok, [{binary(), binary()}], binary(), cowboy_req:req()} | {{error, term()}, cowboy_req:req()}.
format(Field, Value, Req) ->
  case cowboy_req:header(<<"content-type">>, Req, undefined) of
    {<<"text/plain", _Rest/binary>>, Req2} -> format_txt(Value, Req2);
    {<<"text/html", _Rest/binary>>, Req2} -> format_txt(Value, Req2);
    {<<"application/xhtml+xml", _Rest/binary>>, Req2} -> format_txt(Value, Req2);
    {<<"application/json", _Rest/binary>>, Req2} -> format_json(Field, Value, Req2);
    {<<"application/xml", _Rest/binary>>, Req2} -> format_xml(Field, Value, Req2);
    {<<"application/x-yaml", _Rest/binary>>, Req2} -> format_yaml(Field, Value, Req2);
    {<<"application/edn", _Rest/binary>>, Req2} -> format_edn(Field, Value, Req2);
    {undefined, Req2} -> format_txt(Value, Req2);
    {_Type, Req2} -> {{error, unknown_content_type}, Req2}
  end.

% Private

-spec format_txt(binary(), cowboy_req:req()) -> {ok, [{binary(), binary()}], binary(), cowboy_req:req()}.
format_txt(Value, Req) ->
  {ok, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Value, Req}.

-spec format_json(binary(), binary(), cowboy_req:req()) -> {ok, [{binary(), binary()}], binary(), cowboy_req:req()}.
format_json(Field, Value, Req) ->
  Body = <<"{\"", Field/binary, "\":\"", Value/binary, "\"}">>,
  {ok, [{<<"content-type">>, <<"application/json; charset=utf-8">>}], Body, Req}.

-spec format_xml(binary(), binary(), cowboy_req:req()) -> {ok, [{binary(), binary()}], binary(), cowboy_req:req()}.
format_xml(Field, Value, Req) ->
  Body = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
            "<ifconfig>",
              "<", Field/binary, ">", Value/binary, "</", Field/binary, ">"
            "</ifconfig>">>,
  {ok, [{<<"content-type">>, <<"application/xml; charset=utf-8">>}], Body, Req}.

-spec format_yaml(binary(), binary(), cowboy_req:req()) -> {ok, [{binary(), binary()}], binary(), cowboy_req:req()}.
format_yaml(Field, Value, Req) ->
  Body = <<"---\n", Field/binary, ": ", Value/binary>>,
  {ok, [{<<"content-type">>, <<"application/x-yaml; charset=utf-8">>}], Body, Req}.

-spec format_edn(binary(), binary(), cowboy_req:req()) -> {ok, [{binary(), binary()}], binary(), cowboy_req:req()}.
format_edn(Field, Value, Req) ->
  Body = <<"{:", Field/binary, " \"", Value/binary, "\"}">>,
  {ok, [{<<"content-type">>, <<"application/edn; charset=utf-8">>}], Body, Req}.
