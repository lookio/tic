%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Paul Oliver <puzza007@gmail.com>
%%% @author Darach Ennis <darach@gmail.com>
%%% @copyright (C) 2013 Ubiquiti Networks, Inc.
%%% @end
%%%-------------------------------------------------------------------
-module(ubic_lib_SUITE).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, {seconds, 120}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [{pkcs, [parallel],
      [
       pkcs5_empty_test,
       pkcs5_pad_1_test,
       pkcs5_pad_2_test,
       pkcs5_pad_3_test,
       pkcs5_pad_4_test,
       pkcs5_pad_5_test,
       pkcs5_pad_6_test,
       pkcs5_pad_7_test,
       pkcs5_pad_8_test,
       pkcs5_pad_9_test,
       pkcs5_pad_10_test,
       pkcs5_pad_11_test,
       pkcs5_pad_12_test,
       pkcs5_pad_13_test,
       pkcs5_pad_14_test,
       pkcs5_pad_15_test,
       pkcs5_pad_16_test
      ]},
     {ubic_time, [parallel],
      [
       datetime_to_iso8601_test,
       iso8601_to_datetime_test,
       gregorian_seconds_to_iso8601_test,
       iso8601_to_gregorian_seconds_test,
       datetime_to_epoch_test,
       epoch_to_datetime_test,
       epoch_to_iso8601_test,
       iso8601_to_epoch_test
      ]},
     {ubic_api_util, [parallel],
      [
       api_util_url_test
      ]},
     {ubic_country, [parallel],
      [
       iso3166_all_test,
       iso3166_alpha2_test,
       iso3166_alpha3_test
      ]},
     {ubic_lib_kv, [parallel],
      [
       kv_util_find_test
      ]},
     {ubic_lib_option, [parallel],
      [
       to_val_with_def_test,
       of_val_with_cond_test
      ]}
     ].

all() ->
    [{group, pkcs},
     {group, ubic_time},
     {group, ubic_api_util},
     {group, ubic_country},
     {group, ubic_lib_kv},
     {group, ubic_lib_option}].

%% pkcs5

pkcs5_empty_test(_) ->
    Unpadded = <<>>,
    Padded = <<16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_1_test(_) ->
    Unpadded = <<"0123456789ABCDE">>,
    Padded = <<"0123456789ABCDE", 1>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_2_test(_) ->
    Unpadded = <<"0123456789ABCD">>,
    Padded = <<"0123456789ABCD", 2, 2>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_3_test(_) ->
    Unpadded = <<"0123456789ABC">>,
    Padded = <<"0123456789ABC", 3, 3, 3>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_4_test(_) ->
    Unpadded = <<"0123456789AB">>,
    Padded = <<"0123456789AB", 4, 4, 4, 4>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_5_test(_) ->
    Unpadded = <<"0123456789A">>,
    Padded = <<"0123456789A", 5, 5, 5, 5, 5>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_6_test(_) ->
    Unpadded = <<"0123456789">>,
    Padded = <<"0123456789", 6, 6, 6, 6, 6, 6>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_7_test(_) ->
    Unpadded = <<"012345678">>,
    Padded = <<"012345678", 7, 7, 7, 7, 7, 7, 7>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_8_test(_) ->
    Unpadded = <<"01234567">>,
    Padded = <<"01234567", 8, 8, 8, 8, 8, 8, 8, 8>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_9_test(_) ->
    Unpadded = <<"0123456">>,
    Padded = <<"0123456", 9, 9, 9, 9, 9, 9, 9, 9, 9>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_10_test(_) ->
    Unpadded = <<"012345">>,
    Padded = <<"012345", 10, 10, 10, 10, 10, 10, 10, 10, 10, 10>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_11_test(_) ->
    Unpadded = <<"01234">>,
    Padded = <<"01234", 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_12_test(_) ->
    Unpadded = <<"0123">>,
    Padded = <<"0123", 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_13_test(_) ->
    Unpadded = <<"012">>,
    Padded = <<"012", 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_14_test(_) ->
    Unpadded = <<"01">>,
    Padded = <<"01", 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_15_test(_) ->
    Unpadded = <<"0">>,
    Padded = <<"0", 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

pkcs5_pad_16_test(_) ->
    Unpadded = <<"0123456789ABCDEF">>,
    Padded = <<"0123456789ABCDEF", 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16>>,
    Padded = iolist_to_binary(ubic_pkcs5:pad(Unpadded, 16)),
    Unpadded = ubic_pkcs5:unpad(Padded, 16).

%% ubic_time

%% Days between Jan 1, 0001 (beginning of the Gregorian calendar) and Jan 1, 1970 (Unix epoch) in seconds.
%% 62167219200 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(SECONDS_TO_UNIX_EPOCH, 62167219200).

datetime_to_iso8601_test(_) ->
    <<"2012-05-19T22:34:55Z">> = ubic_time:datetime_to_iso8601({{2012,5,19},{22,34,55}}),
    <<"2012-11-30T09:01:00.486Z">> = ubic_time:datetime_to_iso8601({{{2012,11,30},{9,1,0}},486}).

iso8601_to_datetime_test(_) ->
    {{2012,05,19},{22,34,55}} = ubic_time:iso8601_to_datetime(<<"2012-05-19T22:34:55Z">>),
    {{{2012,11,30},{9,1,0}},486} = ubic_time:iso8601_to_datetime(<<"2012-11-30T09:01:00.486Z">>).

gregorian_seconds_to_iso8601_test(_) ->
    GregorianSec = calendar:datetime_to_gregorian_seconds({{2012,10,5},{1,10,11}}),
    <<"2012-10-05T01:10:11Z">> = ubic_time:gregorian_secs_to_iso8601(GregorianSec).

iso8601_to_gregorian_seconds_test(_) ->
    GregorianSec = calendar:datetime_to_gregorian_seconds({{1950,2,22},{15,30,14}}),
    GregorianSec = ubic_time:iso8601_to_gregorian_secs(<<"1950-02-22T15:30:14Z">>),
    Ms = GregorianSec * 1000 + 653,
    Ms = ubic_time:iso8601_to_gregorian_msecs(<<"1950-02-22T15:30:14.653Z">>).

datetime_to_epoch_test(_) ->
    Datetime = {{2000,1,1},{10,20,30}},
    Epoch = calendar:datetime_to_gregorian_seconds(Datetime) - ?SECONDS_TO_UNIX_EPOCH,
    Epoch = ubic_time:datetime_to_epoch_secs(Datetime).

epoch_to_datetime_test(_) ->
    Datetime = {{2000,1,1},{10,20,30}},
    Epoch = calendar:datetime_to_gregorian_seconds(Datetime) - ?SECONDS_TO_UNIX_EPOCH,
    Datetime = ubic_time:epoch_secs_to_datetime(Epoch).

epoch_to_iso8601_test(_) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{2011, 12, 31},{5,25,53}}) - ?SECONDS_TO_UNIX_EPOCH,
    <<"2011-12-31T05:25:53Z">> = ubic_time:epoch_secs_to_iso8601(Epoch),
    <<"2011-12-31T05:25:53.672Z">> = ubic_time:epoch_msecs_to_iso8601(Epoch * 1000 + 672).

iso8601_to_epoch_test(_) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1989, 7, 20},{20,30,21}}) - ?SECONDS_TO_UNIX_EPOCH,
    Epoch = ubic_time:iso8601_to_epoch_secs(<<"1989-07-20T20:30:21Z">>),
    Ms = Epoch * 1000 + 217,
    Ms = ubic_time:iso8601_to_epoch_msecs(<<"1989-07-20T20:30:21.217Z">>).

%% ubic_api_util

api_util_url_test(_) ->
    Url0 = ubic_api_util:url(<<"////">>,[]),
    <<"">> = Url0,
    Url1 = ubic_api_util:url(<<"wss:////">>,[<<"host:port">>,<<"path">>,<<"path">>]),
    %% A single slash is ok, but '//' more usual after prefix
    <<"wss:/host:port/path/path">> = Url1.

%% ubic_country

iso3166_all_test(_) ->
    249 = erlang:length(ubic_country:iso3166_all()).

iso3166_alpha2_test(_) ->
    Codes = ubic_country:iso3166_alpha2(),
    249 = erlang:length(Codes),
    [2 = erlang:size(X) || X <- Codes].

iso3166_alpha3_test(_) ->
    Codes = ubic_country:iso3166_alpha3(),
    249 = erlang:length(Codes),
    [3 = erlang:size(X) || X <- Codes].

%% ubic_lib_kv

kv_util_find_test(_) ->
    Store = [{"A","Apple"},{"B","Bat"},{"C","Coffee"}],
    none = ubic_lib_kv:find(Store,"D"),
    {some,"Bat"} = ubic_lib_kv:find(Store,"B").

%% ubic_lib_option

to_val_with_def_test(_) ->
    Noms = {some,cake},
    Nones = {some,tiffin},
    cake = ubic_lib_option:to_val_with_def(Noms,tiffin),
    tiffin = ubic_lib_option:to_val_with_def(Nones,tiffin).

of_val_with_cond_test(_) ->
    Noms = cake,
    CondTrue = fun(X) -> cake =:= X end,
    CondFalse = fun(X) -> none =:= X end,
    {some,cake} = ubic_lib_option:of_val_with_cond(Noms,CondTrue),
    none = ubic_lib_option:of_val_with_cond(Noms,CondFalse).
