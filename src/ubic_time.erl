%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Paul Oliver <puzza007@gmail.com>
%%% @copyright (C) 2013 Ubiquiti Networks, Inc.
%%% @doc Functions that convert from and to common time formats.
%%% @end
%%%-------------------------------------------------------------------
-module(ubic_time).

-export([ datetime_to_epoch_msecs/1
        , datetime_to_epoch_secs/1
        , datetime_to_iso8601/1
        , datetime_to_iso8601_microseconds/0
        , epoch_msecs_to_datetime/1
        , epoch_msecs_to_iso8601/1
        , epoch_msecs_to_usecs/1
        , epoch_secs_to_datetime/1
        , epoch_secs_to_iso8601/1
        , epoch_usecs_to_msecs/1
        , gregorian_msecs_to_iso8601/1
        , gregorian_secs_to_iso8601/1
        , iso8601_to_datetime/1
        , iso8601_to_epoch_msecs/1
        , iso8601_to_epoch_secs/1
        , iso8601_to_gregorian_msecs/1
        , iso8601_to_gregorian_secs/1
        , now_to_epoch_msecs/0
        , now_to_epoch_secs/0
        , now_to_epoch_usecs/0
        , timestamp_to_epoch_msecs/1
        , timestamp_to_epoch_secs/1
        , timestamp_to_epoch_usecs/1
        ]).

%% Days between Jan 1, 0001 (beginning of the Gregorian calendar) and Jan 1, 1970 (Unix epoch) in seconds.
%% 62167219200 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(GREGORIAN_SECONDS_TO_UNIX_EPOCH, 62167219200).

-type epoch_seconds()                     :: non_neg_integer().
-type epoch_milliseconds()                :: non_neg_integer().
-type epoch_microseconds()                :: non_neg_integer().
-type millisecond()                       :: 0..999.

-export_type([epoch_seconds/0, epoch_milliseconds/0, epoch_microseconds/0, millisecond/0]).


-spec datetime_to_epoch_msecs({calendar:datetime1970(), millisecond()}) -> epoch_milliseconds().
datetime_to_epoch_msecs({Datetime, Msecs}) when Msecs >= 0, Msecs < 1000 ->
    (calendar:datetime_to_gregorian_seconds(Datetime) - ?GREGORIAN_SECONDS_TO_UNIX_EPOCH) * 1000 + Msecs.


-spec datetime_to_epoch_secs(calendar:datetime1970()) -> epoch_seconds().
datetime_to_epoch_secs(Datetime) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - ?GREGORIAN_SECONDS_TO_UNIX_EPOCH.


%% @doc Convert a date and time in the format returned by calendar:universal_time/0 to
%%      a binary string in the ISO 8601 format (e.g. "2012-02-15T14:39:15Z"; "2012-02-15T14:39:15.671Z").
-spec datetime_to_iso8601(calendar:datetime() | {calendar:datetime(), millisecond()}) -> binary().
datetime_to_iso8601({{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = Datetime) ->
    datetime_to_iso8601(Datetime, <<$Z>>);
datetime_to_iso8601({{{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = Datetime, Msecs}) when Msecs < 1000 ->
    Decimals = bstr:lpad(integer_to_binary(Msecs), 3, $0),
    datetime_to_iso8601(Datetime, <<$., Decimals/binary, $Z>>).


-spec datetime_to_iso8601(calendar:datetime(), Suffix :: binary()) -> binary().
datetime_to_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}, Suffix) ->
    YYYY = bstr:lpad(integer_to_binary(Year), 4, $0),
    MM = bstr:lpad(integer_to_binary(Month), 2, $0),
    DD = bstr:lpad(integer_to_binary(Day), 2, $0),
    Hh = bstr:lpad(integer_to_binary(Hour), 2, $0),
    Mm = bstr:lpad(integer_to_binary(Min), 2, $0),
    Ss = bstr:lpad(integer_to_binary(Sec), 2, $0),
    <<YYYY/binary, $-, MM/binary, $-, DD/binary, $T, Hh/binary, $:, Mm/binary, $:, Ss/binary, Suffix/binary>>.


-spec datetime_to_iso8601_microseconds() -> binary().
datetime_to_iso8601_microseconds() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    {_, _, Microseconds} = os:timestamp(),
    erlang:iolist_to_binary(io_lib:format("~4..0b-~2..0b-~2..0bT"
                                          "~2..0b:~2..0b:~2..0b."
                                          "~6..0bZ",
                                          [Year, Month, Day,
                                           Hour, Minute, Second,
                                           Microseconds])).


-spec epoch_msecs_to_datetime(epoch_milliseconds()) -> {calendar:datetime1970(), millisecond()}.
epoch_msecs_to_datetime(EpochMsecs) ->
    Epoch = EpochMsecs div 1000,
    Msecs = EpochMsecs rem 1000,
    {calendar:gregorian_seconds_to_datetime(?GREGORIAN_SECONDS_TO_UNIX_EPOCH + Epoch), Msecs}.


-spec epoch_msecs_to_iso8601(epoch_milliseconds()) -> binary().
epoch_msecs_to_iso8601(Milliseconds) ->
    datetime_to_iso8601(epoch_msecs_to_datetime(Milliseconds)).


-spec epoch_msecs_to_usecs(epoch_milliseconds()) -> epoch_microseconds().
epoch_msecs_to_usecs(Milliseconds) when is_integer(Milliseconds) ->
    Milliseconds * 1000.


-spec epoch_secs_to_datetime(epoch_seconds()) -> calendar:datetime1970().
epoch_secs_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(?GREGORIAN_SECONDS_TO_UNIX_EPOCH + Seconds).


-spec epoch_secs_to_iso8601(epoch_seconds()) -> binary().
epoch_secs_to_iso8601(Seconds) ->
    datetime_to_iso8601(epoch_secs_to_datetime(Seconds)).


-spec epoch_usecs_to_msecs(epoch_microseconds()) -> epoch_milliseconds().
epoch_usecs_to_msecs(Microseconds) when is_integer(Microseconds) ->
    Microseconds div 1000.


-spec gregorian_msecs_to_iso8601(Time :: non_neg_integer()) -> binary().
gregorian_msecs_to_iso8601(Time) ->
    Secs = Time div 1000,
    Msecs = Time rem 1000,
    datetime_to_iso8601({calendar:gregorian_seconds_to_datetime(Secs), Msecs}).


-spec gregorian_secs_to_iso8601(Seconds :: non_neg_integer()) -> binary().
gregorian_secs_to_iso8601(Seconds) ->
    datetime_to_iso8601(calendar:gregorian_seconds_to_datetime(Seconds)).


%% @doc Convert a datetime in the ISO 8601 format to a date and time in the
%%      format returned by calendar:universal_time/0.
-spec iso8601_to_datetime(binary()) -> calendar:datetime() | {calendar:datetime(), millisecond()}.
iso8601_to_datetime(<<YYYY:4/binary, $-, MM:2/binary, $-, DD:2/binary, $T,
                      Hh:2/binary, $:, Mm:2/binary, $:, Ss:2/binary, Tail/binary>>) ->
    Datetime = {{binary_to_integer(YYYY), binary_to_integer(MM), binary_to_integer(DD)},
                {binary_to_integer(Hh), binary_to_integer(Mm), binary_to_integer(Ss)}},
    case Tail of
        <<"Z">> ->
            Datetime;
        <<".000Z">> ->
            Datetime;
        <<$., Millisec:3/binary, $Z>> ->
            {Datetime, binary_to_integer(Millisec)};
        <<$., Millisec:3/binary, UtcOffset:6/binary>> ->
            {local_datetime_to_utc(Datetime, UtcOffset), binary_to_integer(Millisec)};
        <<UtcOffset:6/binary>> ->
            local_datetime_to_utc(Datetime, UtcOffset)
    end.


-spec iso8601_to_epoch_msecs(binary()) -> epoch_milliseconds().
iso8601_to_epoch_msecs(Bin) ->
    datetime_to_epoch_msecs(iso8601_to_datetime(Bin)).


-spec iso8601_to_epoch_secs(binary()) -> epoch_seconds().
iso8601_to_epoch_secs(Bin) ->
    datetime_to_epoch_secs(iso8601_to_datetime(Bin)).


-spec iso8601_to_gregorian_msecs(binary()) -> Milliseconds :: non_neg_integer().
iso8601_to_gregorian_msecs(Bin) ->
    case iso8601_to_datetime(Bin) of
        {Datetime, Msecs} when is_integer(Msecs) ->
            calendar:datetime_to_gregorian_seconds(Datetime) * 1000 + Msecs;
        Datetime ->
            calendar:datetime_to_gregorian_seconds(Datetime) * 1000
    end.


-spec iso8601_to_gregorian_secs(binary()) -> Seconds :: non_neg_integer().
iso8601_to_gregorian_secs(Bin) ->
    Datetime = case iso8601_to_datetime(Bin) of
                   {Datetime1, Millisecs} when is_integer(Millisecs) ->
                       Datetime1;
                   Datetime1 ->
                       Datetime1
    end,
    calendar:datetime_to_gregorian_seconds(Datetime).


-spec local_datetime_to_utc(LocalDatetime :: calendar:datetime(), UtcOffset :: binary()) -> calendar:datetime().
local_datetime_to_utc(LocalDatetime, <<Sign, TimezoneHour:2/binary, $:, TimezoneMin:2/binary>>) ->
    LocalSec = calendar:datetime_to_gregorian_seconds(LocalDatetime),
    %% Convert the the seconds in the local timezone to UTC.
    UtcSec = case ((binary_to_integer(TimezoneHour) * 3600 + binary_to_integer(TimezoneMin)) * 60) of
                 Offset when Sign =:= $- -> LocalSec - Offset;
                 Offset when Sign =:= $+ -> LocalSec + Offset
             end,
    calendar:gregorian_seconds_to_datetime(UtcSec).


-spec now_to_epoch_msecs() -> epoch_milliseconds().
now_to_epoch_msecs() ->
    timestamp_to_epoch_msecs(os:timestamp()).


-spec now_to_epoch_secs() -> epoch_seconds().
now_to_epoch_secs() ->
    timestamp_to_epoch_secs(os:timestamp()).


-spec now_to_epoch_usecs() -> epoch_microseconds().
now_to_epoch_usecs() ->
    timestamp_to_epoch_usecs(os:timestamp()).


-spec timestamp_to_epoch_msecs(erlang:timestamp()) -> epoch_milliseconds().
timestamp_to_epoch_msecs({Megasecs, Secs, Microsecs}) ->
    Megasecs * 1000000000 + Secs * 1000 + Microsecs div 1000.


-spec timestamp_to_epoch_secs(erlang:timestamp()) -> epoch_seconds().
timestamp_to_epoch_secs({Megasecs, Secs, _Microsecs}) ->
    Megasecs * 1000000000 + Secs * 1000.


-spec timestamp_to_epoch_usecs(erlang:timestamp()) -> epoch_microseconds().
timestamp_to_epoch_usecs({Megasecs, Secs, Microsecs}) ->
    Megasecs * 1000000000000 + Secs * 1000000 + Microsecs.
