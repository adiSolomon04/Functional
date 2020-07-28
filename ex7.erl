%%%-------------------------------------------------------------------
%%% @author adisolo
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2020 13:51
%%%-------------------------------------------------------------------
-module(ex7).
-author("adisolo").

%% API
-export([steady/1, calc/3]).

steady(F) ->
{_, IoDevice} = file:open("myLog.elog", [append]),
  StartTime = erlang:timestamp(),

try F() of
  Result -> file:write(IoDevice, to_bytes({calc_time(StartTime), success, Result}))
catch
  error:Error -> file:write(IoDevice,to_bytes({calc_time(StartTime), error, Error}));
  exit:Exit -> file:write(IoDevice,to_bytes({calc_time(StartTime),exit, Exit}));
  Throw -> file:write(IoDevice,to_bytes({calc_time(StartTime), throw, Throw}))
end.

calc_time(StartTime) ->timer:now_diff(erlang:timestamp(),StartTime).

to_bytes(Output) -> io_lib:format("~p", [Output]).

calc(division, A,B) when is_number(A), is_number(B) ->
  try A/B of
    Result -> Result
  catch
    error:_ -> error_divided_by_zero
  end;
calc(_,_,_)-> input_error.
