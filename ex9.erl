-module(ex9).
-author("adisolo").

%% API
-export([etsBot/0]).



etsBot()->
  {Ok,IoDevice}=file:open("etsCommands.txt", [read]),
  case Ok of
    error ->error_in_file;
    ok -> {ok,Line}=file:read_line(IoDevice),
      TableType=string:tokens(Line,"\n"),ets:new(table, [get_atom(TableType),named_table]),
      table_cmd(file:read_line(IoDevice), IoDevice)
  end.

get_atom(Data) -> list_to_atom(lists:flatten(io_lib:format("~s", [Data]))).

table_cmd({ok, Command}, IoDevice) -> [String] = string:tokens(Command,"\n"),[Head|Tail]=string:tokens(String," "),
  case Head of
    "insert" -> insert_cmd(Tail);
    "update" -> update_cmd(Tail);
    "delete" -> delete_cmd(Tail);
    "lookup" -> lookup_cmd(Tail)
  end, table_cmd(file:read_line(IoDevice), IoDevice);
table_cmd(eof, _) -> {_,IoDevice} = file:open("etsRes.ets", [write]),
  Data = ets:tab2list(table),
  write_to_file(Data, IoDevice).


%% commands insert, update , delete, lookup for every row.
%% will do the corresponding command on all of the keys in the row
insert_cmd([Key,Value|Tail]) -> ets:insert_new(table, {Key, Value}),
  insert_cmd(Tail);
insert_cmd([]) -> done.

update_cmd([Key,Value|Tail]) -> case ets:lookup(table, Key) of
                                  [{Key,_}] -> ets:insert(table, {Key, Value}),
                                    update_cmd(Tail);
                                  _ -> update_cmd(Tail)
                                end;
update_cmd([]) -> done.

delete_cmd([Key|Tail]) -> ets:delete(table, Key), delete_cmd(Tail);
delete_cmd([]) -> done.

lookup_cmd([Key|Tail]) -> case ets:lookup(table, Key) of
                            [{Key,Value}] -> io:format("key: ~s val: ~s ",[Key, Value]),
                              lookup_cmd(Tail);
                            _ -> update_cmd(Tail)
                          end;
lookup_cmd([]) -> done.

write_to_file([{Key, Val}|Data], IoDevice) -> file:write(IoDevice,io_lib:format("~s ~s~n",[Key, Val])),
  write_to_file(Data, IoDevice);
write_to_file([],_) -> io:format("~nwrote to file ",[]), botExit.


%%to_bytes(Output) -> io_lib:format("~p~n", [Output]).