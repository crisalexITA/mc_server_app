%%%-------------------------------------------------------------------
%% @doc mc_server public API
%% @end
%%%-------------------------------------------------------------------

-module(mc_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("records.hrl").

start(_StartType, _StartArgs) ->
    mc_server_sup:start_link(),
    % start server
    start_server().

stop(_State) ->
    ok.

%% internal functions
start_server() -> 

    {ok, ListenSocket} = gen_tcp:listen(6666, [{active, true}, binary]),
    
    % create db rooms table 
    ets:new(rooms, [named_table, public, {keypos, #room.name}]),
    % create db users table
    ets:new(users, [named_table, public, {keypos, #user.socket}]),
    % create public room
    ets:insert(rooms, #room{name="public", owner=""}),

    spawn(fun() -> handleClient(ListenSocket) end),
    timer:sleep(infinity).

handleClient(ListenSocket) -> 
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handleClient(ListenSocket) end),
    handleMessages(AcceptSocket).

handleMessages(AcceptSocket) ->
    inet:setopts(AcceptSocket, [{active, true}]),
    receive
        {tcp, Socket, BinMessage} ->
            
            Message = binary_to_term(BinMessage),

            % log incoming message
            io:format("~p:~p~n", [Socket, Message]),

            processMessage(Socket, Message),
            handleMessages(AcceptSocket)
    end.

processMessage(Socket, Data) ->
    case Data of 
        {connect} ->
            gen_tcp:send(Socket, "[SYSTEM] Hello, you can change your name with {register, <name>}"),
            timer:sleep(1),
            gen_tcp:send(Socket, "[SYSTEM] Digit {help} to get a list of avalaiblle commands");

        % register new user
        {register, Name} ->
            gen_tcp:send(Socket, lists:append(["[SYSTEM] You has been registered as ", Name])),
            timer:sleep(1),
            
            % insert into users table
            ets:insert(users, #user{name=Name, socket=Socket}),

            % add user to public room 
            enterRoom(Socket, "public");
        
        % user exit
        {quit, Pid} ->
            gen_tcp:send(Socket, string:concat("Bye", Pid)),
            gen_tcp:close(Socket);
        
        % room message
        {send_message_room, Room, Message} ->
            sendMessageRoom(Socket, Room, Message);

        % private message
        {send_message_private, Receiver, Message} ->
            sendMessagePrivate(Socket, Receiver, Message);

        % rooms list
        {rooms} ->
            getRooms(Socket);

        % create new room
        {create_room, Room} ->
            createRoom(Socket, Room);

        % users room list
        {users_room, Room} ->
            getUsersRoom(Socket, Room);

        % user exit room
        {exit_room, Room} ->
            exitRoom(Socket, Room);

        % user enter room
        {enter_room, Room} ->
            enterRoom(Socket, Room)
            
    end.

exitRoom(Socket, Room) ->
    % send msg to rooms users
    sendMessageRoom(Room, "has left the room", socketToUser(Socket)),
    
    DbRoom = hd(ets:lookup(rooms, Room)),
    Clients = lists:delete(Socket, DbRoom#room.clients),
    ets:insert(rooms, #room{name=Room, clients=Clients}).


getUsersRoom(Socket, Room) ->
    % get clients(users) in room
    DbRoom = hd(ets:lookup(rooms, Room)),
    Clients = DbRoom#room.clients,

    Message = string:replace("[SYSTEM] Actually in the room [*] there are those users: [", "*", Room),
    StringUsers =lists:map(
        fun(Client) -> string:concat(socketToUser(Client), " | ") end,
        Clients
    ),
    gen_tcp:send(Socket, string:concat(Message, string:concat(string:trim(StringUsers, trailing, " | "), "]"))).

socketToUser(Socket) -> 
    DbUser = hd(ets:lookup(users, Socket)),
    DbUser#user.name.

enterRoom(Socket, Room) ->
    DbRoom = hd(ets:lookup(rooms, Room)),
    Clients = DbRoom#room.clients ++ [Socket],
    ets:insert(rooms, #room{name=Room, clients=Clients}),
    
    % send msg to rooms users
    sendMessageRoom(Socket, Room, "entered the room").

sendMessageRoom(Socket, Room, Text) ->
    % get clients(users) in room
    DbRoom = hd(ets:lookup(rooms, Room)),
    Clients = DbRoom#room.clients,
    % send message to each user in the room
    lists:foreach(
        fun(Client) ->          
            %StringPid = string:replace("[$#*] ^", "$", socketToUser(Socket)),
            %StringRoom = string:replace(StringPid, "*", Room),
            %StringText = string:replace(StringRoom, "^", Text),
            gen_tcp:send(Client, lists:append(["[", socketToUser(Socket), "#", Room, "] ", Text]))
        end,
        Clients
    ).

sendMessagePrivate(Socket, Receiver, Text) ->
    % get receiver client 
    DbReceiver = hd(ets:match_object(users, #user{_='_', name=Receiver})),

    % sender feedback
    gen_tcp:send(Socket, lists:append(["[ -> ", DbReceiver#user.name, "] ", Text])),

    % receiver feedback
    gen_tcp:send(DbReceiver#user.socket, lists:append(["[ <- ", socketToUser(Socket), "] ", Text])).


getRooms(Socket) ->
    DbRooms = ets:tab2list(rooms),

    Message = "[#System] Actually there are those rooms: [",
    StringRooms =lists:map(
        fun(Room) -> string:concat(Room#room.name, " | ") end,
        DbRooms
    ),

    gen_tcp:send(Socket, string:concat(Message, string:concat(string:trim(StringRooms, trailing, " | "), "]"))).

createRoom(Socket, Room) ->
    % insert room into table 
    ets:insert(rooms, #room{name=Room, owner=socketToUser(Socket), clients=[Socket]}),

    % send feedback message to user
    gen_tcp:send(Socket, string:replace("[#System] You have created the room [*]", "*", Room)).