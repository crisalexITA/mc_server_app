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
    
    data_manager:init(),
    
    spawn(fun() -> handleClient(ListenSocket) end),
    timer:sleep(infinity).

handleClient(ListenSocket) -> 
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handleClient(ListenSocket) end),
    
    %  welcome message
    gen_tcp:send(AcceptSocket, "\e[31m" ++ "\n\n**************************************\nchat server\n***************************************\n" ++ "\e[33m" ++ "[SYSTEM] " ++ "\e[0m" ++ "Welcome, please enter your name with command '+user:<username>'\n"),

    handleMessages(AcceptSocket).

% handle incoming message loop
handleMessages(AcceptSocket) ->
    inet:setopts(AcceptSocket, [{active, true}]),
    receive
        {tcp, Socket, Data} ->
            
            % log input
            io:format("[LOG] ~p : ~p ~n", [Socket, Data]),

            % read input from clients and do actions
            input_handler:handleInput(binary_to_list(Data), Socket),
            
            % continue loop
            handleMessages(AcceptSocket)
    end.

quit(Socket) ->
    
    % remove user from rooms
    Rooms = ets:tab2list(rooms),
    lists:foreach(
        fun(Room) -> 
            io:format("~p", [Room#room.name])
        end,
        Rooms
    ),

    % remove user from users
    ets:delete(users, Socket).

