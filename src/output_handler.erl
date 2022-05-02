-module(output_handler).
-export([sendMessageRoom/3, sendMessagePrivate/5, sendMessageSystem/2]).
-include("records.hrl").

% @doc send message to room
sendMessageRoom(Sender, RoomName, Message) ->

    Room = data_manager:getRoom(RoomName),

    Clients = Room#room.clients,

    lists:foreach(
        fun(Client) ->          
            gen_tcp:send(Client, "\e[0;34m" ++ "[" ++ Sender ++  " # " ++  RoomName ++ "] " ++ "\e[0m" ++ Message ++ "\n")
        end,
        Clients
    ).

% @doc send private message to user
sendMessagePrivate(Socket, Username, RecipientSocket, RecipentUsername, Message) ->

    % sender message
    gen_tcp:send(Socket, "\e[0;32m" ++ "[ -> " ++  RecipentUsername  ++ "] " ++ "\e[0m" ++ Message ++ "\n"),

    % receiver message
    gen_tcp:send(RecipientSocket, "\e[0;32m" ++ "[ <- " ++  Username  ++ "] "  ++ "\e[0m"  ++ Message ++ "\n").

% @doc send system message to user
sendMessageSystem(Socket, Message) ->
    
    % send message
    gen_tcp:send(Socket, "\e[0;33m" ++ "[SYSTEM] " ++ "\e[0m" ++ Message ++ "\n").

    