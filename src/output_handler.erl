-module(output_handler).
-export([sendMessageRoom/3, sendMessagePrivate/5, sendMessageSystem/2]).
-include("records.hrl").

% @doc send message to room
sendMessageRoom(Sender, RoomName, Message) ->

    Room = data_manager:getRoom(RoomName),

    Clients = Room#room.clients,

    lists:foreach(
        fun(Client) ->          
            gen_tcp:send(Client, "[" ++ Sender ++  " # " ++  RoomName ++ "] " ++ Message ++ "\n")
        end,
        Clients
    ).

% @doc send private message to user
sendMessagePrivate(Socket, Username, RecipientSocket, RecipentUsername, Message) ->

    % sender message
    gen_tcp:send(Socket, "[ -> " ++  RecipentUsername  ++ "] " ++ Message ++ "\n"),

    % receiver message
    gen_tcp:send(RecipientSocket, "[ <- " ++  Username  ++ "] " ++ Message ++ "\n").

% @doc send system message to user
sendMessageSystem(Socket, Message) ->
    
    % send message
    gen_tcp:send(Socket, "[SYSTEM] " ++ Message ++ "\n").

    