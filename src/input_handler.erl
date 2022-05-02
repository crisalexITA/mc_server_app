-module(input_handler).
-export([handleInput/2]).
-include("records.hrl").

%% @doc handle inputs from clients
handleInput(Input, Socket) ->

    StrippedInput = string:strip(Input, right, $\n),

    % check if user has been registered
    ClientRegistered = data_manager:clientRegistered(Socket),

    case ClientRegistered of
        
        % user registered -> parse command
        {true} -> 

            Command = parseCommand([{create_room, "^\\+room:\\w+"}, {exit_room, "^@@[A-Za-z0-9]{1,}:\\#exit"}, {enter_room, "^@@[A-Za-z0-9]{1,}:\\#enter"}, {send_private, "^@[A-Za-z0-9]{1,}:\\w+"}, {list_rooms, "\\?rooms"}, {help, "\\?help"}, {send_room, "^@@[A-Za-z0-9]{1,}:\\w+"}], StrippedInput),

            % do user action
            case Command of 

                % create room
                create_room -> createRoom(Socket, StrippedInput);

                % send private message
                send_private -> sendPrivate(Socket, StrippedInput);

                % send room message
                send_room -> sendRoom(Socket, StrippedInput);

                % exit room
                exit_room -> exitRoom(Socket, StrippedInput);
                
                % enter room
                enter_room -> enterRoom(Socket, StrippedInput);
                    
                % get room list
                list_rooms -> listRooms(Input, Socket);

                % get room list
                help -> help(Socket);

                % command not found
                no_match -> output_handler:sendMessageSystem(Socket, "command not found, type '?help' to get available commands")
           
            end;

        % user not registered yet -> ask to register
        {false} -> 
            
            Command = parseCommand([{register_user, "\\+user:\\w+"}], StrippedInput),

            case Command of

                % register user
                register_user -> registerUser(Socket, StrippedInput);
                    
                % ask client to insert username
                no_match ->  output_handler:sendMessageSystem(Socket, "Please enter your username with the command '+user:<username>'")

            end
    end.

%% @doc parse commands from clients and try to divide commands from messages  
parseCommand([], Input) ->
    
    no_match;

parseCommand([RegExp | RegExpsList], Input) ->
    
    case re:run(Input, element(2, RegExp)) of 
        {match, _c} -> element(1, RegExp);
        nomatch -> parseCommand(RegExpsList, Input)
    end.

%% @doc insert new user into user table and add it to 'main' room
registerUser(Socket, StrippedInput) ->
                    
    Username = getMessageFromInput(StrippedInput),

    %% register user
    data_manager:registerUser(Socket, Username),

    %% send response to user
    output_handler:sendMessageSystem(Socket, "You have been registered as " ++ Username),

    %% add user to main room
    data_manager:addUserRoom(Socket, "main"),

    %% informs the users of the room about the entrance of the new user
    output_handler:sendMessageRoom("SYSTEM", "main", Username ++ " joined the room").

%% @doc create a room
createRoom(Socket, StrippedInput) ->

    % get room from Input
    Room = getMessageFromInput(StrippedInput),

    Response = data_manager:createRoom(Socket, Room),

    case Response of 

        % room already existent
        ko ->
            % send message to user
            output_handler:sendMessageSystem(Socket, "Room '" ++ Room ++ "' already existent");
        
        % room created
        ok  ->
            % send message to user
            output_handler:sendMessageSystem(Socket, "Room '" ++ Room ++ "' created")
    end.

%% @doc add user to a room
enterRoom(Socket, StrippedInput) ->
    
    % get room from Input
    Room = getRecipientFromInput(StrippedInput),

    %% add user to main room
    Response = data_manager:addUserRoom(Socket, Room),

    case Response of 

        ko ->
            % send message to user
            output_handler:sendMessageSystem(Socket, "Room '" ++ Room ++ "' doesn't not exist");
        
        _ ->
            % get username from socket
            Username = data_manager:getUserFromSocket(Socket),

            % send message to all users connected to the room
            output_handler:sendMessageRoom("SYSTEM", Room, Username ++ " joined the room")

    end.

%% @doc send message to an user
sendPrivate(Socket, StrippedInput) ->

    % get username from socket
    Username = data_manager:getUserFromSocket(Socket),

    % get receiver name from Input
    RecipientUsername = getRecipientFromInput(StrippedInput),

    % get recipient socket from user
    RecipientSocket = data_manager:getSocketFromUser(RecipientUsername),

    case RecipientSocket of 

        % user not exists
        ko -> 
            % send message to user
            output_handler:sendMessageSystem(Socket, "User " ++ RecipientUsername ++ " doesn't not exist");

        % user exists
        _ -> 
            
            % get message from Input
            Message = getMessageFromInput(StrippedInput),
    
            % send message to user
            output_handler:sendMessagePrivate(Socket, Username, RecipientSocket, RecipientUsername, Message)
    end.

%% @doc send message to all users in a room
sendRoom(Socket, StrippedInput) ->

    % get room from Input
    RoomName = getRecipientFromInput(StrippedInput),

    % check if room exists
    Response = data_manager:getRoom(RoomName),

    case Response of 

        % room not exists
        ko ->
            % send message to user
            output_handler:sendMessageSystem(Socket, "Room '" ++ RoomName ++ "' not exists");
        
        % room exists
        _  ->
             % get username from socket
            Username = data_manager:getUserFromSocket(Socket),

            % get message from Input
            Message = getMessageFromInput(StrippedInput),

            % send message to all users connected to the room
            output_handler:sendMessageRoom(Username, RoomName, Message)
    end.

%% @doc remove user from a room
exitRoom(Socket, StrippedInput) ->
                
    % get room from Input
    Room = getRecipientFromInput(StrippedInput),

    %% add user to main room
    data_manager:removeUserRoom(Socket, Room),

    % get username from socket
    Username = data_manager:getUserFromSocket(Socket),

    % send message to all users connected to the room
    output_handler:sendMessageRoom("SYSTEM", Room, Username ++ " left the room"),

    % send feedback to user
    output_handler:sendMessageSystem(Socket, "You have left the room '" ++ Room ++ "'").

%% @doc get the list of active rooms
listRooms(Input, Socket) ->
    
    Rooms = data_manager:getRooms(),
    
    Message = "Actually there are those rooms: ",
    StringRooms = lists:map(
        
        fun(Room) -> 
            string:concat("'" ++ Room#room.name ++ "'", ",") 
        end,
        Rooms
    ),

    output_handler:sendMessageSystem(Socket, string:concat(Message, string:trim(StringRooms, trailing, ","))).
    
%% @doc get message from input
getMessageFromInput(Input) ->
    
    SplittedList = string:split(Input, ":"),
    Message = lists:last(SplittedList),
    Message.

%% @doc get recipient (user/room) from input
getRecipientFromInput(Input) ->
    
    SplittedList = string:split(Input, ":"),
    Recipient = hd(SplittedList),
    StrippedRecipient = string:strip(Recipient, left, $@),
    StrippedRecipient.

help(Socket) -> 
    Rooms = "\e[0;36m" ++ "?rooms" ++ "\e[0m" ++ " -> get a list of all available rooms\n",
    RegisterName = "\e[0;36m" ++ "+user:<name>" ++ "\e[0m" ++ " -> register your username\n",
    SendRoom = "\e[0;36m" ++ "@@<room name>:<message>" ++ "\e[0m" ++ " -> send a message in a room\n",
    SendPrivate = "\e[0;36m" ++ "@<user>:<message>" ++ "\e[0m" ++ " -> send a message to a user\n",
    CreateRoom = "\e[0;36m" ++ "+room:<room name>" ++ "\e[0m" ++ " -> create a new room\n",
    UsersRoom = "\e[0;36m" ++ "@@<room name>:#users" ++ "\e[0m" ++ " -> get a list of users in a room\n",
    ExitRoom = "\e[0;36m" ++ "@@<room name>:#exit" ++ "\e[0m" ++ " -> exit room\n",
    EnterRoom = "\e[0;36m" ++ "@@<room name>:#enter" ++ "\e[0m" ++ " -> enter room\n",
    output_handler:sendMessageSystem(Socket, "List of available command:\n" ++ Rooms ++ RegisterName ++ SendRoom ++ SendPrivate ++ CreateRoom ++ UsersRoom ++ ExitRoom ++ EnterRoom).






