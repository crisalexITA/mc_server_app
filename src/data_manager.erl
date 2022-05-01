-module(data_manager).
-export([clientRegistered/1, registerUser/2, addUserRoom/2, removeUserRoom/2, getRoom/1, getUserFromSocket/1, getSocketFromUser/1, createRoom/2, getRooms/0, init/0]).
-include("records.hrl").

init() ->
    % create db rooms table 
    ets:new(rooms, [named_table, public, {keypos, #room.name}]),
    % create db users table
    ets:new(users, [named_table, public, {keypos, #user.socket}]),
    % create public room
    ets:insert(rooms, #room{name="main", owner=""}).

%% @doc check if socket is linked to an username (registered)
clientRegistered(Socket) ->
    
    % get user linked to Socket from users table
    User = ets:lookup(users, Socket),

    Result = if
        User == [] -> {false};
        true -> {true}
    end,

    Result.

%% @doc insert into users table Socket, Username record
registerUser(Socket, Username) ->

    % insert into users table
    ets:insert(users, #user{name=Username, socket=Socket}),

    {ok}.

% @doc add user to room 
addUserRoom(Socket, RoomName) ->

     % check if room exists
    RoomExists = getRoom(RoomName),
    
    Result = case RoomExists of

        % room not existent
        ko -> ko;
            
        % room exist
        _ -> 
            Clients = RoomExists#room.clients ++ [Socket],
            % insert socket into socket list room record
            ets:insert(rooms, #room{name=RoomName, clients=Clients}),
            ok 
    end,
    Result.

% @doc get active rooms list 
getRooms() ->
    ets:tab2list(rooms).

% @doc remove user from room 
removeUserRoom(Socket, RoomName) ->

    % get room from rooms table
    RoomList = ets:lookup(rooms, RoomName),
    Result = if
        
        RoomList == [] -> {ko};
        
        true -> 
            
            Room = hd(RoomList),
            Clients = Room#room.clients -- [Socket],
            
            % insert socket into socket list room record
            ets:insert(rooms, #room{name=RoomName, clients=Clients}),

            {ok}
    end,

    Result.

% @doc get room by name
getRoom(Name) ->

    RoomList = ets:lookup(rooms, Name),

    Result = if
        
        RoomList == [] -> ko;
        
        true -> 
            Room = hd(RoomList),
            Room
    end,
    Result.


% @doc get user name from socket
getUserFromSocket(Socket) ->

    % get user record by socket
    UserList = ets:lookup(users, Socket),

    Result = if
        
        UserList == [] -> {ko};
        
        true ->
            User = hd(UserList),
            Username = User#user.name,
            Username
    end,

    Result.

% @doc get user name from socket
getSocketFromUser(Username) ->

    % get user record by name
    UserList = ets:match_object(users, #user{_='_', name=Username}),

    Result = if
        
        UserList == [] -> ko;
        
        true ->
            User = hd(UserList),
            Socket = User#user.socket,
            Socket
    end,

    Result.


% create new room
createRoom(Socket, Room) ->
    
    % check if room exists
    RoomExists = getRoom(Room),
    
    Result = case RoomExists of

        % room not existent -> create it
        ko -> 
            % insert room into table
            ets:insert(rooms, #room{name=Room, owner=Socket, clients=[Socket]}),
            ok;

        % room already existent
        _ -> ko
    
    end,
    Result.









            
            

            
