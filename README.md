mc chat
=====


Server
-----

TCP server for chat management. It is possible to send private messages to users, create your own rooms, enter and exit rooms and send private messages between users.

![system](https://github.com/crisalexITA/mc_server_app/blob/main/assets/system.png?raw=true)

The server is made of four modules that interface with each other:
* **TCP listener (mc_server_app.erl):** manages incoming client connections and creates the processes (spawn) that will take care of them
* **input handler (input_handler.erl):** parses inputs and executes commands
* **data manager (data_manager.erl):** interacts with the database for data management (insertions, changes, deletions)
* **output hanndler (output_handler.erl):** takes care of sending messages to clients


Client 
-----
It is possible to communicate with the server via shell commands

| Command        | Description     | Example |
|-|-----------|------------|
|**?help** |return the list of available commands|_?help_|
|**?rooms**|return the list of available rooms|_?rooms_|
|**@@\<room name\>:#users**|return the list of users in a room|_@@green room:#users_|
|**+room:\<room name\>**|create a new room |_+room:blue room_|
|**@@\<room name\>:#enter**|the user joins the room and will receive all messages sent in that room|_@@yellow room:#enter_|
|**@@\<room name\>:#exit**|the user leaves the room and will no longer receive messages from that roomm|_@@yellow room:#exit_|
|**@@\<room name\>:\<message\>**|the user sends the message to all users in the room|_@@blue room:Hi all!_|
|**@\<user name\>:\<message\>**|the user sends a private message to another user|_@Bart:Hi! How are you?_|
|**+user:\<user name\>**|the user registers his username|_+user:Bart_|
|**#quit**|the user leaves the chat|_#quit_|

Database
-----
The data is stored via ETS in the following tables:
- **room**: table containing all the server rooms (system rooms and those created by users); is made up of the following fields: 
    - **name**: room name
    - **owner**: room creator user (socket)
    - **clients**: list of sockets connected to the room
    
- **users**: table containing the users connected to the server, consisting of the following fields:
    - **name**: username of the user
    - **socket**: socket associated with the user (it is also the user's identifier)Database

Use
-----
To use the chat do the following steps:
1. **clone server app:** https://github.com/crisalexITA/mc_server_app
2. **start server:** open an os shell and go to server app folder and run _rebar3 shell_
3. **start client:** open an os shell and connect to the server by _nc localhost 6666_
4. **register username:** from client set user nickname with _+user:\<user name\>_
5. **execute commands:** use one of the commands specified above 

_* steps from 3 to 5 can be repeated several times to create more clients_


Technical Notes
-----
developed on linux platform (Ubuntu 20.04) with Erlang/OTP 22.3 and rebar3 in 'application' mode


