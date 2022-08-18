-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    % Make unique reference
    Ref = make_ref(),
    % Send request to server, in case it is not reachable it will return error
    % If reachable sends ok if the server was able to join him otherwise returns user already joined
    case catch St#client_st.server ! {join, self(), Ref, Channel, St#client_st.nick } of
        {'EXIT', _ } -> {reply, {error, server_not_reached, "server_not_reached"}, St};
        _ -> 
            receive 
                { response, Ref, Result } -> 
                    case Result of
                        ok -> {reply, ok, St};
                        user_already_joined -> {reply, {error, Result, "user_already_joined"}, St} 
                    end
            after 
                500 ->
                    {reply, {error, server_not_reached, "server_not_reached"}, St}
            end
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    ChannelAux = lists:delete($#, Channel),
    % Make unique reference
    Ref = make_ref(),
    % Sends request to channel, if channel server no reachble sends exit message
    % If server was reachable then sends back ok it was able to leave or if was no part of the channel returns error
    case catch list_to_atom(ChannelAux) ! {leave_channel, Ref, self() } of
        {'EXIT', _ } -> {reply, {error, server_not_reached, "server_not_reached"}, St};
        _ -> 
            receive 
                { response, Ref, Result } -> 
                    case Result of
                        ok -> {reply, ok, St};
                        user_not_joined -> {reply, {error, Result, "user_not_joined"}, St} 
                    end
            after 
                2000 ->
                    {reply, {error, server_not_reached, "server_not_reached"}, St}
            end
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    ChannelAux = lists:delete($#, Channel),
    % Make unique reference
    Ref = make_ref(),
    % Sends request to channel, if channel server no reachble sends exit message
    % If server was reachable then sends back ok it was able to send message or if was no part of the channel returns error
    case catch list_to_atom(ChannelAux) ! {send_message, Ref, self(), Msg, St#client_st.nick, Channel} of
        {'EXIT', _ } -> {reply, {error, server_not_reached, "server_not_reached"}, St};
        _ -> 
            receive 
                { response, Ref, Result } -> 
                    case Result of
                        ok -> {reply, ok, St};
                        user_not_joined -> {reply, {error, Result, "user_not_joined"}, St} 
                    end
            after 
                2000 ->
                    {reply, {error, server_not_reached, "server_not_reached"}, St}
            end
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    Ref = make_ref(),
    case catch St#client_st.server ! {checkNickname, self(), Ref, NewNick, St#client_st.nick} of
        {'EXIT', _ } -> {reply, {error, server_not_reached, "server_not_reached"}, St};
        _ -> 
            receive 
                { response, Ref, Result } -> 
                    case Result of
                        ok -> {reply, ok, St#client_st{nick = NewNick}};
                        nick_taken -> {reply, {error, Result, "nick_taken"}, St} 
                    end
            after 
                2000 ->
                    {reply, {error, server_not_reached, "server_not_reached"}, St}
            end
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
