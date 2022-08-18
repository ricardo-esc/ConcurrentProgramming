-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    PID = spawn(fun () -> loop([], []) end),
    case whereis(ServerAtom) of
        undefined -> nil;
        _ -> unregister(ServerAtom)
    end,

    register(ServerAtom,PID),
    PID.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    Ref = make_ref(),
    ServerAtom ! { stop, self(), Ref },
    receive 
        { response, Ref } -> ok
    end.

loop(St, NickLists) ->
    receive
        % Try to make the user join a channel
        {join, From , Ref, Channel, Nick} ->
            % Remove the '#' from the channel to manipulate it as an atom 
            ChannelAux = lists:delete($#, Channel),
            % Check if the channel already exists
            % If it exists will send the client id to the Channel Process to try adding thr client
            % Else it will create the Channel Process and then will send to that process the client Id
            case lists:member(ChannelAux, St) of
                true -> 
                    list_to_atom(ChannelAux) ! {addClient, Ref, From},
                    case lists:member(Nick, NickLists) of
                        false -> 
                            AuxNicks = NickLists ++ [Nick],
                            loop(St, AuxNicks);
                        true ->  loop(St, NickLists)
                    end;
                false ->
                    channel:start(ChannelAux),
                    % Add to the Channel List the new Channel
                    AuxList = St ++ [ChannelAux],
                    list_to_atom(ChannelAux) ! {addClient, Ref, From},
                    case lists:member(Nick, NickLists) of
                        false -> 
                            AuxNicks = NickLists ++ [Nick],
                            loop(AuxList, AuxNicks);
                        true ->  loop(AuxList, NickLists)
                    end
            end;
        {checkNickname, From, Ref, Nick, OldNick} ->
            case lists:member(Nick, NickLists) of
                true ->
                    From ! {response, Ref, nick_taken},
                    loop(St, NickLists);
                false -> 
                    AuxNick = NickLists ++ [Nick],
                    AuxNick2 = AuxNick -- [OldNick],
                    From ! {response, Ref, ok},
                    loop(St, AuxNick2) 
                end;
        {stop, _From, _Ref} ->
            [ list_to_atom(X) ! { stop, self() }  || X <- St],
            _From ! {response, _Ref},
            ok
    end.
    


