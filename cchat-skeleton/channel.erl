-module(channel).
-export([start/1,stop/1]).

% Start a new server process with the given name
start(ChannelName) ->
    PID = spawn(fun () -> loop([]) end),
    register(list_to_atom(ChannelName),PID).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ChannelName) ->
    % TODO Implement function
    % Return ok
    ChannelName ! {stop, self(), 0},
    ok.

loop(St) ->
  receive
    % Joining a client to the Channel
    % If the user is already on the state list it will send the atom user_already_joined
    % Else it will add the client to the state list and return ok back to the user
    {addClient, Ref, ClientId} ->
      case lists:member(ClientId, St) of
        true -> 
          ClientId ! {response, Ref, user_already_joined},
          loop(St);
        false ->
          AuxList = St ++ [ClientId],
          ClientId ! {response, Ref, ok},
          loop(AuxList)
      end;
    % Sending a message to clients inside a Channel
    % If the user is not inside the channel it will return user_not_joined
    % Else it will return the list of clients ids
    {send_message, Ref, ClientId, Msg, Nick, Channel} ->
      case lists:member(ClientId, St) of
        true ->
          ClientsWithoutSame = St -- [ClientId],
          ClientId ! {response, Ref, ok},
          NewRef = make_ref(),
          [ X ! {request, self(), NewRef, {message_receive, Channel, Nick, Msg}}  || X <- ClientsWithoutSame],
          loop(St);
        false ->
          ClientId ! {response, Ref, user_not_joined},
          loop(St)
      end;
    % Leaving a Channel
    % If the user is not inside the channel will return user_already_joined
    % Else it will remove the user and send back ok
    {leave_channel, Ref, ClientId} ->
      case lists:member(ClientId, St) of
        true -> 
          AuxList = St -- [ClientId],
          ClientId ! {response, Ref, ok},
          loop(AuxList);
        false ->
          ClientId ! {response, Ref, user_not_joined},
          loop(St)
      end;
    {stop, _From } ->
      ok
      %exit(ok)
  end.
