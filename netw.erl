% simulator of interbox transport protocol
%
% Cloudozer 2015


-module(netw).
-export([gen_rand_net/1, setup/3
		]).

-define(PORTS_NBR,4).
-define(MAX_RAND_ATTEMPTS,5).
-define(MAX_LATENCY,100).



setup(N,Traff,Msg_len) ->
	random:seed(),
	G = gen_rand_net(N),
	Bs = lists:foldl(fun(B,Acc) -> [ {B, spawn(box,new,[B,Traff]) } |Acc] 
					end,[],dict:fetch_keys(G)),
	io:format("Bs: ~p~n",[Bs]),
	lists:foreach(  fun({B,Pid}) -> 
						Links = dict:fetch(B,G),
						io:format("Links:~n~p~n",[Links]),
						Neibs = [ begin
									{B2,Bid}=lists:keyfind(B2,1,Bs),
									{B2,P,Lat,Bid}
								  end || {B2,P,Lat} <- Links ],
						Pid ! {neibs, Neibs} 
					end, Bs),
	
	%% compute the shortest path between the first and the second boxes
	%[{B1,_},{B2,_}|_] = Bs,
	%SP12 = shortest_path(B1,B2,G),
	%io:format("Shortes path between ~p and ~p:~n~p~n",[B1,B2,SP12]),

	%% simalate message passing between box1 and box2 via the shortest path


	%% compute the routes from each box to box2

	%% simulate message passing between box1 and box using ibtp protocol


	%% shutdown 
	lists:foreach(  fun({_B,Pid}) -> Pid ! quit
					end,Bs).



gen_rand_net(0) -> no_network;
gen_rand_net(N) ->
	M = round(0.3 *?PORTS_NBR*N + random:uniform(round(0.2*?PORTS_NBR*N))),
	io:format("Generating ~w wires~n",[M]),

	Net = dict:from_list([ {get_box(J),[]} || J <- lists:seq(1,N) ]),
	wire_boxes(N,M,Net).



get_box(J) -> "box "++ integer_to_list(J).



wire_boxes(_,0,Net) -> Net;
wire_boxes(N,M,Net) ->
	case get_random_wire(N,Net,0) of
		{Box1,Box2} ->
			P1 = 1 + length(dict:fetch(Box1,Net)),
			P2 = 1 + length(dict:fetch(Box2,Net)),
			Lat = get_rand_latency(),
			Net1 = dict:append(Box1,{Box2,P1,Lat},dict:append(Box2,{Box1,P2,Lat},Net)),
			wire_boxes(N,M-1,Net1);
		false ->
			io:format("~p wires could not be used~n",[M]),
			wire_boxes(N,0,Net)
	end.



get_random_wire(_,_,?MAX_RAND_ATTEMPTS) -> false;
get_random_wire(N,Net,Attempt) ->
	B1 = get_box(random:uniform(N)),
	B2 = get_box(random:uniform(N)),
	case length(dict:fetch(B1,Net))>=?PORTS_NBR orelse length(dict:fetch(B2,Net))>=?PORTS_NBR of
		true -> get_random_wire(N,Net,Attempt+1);
		false -> case B1=:=B2 of
					true -> get_random_wire(N,Net,Attempt+1);
					false ->{B1,B2}
				end
	end.



get_rand_latency() -> random:uniform(?MAX_LATENCY div 2) + (?MAX_LATENCY div 2).



