% simulator of interbox transport protocol
%
% Cloudozer 2015


-module(net).
-export([gen_rand_net/1, setup/4
		]).

-define(PORTS_NBR,4).
-define(MAX_RAND_ATTEMPTS,5).



setup(N,Lat,Traff,Msg_len) ->
	G = gen_rand_net(N),
	Bs = lists:foldl(fun(B,Acc) -> [ {B, spawn(box,new,[B,dict:fetch(B,G),Lat,Traff]) } |Acc] 
					end,[],dict:fetch_keys(G)),

	lists:foreach(  fun({B,Pid}) -> 
						Links = dict:fetch(B,G),
						Neibs = [ begin 
									{B2,B2_pid} = lists:keyfind(B2,1,Bs),
									{P1,P2,B2,B2_pid} 
								  end || {P1,P2,B2} <- Links ],
						Pid ! {neibs, Neibs} 
					end, Bs),
	
	%% compute shortest path between the first and the second boxes

	%% simalate message passing between box1 and box2 via the shortest path


	%% compute the routes from each box to box2

	%% simulate message passing between box1 and box using ibtp protocol


	%% shutdown 
	lists:foreach(  fun({_B,Pid}) -> Pid ! quit
					end,Bs).



gen_rand_net(0) -> no_network;
gen_rand_net(N) ->
	crypto:start(),
	M = round(0.3 *?PORTS_NBR*N + random:uniform(round(0.2*?PORTS_NBR*N))),
	io:format("Generating ~w wires~n",[M]),

	Boxes = lists:foldl(fun(_,Acc)-> 
				[K|_] = MACs = [ get_mac() || _ <- lists:seq(1,?PORTS_NBR) ],
				[{K,MACs}|Acc]
						end,[],lists:seq(1,N)),

	wire_boxes(M,dict:new(),Boxes).



wire_boxes(0,Net,_) -> Net;
wire_boxes(M,Net,Boxes) ->
	case get_random_wire(Boxes,0) of
		{{Box1,P1,P2,Box2}, Boxes1} ->
			Net1 = dict:append(Box1,{P1,P2,Box2},dict:append(Box2,{P2,P1,Box1},Net)),
			wire_boxes(M-1,Net1,Boxes1);
		false ->
			io:format("~p wires could not be used~n",[M]),
			wire_boxes(0,Net,Boxes)
	end.



get_random_wire(_,?MAX_RAND_ATTEMPTS) -> false;
get_random_wire(Boxes,K) ->
	N = length(Boxes),
	case N < 2 of
		true -> false;
		false ->
			J1 = random:uniform(N),
			{Box1,Ports1} = lists:nth(J1,Boxes),
			J2 = random:uniform(N),
			{Box2,Ports2} = lists:nth(J2,Boxes),
			
			case Box1 =:= Box2 of
				true -> get_random_wire(Boxes,K+1);
				false-> 
					[P1|Ports11] = Ports1,
					[P2|Ports22] = Ports2,
					case {Ports11,Ports22} of
						{[],[]} -> Boxes1 = lists:keydelete(Box2,1,lists:keydelete(Box1,1,Boxes) );
						{[],_} -> Boxes1 = lists:keyreplace(Box2,1,lists:keydelete(Box1,1,Boxes),{Box2,Ports22} );
						{_,[]} -> Boxes1 = lists:keyreplace(Box1,1,lists:keydelete(Box2,1,Boxes),{Box1,Ports11} );
						{_,_} -> Boxes1 = lists:keyreplace(Box1,1,lists:keyreplace(Box2,1,Boxes,{Box2,Ports22}),{Box1,Ports11} )
					end,
					{{Box1,P1,P2,Box2}, Boxes1}
			end
	end.



get_mac() -> crypto:strong_rand_bytes(6).

