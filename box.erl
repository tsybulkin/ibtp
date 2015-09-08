% simulator of interbox transport protocol
%
% Cloudozer 2015


-module(box).
-export([new/2
		]).

-define(BOOF_SIZE,8).
-define(BPS,10000). % 1000 bit per second
-define(APL,3000). % average packet length
-define(Q_READS,4). % number of successful queue reads


new(Box,Traff) -> % Traff - mean traffic in packets per second a given box is generating during simulation
	io:format("Box: ~p started.~n",[Box]),
	receive
		{neibs,Neibs} -> 
			io:format("Got message about neighbors: ~p~n",[Neibs]),
			Ports = lists:foldl(fun({B,P,Pid},Ports)-> 
									Boof_in = Boof_out = queue:new(),
									Cs = 0, Cg = ?BOOF_SIZE, NCs = 0,
									dict:store(P,{B,Pid,1,Boof_in,Boof_out,Cs,Cg,NCs},Ports) 
								end,dict:new(),Neibs),
			Q = queue:new(),
			BD = [],
			RTab = dict:new(),
			cycle(Box,Ports,RTab,Q,BD,Traff)
	end.



cycle(Box,Ports0,RTab,Q,BD,Traff) ->
	Ports = lists:foldl(fun(Port,Dict) ->
		{B,Pid,P,Boof_in,Boof_out,Cs,Cg,NCs} = dict:fetch(Port,Dict),
		case queue:out(Boof_out) of
			{{value,Pkt},Boof_out1} ->
				Pid ! {P,Pkt}, 
				dict:store(Port,{B,Pid,P,Boof_in,Boof_out1,Cs-1,Cg,NCs},Dict);
			{empty,Boof_out} -> Dict
		end
	end,Ports0,dict:fetch_keys(Ports0)),

	Tick = 1000*?APL/?BPS, % in milliseconds
	receive
		quit -> io:format("Box: ~p terminated~n",[Box]);
		{Port,Pkt} ->
			{B,Pid,P,Boof_in,Boof_out,Cs,Cg,NCs} = dict:fetch(Port,Ports),
			Ports1 = dict:store(Port,{B,Pid,P,queue:in(Pkt,Boof_in),Boof_out,Cs,Cg-1,NCs-1},Ports),
			cycle(Box,Ports1,RTab,Q,BD,Traff)
	after
		Tick -> free_queue(Box,Ports,RTab,Q,BD,Traff,?Q_READS)
	end.


free_queue(Box,Ports,RTab,Q,BD,Traff,0) -> cycle(Box,Ports,RTab,Q,BD,Traff);
free_queue(Box,Ports,RTab,Q,BD,Traff,Qreads) ->
	case queue:out(Q) of
		{{value,Pkt={Dest,_Source,_N,_}},Q1} ->	
			case get_route(Dest,RTab) of
				no_route -> multiplex(Box,Ports,RTab,queue:in(Pkt,Q1),BD,Traff);
				Route -> 
					{B,Pid,P,Boof_in,Boof_out,Cs,Cg,NCs} = dict:fetch(Route,Ports),
					Ports1 = dict:store(Route,{B,Pid,P,Boof_in,queue:in(Pkt,Boof_out),Cs-1,Cg,NCs},Ports),
					free_queue(Box,Ports1,RTab,Q1,BD,Traff,Qreads-1)
			end;
		{empty,Q} -> multiplex(Box,Ports,RTab,Q,BD,Traff)		
	end.


multiplex(Box,Ports,RTab,Q,BD,Traff) -> cycle(Box,Ports,RTab,Q,BD,Traff).	



get_route(Dest,RTab) ->
	case dict:is_key(Dest,RTab) of
		true -> [Route|_] = dict:fetch(Dest,RTab), Route;
		false-> no_route
	end.

