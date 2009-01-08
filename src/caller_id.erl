%% @author Maxim Treskin <zerthurd@gmail.com>
%% @copyright 2009
%% @version {@date} {@time}
%% @end

%% @doc <strong>{@module}</strong> - Caller ID (ETS 300 659-1) parsing

-module(caller_id).

-author('Maxim Treskin <zerthurd@gmail.com>').

-export([cid_parse/1]).

-record(dt, {			%% Date and Time
		  month,
		  day,
		  hour,
		  minute
		 }).
-record(cl, {number}).		%% Calling Line Identity
-record(cd, {number}).		%% Called Line Identity
-record(cl_ra, {reason}).	%% Reason for Absence of Calling Line Identity
-record(cpn, {name}).		%% Calling Party Name
-record(cpn_ra, {reason}).	%% Reason for absence of Calling Party Name
-record(vi, {state}).		%% Visual Indicator
-record(ccli, {number}).	%% Complementary Calling Line Identity
-record(ct, {type}).		%% Call type
-record(fcdli, {number}).	%% First Called Line Identity (in case of forwarded call)
-record(nmss, {status}).	%% Network Message System Status
-record(fct, {type}).		%% Type of Forwarded call (in case of forwarded call)
-record(cut, {type}).		%% Type of Calling user
-record(rn, {number}).		%% Redirecting Number (in case of forwarded call)

-define(SDMF, 16#04).
-define(MDMF, 16#80).

-define(type_dt, 16#01).
-define(type_cl, 16#02).
-define(type_cd, 16#03).
-define(type_cl_ra, 16#04).
-define(type_cpn, 16#07).
-define(type_cpn_ra, 16#08).
-define(type_vi, 16#0B).
-define(type_ccli, 16#10).
-define(type_ct, 16#11).
-define(type_fcdli, 16#12).
-define(type_nmss, 16#13).
-define(type_fct, 16#15).
-define(type_cut, 16#16).
-define(type_rn, 16#1A).
%%-define(type_charge, 16#20).
%%-define(type_dt, 16#E0).
%%-define(type_dt, 16#E1).

%% @spec (CID) ->
%%  [Chunk] | {bad_msg_type, Type} | {bad_checksum, integer(), integer()}
%%    CID = string()
%%    Chunk = #dt{} | #cl{} | #cd{} | #cl_ra{} |
%%            #cpn{} | #cpn_ra{} | #vi{} | #ccli{} |
%%            #ct{} | #fcdli{} | #nmss{} | #fct{} |
%%            #cut{} | #rn{}
%%    Type = integer()
%% @doc Parse caller ID to chunks
cid_parse(CID) when is_list(CID) ->
    [CS|D] = lists:reverse(list_prep(CID)),
    Data = lists:reverse(D),
    case (lists:sum(Data) + CS) rem 16#100  of
		0 ->
			case Data of
				[?SDMF, Len | Rest] ->
					sdmf(Len, Rest);
				[?MDMF, Len | Rest] ->
					mdmf(Len, Rest);
				[Other, _] ->
					{bad_msg_type, Other}
			end;
		OCS ->
			{bad_checksum, CS, OCS}
    end;
cid_parse(_) ->
    undefined.

-define(DT_LEN, 8).

sdmf(_Len, Rest) ->
    {DT, Num} = lists:split(?DT_LEN, Rest),
    [parse_dt(DT), parse_cl(Num)].

mdmf(_Len, Rest) ->
    do_blocks(Rest).

do_blocks(Msg) ->
    do_blocks(Msg, []).

do_blocks([], Acc) ->
    lists:reverse(Acc);
do_blocks(Msg, Acc) ->
    [Type, Len | Rest] = Msg,
    {Data, Rest1} = lists:split(Len, Rest),
    Res = parse_block(Type, Len, Data),
    do_blocks(Rest1, [Res|Acc]).

parse_block(Type, _Len, Data) ->
    case Type of
		?type_dt -> parse_dt(Data);
		?type_cl -> parse_cl(Data);
		?type_cd -> parse_cd(Data);
		?type_cl_ra -> parse_cl_ra(Data);
		?type_cpn -> parse_cpn(Data);
		?type_cpn_ra -> parse_cpn_ra(Data);
		?type_vi -> parse_vi(Data);
		?type_ccli -> parse_ccli(Data);
		?type_ct -> parse_ct(Data);
		?type_fcdli -> parse_fcdli(Data);
		?type_nmss -> parse_nmss(Data);
		?type_fct -> parse_fct(Data);
		?type_cut -> parse_cut(Data);
		?type_rn -> parse_rn(Data);
		_ -> {bad_type, Type}
    end.

%% Parsers of parameter types

parse_dt(DT) ->
    case DT of
		[Mon1, Mon2, D1, D2, H1, H2, M1, M2] ->
			#dt{month = [Mon1, Mon2],
				day = [D1, D2],
				hour = [H1, H2],
				minute = [M1, M2]
			   };
		_ ->
			undefined
    end.

parse_cl(Data) ->
    #cl{number = Data}.

parse_cd(Data) ->
    #cd{number = Data}.

parse_cpn(Data) ->
    #cpn{name = Data}.

parse_cl_ra([H|_]) ->
    Rsn =
		case H of
			$O -> unavailable;			%% Unavailable
			$P -> private;			%% Private (CLIR involved)
			_ -> unknown
		end,
    #cl_ra{reason = Rsn}.


parse_cpn_ra([H|_]) ->
    Rsn =
		case H of
			$O -> unavailable;			%% Unavailable
			$P -> private;			%% Private (Name delivery has been blocked)
			_ -> unknown
		end,
    #cpn_ra{reason = Rsn}.

parse_vi(Data) ->
    State =
		case Data of
			16#00 -> off;			%% Deactivation (indicator off)
			16#FF -> on;			%% Activation (indicator on)
			_ -> unknown
		end,
    #vi{state = State}.

parse_ccli(Data) ->
    #ccli{number = Data}.

parse_ct([H|_]) ->
    Type =
		case H of
			16#01 -> voice_call;		%% Voice Call
			16#02 -> ringback_freecall;		%% CLI Ring Back when free call
			16#03 -> cn_delivery;		%% Calling Name Delivery
			16#81 -> msg_wait_call;		%% Message Waiting Call
			_ -> unknown			%% Reserved for network operator use
		end,
    #ct{type = Type}.

parse_fcdli(Data) ->
    #fcdli{number = Data}.

parse_nmss([H|_]) ->
    Status =
		case H of
			16#00 -> no;			%% No messages
			16#01 -> one;			%% 1 message or unspecified number of message waiting
			_ -> unknown
		end,
    #nmss{status = Status}.

parse_fct([H|_]) ->
    Type =
		case H of
			16#00 -> unavailable;		%% Unavailable or unknown forwarded call type
			16#01 -> fc_busy;			%% Forwarded call on busy
			16#02 -> fc_noreply;		%% Forwarded call on no reply
			16#03 -> fc_uncond;			%% Unconditional forwarded call
			16#04 -> defl_alert;		%% Deflected call (after alerting)
			16#05 -> defl_immed;		%% Deflected call (immediate)
			16#06 -> fc_inability;		%% Forwarded call on inability to reach mobile subscriber
			_ -> unknown
		end,
    #fct{type = Type}.

parse_cut([H|_]) ->
    Type =
		case H of
			16#00 -> no;
			16#01 -> one;
			_ -> unknown
		end,
    #cut{type = Type}.

parse_rn(Data) ->
    #rn{number = Data}.

%% Convert CID string to hexadecimal base list
list_prep(CID) ->
    list_prep([], CID).
list_prep(H, []) ->
    H;
list_prep(H, T) ->
    {H1, T1} = lists:split(2, T),
    Num = erlang:list_to_integer(H1, 16),
    list_prep(H ++ [Num], T1).
