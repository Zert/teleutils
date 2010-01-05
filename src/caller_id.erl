%% @author Maxim Treskin <zerthurd@gmail.com>
%% @copyright 2009
%% @version {@date} {@time}
%% @end

%% @doc <strong>{@module}</strong> - Caller ID (ETS 300 659-1) parsing
%% @headerfile "caller_id.hrl"

-module(caller_id).

-author('Maxim Treskin <zerthurd@gmail.com>').

-include("caller_id.hrl").

-export([
         cid_parse/1,
         cid_compose/1,
         cid_compose/2
        ]).

%% Example
%%
%% MDMF: "80160108303731313130333602073731353130303007014F10"
%% SDMF: "0412303731313130333637313531303030F9"
%%
%% 2> MC = caller_id:cid_parse("80160108303731313130333602073731353130303007014F10").
%% [{dt,"07","11","10","36"},{cl,"7151000"},{cpn,"O"}]
%% 3> SC = caller_id:cid_parse("0412303731313130333637313531303030F9").
%% [{dt,"07","11","10","36"},{cl,"7151000"}]
%% 4> caller_id:cid_compose(mdmf, MC).
%% "80160108303731313130333602073731353130303007014F10"
%% 5> caller_id:cid_compose(sdmf, SC).
%% "0412303731313130333637313531303030F9"


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

%% @spec ([Chunk]) ->
%%  CID
%%    CID = string()
%%    Chunk = #dt{} | #cl{} | #cd{} | #cl_ra{} |
%%            #cpn{} | #cpn_ra{} | #vi{} | #ccli{} |
%%            #ct{} | #fcdli{} | #nmss{} | #fct{} |
%%            #cut{} | #rn{}
%% @doc Compose caller ID from chunks (MDMF)
cid_compose(L)
  when is_list(L) ->
    cid_compose(mdmf, L).

%% @spec (Type, [Chunk]) ->
%%  CID
%%    CID = string()
%%    Type = sdmf | mdmf
%%    Chunk = #dt{} | #cl{} | #cd{} | #cl_ra{} |
%%            #cpn{} | #cpn_ra{} | #vi{} | #ccli{} |
%%            #ct{} | #fcdli{} | #nmss{} | #fct{} |
%%            #cut{} | #rn{}
%% @doc Compose caller ID from chunks
cid_compose(sdmf, L) ->
    compose_blocks(L);
cid_compose(mdmf, L) ->
    compose_blocks(L, []).

%% SDMF
compose_blocks(SL) ->
    DtL = lists:keyfind(dt, 1, SL),
    ClL = lists:keyfind(cl, 1, SL),
    L1 = lists:concat([compose_block(?SDMF, DtL), compose_block(?SDMF, ClL)]),
    L2 = lists:concat([[?SDMF], L1]),
    cs_append(L2).

%% MDMF
compose_blocks([], Acc) ->
    L1 = lists:concat(lists:reverse(Acc)),
    Len = length(L1),
    L2 = lists:concat([[?MDMF, Len], L1]),
    cs_append(L2);
compose_blocks([H|T], Acc) ->
    case compose_block(?MDMF, H) of
        undefined ->
            compose_blocks(T, Acc);
        List ->
            compose_blocks(T, [List | Acc])
    end.

cs_append(List) ->
    CS = 16#100 - (lists:sum(List) rem 16#100),
    lists:concat([lists:flatten(io_lib:format("~2.16.0B", [P])) || P <- lists:concat([List, [CS]])]).


-define(DT_LEN, 8).

compose_block(?SDMF, Struct) ->
    case struct_flat(Struct) of
        {?type_dt, Data} ->
            [(?DT_LEN+1)*2 | Data];
        {?type_cl, Data} ->
            Data;
        _ -> undefined
    end;

compose_block(?MDMF, Struct) ->
    case struct_flat(Struct) of
        {Type, Data} ->
            Len = length(Data),
            [Type, Len | Data];
        _ -> undefined
    end.


struct_flat(#dt{month = [Mon1, Mon2],
                day = [D1, D2],
                hour = [H1, H2],
                minute = [M1, M2]}) ->
    {?type_dt, [Mon1, Mon2, D1, D2, H1, H2, M1, M2]};
struct_flat(#cl{number = Number}) ->
    {?type_cl, Number};
struct_flat(#cl_ra{reason = Reason}) ->
    {?type_cl_ra, Reason};
struct_flat(#cpn{name = Name}) ->
    {?type_cpn, Name};
struct_flat(#cpn_ra{reason = Reason}) ->
    {?type_cpn_ra, Reason};
struct_flat(#cd{number = Number}) ->
    {?type_cd, Number};
struct_flat(#vi{state = State}) ->
    {?type_vi, State};
struct_flat(#ccli{number = Number}) ->
    {?type_ccli, Number};
struct_flat(#ct{type = Type}) ->
    {?type_ct, Type};
struct_flat(#fcdli{number = Number}) ->
    {?type_fcdli, Number};
struct_flat(#nmss{status = Status}) ->
    {?type_nmss, Status};
struct_flat(#fct{type = Type}) ->
    {?type_fct, Type};
struct_flat(#cut{type = Type}) ->
    {?type_cut, Type};
struct_flat(#rn{number = Number}) ->
    {?type_rn, Number};
struct_flat(_) ->
    undefined.

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

parse_cl_ra(<<H:8,_/binary>>) ->
    Rsn =
        case H of
            $O -> unavailable; %% Unavailable
            $P -> private;     %% Private (CLIR involved)
            _ -> unknown
        end,
    #cl_ra{reason = Rsn}.


parse_cpn_ra(<<H:8,_/binary>>) ->
    Rsn =
        case H of
            $O -> unavailable; %% Unavailable
            $P -> private;     %% Private (Name delivery has been blocked)
            _ -> unknown
        end,
    #cpn_ra{reason = Rsn}.

parse_vi(Data) ->
    State =
        case Data of
            16#00 -> off; %% Deactivation (indicator off)
            16#FF -> on;  %% Activation (indicator on)
            _ -> unknown
        end,
    #vi{state = State}.

parse_ccli(Data) ->
    #ccli{number = Data}.

parse_ct(<<H:8,_/binary>>) ->
    Type =
        case H of
            16#01 -> voice_call;        %% Voice Call
            16#02 -> ringback_freecall; %% CLI Ring Back when free call
            16#03 -> cn_delivery;       %% Calling Name Delivery
            16#81 -> msg_wait_call;     %% Message Waiting Call
            _ -> unknown                %% Reserved for network operator use
        end,
    #ct{type = Type}.

parse_fcdli(Data) ->
    #fcdli{number = Data}.

parse_nmss(<<H:8,_/binary>>) ->
    Status =
        case H of
            16#00 -> no;  %% No messages
            16#01 -> one; %% 1 message or unspecified number of message waiting
            _ -> unknown
        end,
    #nmss{status = Status}.

parse_fct(<<H:8,_/binary>>) ->
    Type =
        case H of
            16#00 -> unavailable;  %% Unavailable or unknown forwarded call type
            16#01 -> fc_busy;      %% Forwarded call on busy
            16#02 -> fc_noreply;   %% Forwarded call on no reply
            16#03 -> fc_uncond;    %% Unconditional forwarded call
            16#04 -> defl_alert;   %% Deflected call (after alerting)
            16#05 -> defl_immed;   %% Deflected call (immediate)
            16#06 -> fc_inability; %% Forwarded call on inability to reach mobile subscriber
            _ -> unknown
        end,
    #fct{type = Type}.

parse_cut(<<H:8,_/binary>>) ->
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

%% bin_prep(CID) ->
%%     bin_prep(<<>>, CID).
%% bin_prep(H, []) ->
%%     H;
%% bin_prep(H, T) ->
%%     {H1, T1} = lists:split(2, T),
%%     Num = erlang:list_to_integer(H1, 16),
%%     bin_prep(<<H/binary, Num:8>>, T1).
