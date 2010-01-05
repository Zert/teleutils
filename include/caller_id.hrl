-record(dt, {           %% Date and Time
          month,
          day,
          hour,
          minute
         }).
-record(cl, {number}).      %% Calling Line Identity
-record(cd, {number}).      %% Called Line Identity
-record(cl_ra, {reason}).   %% Reason for Absence of Calling Line Identity
-record(cpn, {name}).       %% Calling Party Name
-record(cpn_ra, {reason}).  %% Reason for absence of Calling Party Name
-record(vi, {state}).       %% Visual Indicator
-record(ccli, {number}).    %% Complementary Calling Line Identity
-record(ct, {type}).        %% Call type
-record(fcdli, {number}).   %% First Called Line Identity (in case of forwarded call)
-record(nmss, {status}).    %% Network Message System Status
-record(fct, {type}).       %% Type of Forwarded call (in case of forwarded call)
-record(cut, {type}).       %% Type of Calling user
-record(rn, {number}).      %% Redirecting Number (in case of forwarded call)

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
