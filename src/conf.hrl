%%
%% conf.hrl
%% Kevin Lynx
%% 06.11.2013
%%
-ifndef(KDHTCONFHRL).
-define(KDHTCONFHRL, true).

%% The time waiting for a response for a query
-define(QUERY_TIMEOUT, 2000).
-define(K, 8).

% system monitor flags
-define(MAX_NODE_TIMER, 160*8).

-ifdef(debug).

-define(NODE_TIMEOUT, 1*60*1000).
%% Not implemented yet
-define(BUCKET_TIMEOUT, 1*60*1000).
%% InfoHash expire time
-define(EXPIRETIME, 1000*5*60).
%% InfoHash checking interval time
-define(CHECK_INTERVAL, 1000*2*60).
-define(SAVE_INTERVAL, 2*60*1000).
-else.

-define(NODE_TIMEOUT, 10*60*1000).
%% Not implemented yet
-define(BUCKET_TIMEOUT, 10*60*1000).
-define(EXPIRETIME, 1000*30*60).
%% InfoHash checking interval time
-define(CHECK_INTERVAL, 1000*10*60).
-define(SAVE_INTERVAL, 5*60*1000).

-endif.
-endif.
