-module(scraper_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    scraper_sup:start_link().
    %% inets:start() %% needs to be moved to deps?
    %% supervisor:start_child(scraper_worker_sup, []),
    %% supervisor:start_child(scraper_worker_sup, []),
    %% supervisor:start_child(scraper_worker_sup, []),
    %% scraper_queue_ser:add_urls(["http://www.zininsushi.nl"]).
    

stop(_State) ->
    ok.
