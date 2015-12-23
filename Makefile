
all: deps
	./rebar compile

deps:
	git submodule update --init
	./rebar get-deps

test:
	erl -pa ebin -eval 'application:start(exgb, permanent), {ok, Model} = exgb:load_model("priv/xgb.model"), Pred = exgb:predict(Model, [2.0,2.0,2.0]), io:format("Model: ~p~nPred: ~p~n", [Model, Pred]), init:stop().'

