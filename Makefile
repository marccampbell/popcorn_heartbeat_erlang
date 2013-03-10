SERVER := erl -pa ebin -pa deps/*/ebin -smp enable -setcookie heartbeats -config rel/files/app.config ${ERL_ARGS}

all:
	rebar get-deps && rebar compile

erl:
	rebar compile

clean:
	rebar clean

clean_logs:
	rm -rf log*

build_plt: erl
	dialyzer --verbose --build_plt --apps kernel stdlib erts compiler hipe crypto \
		edoc gs syntax_tools --output_plt ~/.popcorn_heartbeat_erlang.plt -pa deps/*/ebin ebin

analyze: erl
	dialyzer --verbose -pa deps/*/ebin --plt ~/.popcorn_heartbeat_erlang.plt -Werror_handling ebin

xref: all
	rebar skip_deps=true xref

shell: erl
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@`hostname` -boot start_sasl; \
	else ${SERVER} -name popcorn_heartbeat_erlang@`hostname` -boot start_sasl; \
	fi

run: erl
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@`hostname` -boot start_sasl -s popcorn_heartbeat_erlang; \
	else ${SERVER} -name popcorn_heartbeat_erlang@`hostname` -boot start_sasl -s popcorn_heartbeat_erlang; \
	fi

test: erl
	mkdir -p log/ct
	rebar skip_deps=true ct
	open log/ct/index.html

doc: erl
	rebar skip_deps=true doc
