REBAR = `which rebar || ./rebar`

all:
	@( $(REBAR) compile skip_deps=true )

clean:
	@( $(REBAR) clean )

test:
	@( $(REBAR) eunit skip_deps=true )

.PHONY: test