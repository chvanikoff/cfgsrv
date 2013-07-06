REBAR = `which rebar || echo ./rebar`

all:
	@( $(REBAR) compile)

clean:
	@( $(REBAR) clean )

test:
	@( $(REBAR) eunit )

.PHONY: all clean test