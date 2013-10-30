APPNAME='piconf'
rebar compile
erl -name "${APPNAME}_${RANDOM}" -pa ebin \
	-eval "application:start($APPNAME)." \
	-eval "io:format(\"Application: $APPNAME started~n\")."
