APPNAME='piconf'
rebar compile
echo "cert $CERTFILE"
erl -name "${APPNAME}_${RANDOM}@`hostname`" -pa ebin deps/*/ebin \
	-eval "application:start($APPNAME)."
echo -e "\e[0;33mreturn value: \e[1;31m$?\e[0m"
