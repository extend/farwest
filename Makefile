# See LICENSE for licensing information.

PROJECT = farwest

# Dependencies.

DEPS = farwest_core farwest_ui
dep_farwest_core = https://github.com/extend/farwest_core 0.3.0
dep_farwest_ui = https://github.com/extend/farwest_ui 0.3.0

# Release.

release: clean-release deps app
	# Workaround for mimetypes for now
	cd deps/mimetypes && ./../lager/rebar clean compile
	relx -c rel/relx.config -o rel/farwest

clean-release:
	rm -rf rel/farwest

# Standard targets.

include erlang.mk
