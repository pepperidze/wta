PROJECT = wta
PROJECT_DESCRIPTION = Work Time Accounting
PROJECT_VERSION = 0.1.0

ERLC_OPTS += \
	+warn_export_vars \
	+warn_shadow_vars \
	+warn_obsolete_guard \
	+warn_export_all \
	+warnings_as_errors

ERLC_OPTS += +'{parse_transform, lager_transform}'

DEPS = lager pgapp cowboy jsx liver

dep_pgapp  = git https://github.com/epgsql/pgapp            e55452b
dep_cowboy = git https://github.com/ninenines/cowboy        2.11.0
dep_jsx    = git https://github.com/talentdeficit/jsx.git   v3.0.0
dep_liver  = git https://github.com/erlangbureau/liver.git  0.9.4

REL_DEPS += relx

TEST_DEPS = meck

EUNIT_OPTS = verbose
EUNIT_ERL_OPTS = -config config/docker.config
TEST_ERLC_OPTS = +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

include erlang.mk
