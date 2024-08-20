-define(WTA_EXCLUSION_COME_LATE, <<"come_late">>).
-define(WTA_EXCLUSION_LEAVE_EARLY, <<"leave_early">>).
-define(WTA_EXCLUSION_SICK_LEAVE, <<"sick_leave">>).
-define(WTA_EXCLUSION_DAY_OFF, <<"day_off">>).
-define(WTA_EXCLUSION_VACATION, <<"vacation">>).

-define(WTA_EXCLUSIONS, [
    ?WTA_EXCLUSION_COME_LATE,
    ?WTA_EXCLUSION_LEAVE_EARLY,
    ?WTA_EXCLUSION_SICK_LEAVE,
    ?WTA_EXCLUSION_DAY_OFF,
    ?WTA_EXCLUSION_VACATION
]).

-define(WTA_PERIOD_WEEK, <<"week">>).
-define(WTA_PERIOD_MONTH, <<"month">>).
-define(WTA_PERIOD_YEAR, <<"year">>).
-define(WTA_PERIOD_ALL, <<"all">>).

-define(WTA_PERIODS, [
    ?WTA_PERIOD_WEEK,
    ?WTA_PERIOD_MONTH,
    ?WTA_PERIOD_YEAR,
    ?WTA_PERIOD_ALL
]).
