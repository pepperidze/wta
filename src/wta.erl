-module(wta).

-export_type([
    uid/0,
    employee_id/0
]).

-type uid() :: binary().
-type employee_id() :: pos_integer().
