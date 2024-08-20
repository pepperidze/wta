CREATE TYPE exclusion AS ENUM ('come_late', 'leave_early', 'sick_leave', 'day_off', 'vacation');

CREATE OR REPLACE FUNCTION check_if_work_day(ex_type exclusion, date DATE, work_days INTEGER[])
RETURNS BOOLEAN AS $$
BEGIN
    RETURN EXTRACT(ISODOW FROM date) = ANY(work_days) AND
        (ex_type NOT IN ('sick_leave', 'day_off', 'vacation') OR ex_type IS NULL);
END;
$$ LANGUAGE plpgsql;
