CREATE TABLE public.logs (
    id SERIAL PRIMARY KEY,
    employee_id INTEGER NOT NULL REFERENCES public.employees(id),
    card_uid UUID REFERENCES public.cards(uid),
    date DATE NOT NULL DEFAULT CURRENT_DATE,
    start_time TIME(0),
    end_time TIME(0) CHECK (end_time > start_time),
    ex_type exclusion,
    ex_start_time TIME(0),
    ex_end_time TIME(0) CHECK (ex_end_time > ex_start_time),
    last_update TIMESTAMP(0) WITHOUT TIME ZONE DEFAULT CURRENT_TIMESTAMP(0),
    created_at TIMESTAMP(0) WITHOUT TIME ZONE DEFAULT CURRENT_TIMESTAMP(0),
    UNIQUE (employee_id, date),
    CHECK (
        (ex_type IN ('come_late', 'leave_early') AND ex_start_time IS NOT NULL AND ex_end_time IS NOT NULL)
        OR (ex_type IN ('sick_leave', 'day_off', 'vacation') AND ex_start_time IS NULL AND ex_end_time IS NULL)
        OR (ex_type IS NULL AND ex_start_time IS NULL AND ex_end_time IS NULL)
    )
);
