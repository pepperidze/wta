CREATE TABLE public.schedules (
    employee_id INTEGER PRIMARY KEY REFERENCES public.employees(id),
    start_time TIME(0) NOT NULL,
    end_time TIME(0) NOT NULL CHECK (end_time > start_time),
    work_days INTEGER[] NOT NULL CHECK (array_length(work_days, 1) >= 1 AND array_length(work_days, 1) <= 7),
    is_free_schedule BOOLEAN NOT NULL,
    last_update TIMESTAMP(0) WITHOUT TIME ZONE DEFAULT CURRENT_TIMESTAMP(0),
    created_at TIMESTAMP(0) WITHOUT TIME ZONE DEFAULT CURRENT_TIMESTAMP(0)
);
