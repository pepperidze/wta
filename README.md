## WTA - Work Time Accounting

### Setup
* `make run` - run release
* `docker-compose up db -d` - start db service and init /priv/sql scripts
* `docker-compose down -v` - stop db service
* `docker-compose run eunit` - run all eunit tests
* `docker-compose run eunit make eunit t=wta_db_tests` - run specific eunit tests

### Notes
* employee can have only one work time exclusion per day
* exclusions for non-working days are skipping
* exclusions could be override
* employee can work on non-working days, it is handling correctly

### WTA API
#### API /employee/create
```bash
curl -X POST http://localhost:8080/employee/create \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "last_name=Name&first_name=Name&middle_name=Name&contract_id=1001"

{"status":"ok","data":{"employee_id":1}}
{"status":"error","message":"employee_already_exists"}
```

#### API /card/create
```bash
curl -X POST http://localhost:8080/card/create \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "comment=Internal"

{"status":"ok","data":{"card_uid":"8757db70-7494-4ed6-a475-4e916191b4a0"}}
```

#### API /card/assign
```bash
curl -X POST http://localhost:8080/card/assign \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "employee_id=1&card_uid=8757db70-7494-4ed6-a475-4e916191b4a0"

{"status":"ok","data":{"card_uid":"8757db70-7494-4ed6-a475-4e916191b4a0","employee_id":1}}
{"status":"error","message":"card_not_exists"}
{"status":"error","message":"employee_not_exists"}
{"status":"error","message":"card_not_valid_to_assign"}
```

#### API /card/delete
```bash
curl -X POST http://localhost:8080/card/delete \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "card_uid=f5d6bf38-ca2b-4437-b8a2-22a12b9635fb"

{"status":"ok","data":{"card_uid":"f5d6bf38-ca2b-4437-b8a2-22a12b9635fb","employee_id":null}}
{"status":"error","message":"card_not_exists"}
```

#### API /card/delete_all_by_employee
```bash
curl -X POST http://localhost:8080/card/delete_all_by_employee \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "employee_id=1"

{"status":"ok","data":{"1":["8757db70-7494-4ed6-a475-4e916191b4a0"]}}
{"status":"error","message":"employee_not_exists"}
```

#### API /card/list_by_employee
```bash
curl -X POST http://localhost:8080/card/list_by_employee \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "employee_id=1"

{"status":"ok","data":{"1":["8757db70-7494-4ed6-a475-4e916191b4a0"]}}
{"status":"error","message":"employee_not_exists"}
```

#### API /card/touch
```bash
curl -X POST http://localhost:8080/card/touch \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "card_uid=8757db70-7494-4ed6-a475-4e916191b4a0"

{"status":"ok","data":{}}
{"status":"error","message":"card_not_exists"}
{"status":"error","message":"card_not_assigned"}
```

#### API /work_time/set
```bash
curl -X POST http://localhost:8080/work_time/set \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "employee_id=1&start_time=09:00:00&end_time=18:00:00&work_days=1,2,3,4,5&is_free_schedule=false"

{"status":"ok","data":{}}
{"status":"error","message":"schedule_already_exists"}
```

#### API /work_time/get
```bash
curl -X POST http://localhost:8080/work_time/get \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "employee_id=1"

{"status":"ok","data":{"employee_id":1,"end_time":"18:00:00","is_free_schedule":false,"start_time":"09:00:00","work_days":[1,2,3,4,5]}}
{"status":"error","message":"employee_not_exists"}
{"status":"error","message":"schedule_not_exists"}
```

#### API /work_time/add_exclusion
```bash
curl -X POST http://localhost:8080/work_time/add_exclusion \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "employee_id=1&exclusion_type=come_late&start_datetime=2024-01-01T09:00:00Z&end_datetime=2024-01-01T10:00:00Z"

{"status":"ok","data":{}}
{"status":"error","message":"employee_not_exists"}
{"status":"error","message":"datetime_not_valid"}
```

#### API /work_time/get_exclusion
```bash
curl -X POST http://localhost:8080/work_time/get_exclusion \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "employee_id=1"

{"status":"ok","data":{"1":[{"date":"2024-01-01","end_time":"10:00:00","exclusion_type":"come_late","start_time":"09:00:00"}]}}
{"status":"error","message":"employee_not_exists"}
```

#### API /work_time/history_by_employee
```bash
curl -X POST http://localhost:8080/work_time/history_by_employee \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "period=all&employee_id=1"

{"status":"ok","data":{"history":[{"date":"2024-01-01","employee_id":1,"exclusion":"come_late","exclusion_end_time":"10:00:00","exclusion_start_time":"09:00:00","exclusion_time":"01:00:00","is_free_schedule":false,"logged_end_time":null,"logged_start_time":null,"logged_time":null,"planned_time":"09:00:00","schedule_end_time":"18:00:00","schedule_start_time":"09:00:00","work_days":[1,2,3,4,5]}]}}
{"status":"error","message":"employee_not_exists"}
```

#### API /work_time/history
```bash
curl -X POST http://localhost:8080/work_time/history \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "period=all&limit=100&offset=0"

{"status":"ok","data":{"history":[{"date":"2024-01-01","employee_id":1,"exclusion":"come_late","exclusion_end_time":"10:00:00","exclusion_start_time":"09:00:00","exclusion_time":"01:00:00","is_free_schedule":false,"logged_end_time":null,"logged_start_time":null,"logged_time":null,"planned_time":"09:00:00","schedule_end_time":"18:00:00","schedule_start_time":"09:00:00","work_days":[1,2,3,4,5]},{"date":"2024-08-19","employee_id":1,"exclusion":"come_late","exclusion_end_time":"10:00:00","exclusion_start_time":"09:00:00","exclusion_time":"01:00:00","is_free_schedule":false,"logged_end_time":null,"logged_start_time":null,"logged_time":null,"planned_time":"09:00:00","schedule_end_time":"18:00:00","schedule_start_time":"09:00:00","work_days":[1,2,3,4,5]}],"limit":100,"offset":0}}
```

#### API /work_time/statistics_by_employee
```bash
curl -X POST http://localhost:8080/work_time/statistics_by_employee \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "period=all&employee_id=3"

{"status":"ok","data":{"3":{"come_late":7,"come_late_exclusion":2,"leave_early":7,"leave_early_exclusion":2,"logged_time":"179:00:00","planned_time":"180:00:00"}}}
{"status":"error","message":"employee_not_exists"}
```

#### API /work_time/statistics
```bash
curl -X POST http://localhost:8080/work_time/statistics \
-H "Content-Type: application/x-www-form-urlencoded" \
-d "period=all&limit=100&offset=0"

{"status":"ok","data":{"limit":100,"offset":0,"statistics":[{"3":{"come_late":7,"come_late_exclusion":2,"leave_early":7,"leave_early_exclusion":2,"logged_time":"179:00:00","planned_time":"180:00:00"}},{"4":{"come_late":0,"come_late_exclusion":0,"leave_early":0,"leave_early_exclusion":0,"logged_time":null,"planned_time":"09:00:00"}}]}}
```
