# Workouts
The objectives of this project are to:
  - enable easier notation for workouts (textual form)
  - be able to specify weekly based training plans in e.g. spreadsheets that can be exported to CSV
  - be able to import workouts from CSV file to Garmin Connect workout list
  - be able to schedule imported workouts in Garmin Connect calendar automaticaly based on start/end date

An example of a workout definition notation:
```sh
workout: run-fast
- warmup: 10:00 @ z2
- repeat: 3
  - run: 1.5km @ 5:10-4:40
  - recover: 500m @ z2
- cooldown: 05:00
```
In CSV 1st row is reserved for heading, can be anything and the 1st column is reserved for a week number. The cells represent days and they are populated with workouts (definitions and references), so there's a limitation: a single work out per day/cell, for now. In case of the references, a workout needs to be defined first in one of the previous cells and then it can be referenced by name in the following cells.

An example of 2-weeks training plan, containing 2 workout definitions, 4 references and 6 training days in total:

| Week | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
| ----:| --- | --- | --- | --- | --- | --- | --- |
| 1    | ``workout: run-fast``<br>``- warmup: 10:00 @ z2``<br>``- repeat: 3``<br>&nbsp;&nbsp;``- run: 1.5km @ 5:10-4:40``<br>&nbsp;&nbsp;``- recover: 500m @ z2``<br>``- cooldown: 05:00``|rest|rest|run-fast|rest|rest|rest|
| 2    | run-fast| ``workout: long-15`` <br> ``- run: 15 km @ z2``|rest|run-fast|rest|rest|long-15|