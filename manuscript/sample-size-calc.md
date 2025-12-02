-   Each participant comes three times (sessions).
-   Each session has a fixed supply-air temperature (SAT): 15°C, 17°C, or 19°C.
-   Within each session, the participant is exposed to three air speed conditions (one at each desk).

So each participant experiences 9 conditions total = 3 SAT levels × 3 air speeds.

In statistical-design terms, that’s a within-subject (repeated measures) 3 × 3 factorial design \* Factor A: SAT with 3 levels (15 / 17 / 19) \* Factor B: Air speed with 3 levels (e.g. low / medium / high)

All conditions are measured on the same people



### conceptual model
Acceptability ~ SAT * Speed + (1 | Participant)

* Fixed effects: main effects of SAT and Speed, plus their interaction.
* Random effects: random intercept for participant (captures repeated measures).

* Alpha (Type I error): usually 0.05 (two-sided).
* Power (1 − β): usually 0.80 or 0.90.