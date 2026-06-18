# Purpose

This repo contains my submission for the Data Science 871 Part A
practical assessment. Each question is structured in its own
self-contained folder for readability and reproducibility. All questions
have been knitted to `.md` files; Question 1 additionally includes a
`.pptx` deliverable as required by the brief. Below I outline how I
approached each question.

- Question 1: Coffee Hub Analysis

I started by writing a `load_coffee()` function to fix the Excel/UTF-8
encoding issues (`Latin-ASCII`), drop any coffees missing a roast or
reviewer descriptions, and engineer two helper variables, a Rand cost
and a single merged review string. I then scored every coffee against
the 29 top Stellenbosch student keywords to build
`kw_hits`/`student_match`, and wrote seven analysis functions covering
origins, roast, value, roasters, student picks, cost and location. I
presented the findings as five themed plots using my own coffee palette/
or atleast tried to. My main conclusion was that roast and origin drive
quality while price barely tracks rating. See `Question_1` folder for
knitted file.

- Question 2: Baby Names Analysis

This was my most statistically involved question. I loaded four datasets
(baby names plus Billboard and HBO data), aggregated to the national
level, and measured naming persistence using **Spearman rank
correlation** of each year’s top-25 names against 1-, 2- and 3-year
lags, comparing pre- and post-1990. I added year-on-year surge
detection, cultural case studies cross-referencing Billboard artists and
HBO characters against name spikes (Whitney, Elvis, Arya, Barack), a
diversity metric (names needed to cover 50% of births), and decade
bubble charts. I concluded that modern names churn faster and are
increasingly driven by music, TV and political culture. See `Question_2`
folder for knitted file.

- Question 3: Loan and Credit Analysis

I cleaned a one-million-row loan dataset (dropping columns more than 80%
empty, fixing rate/term/employment formatting), and defined a clear
default outcome on closed loans only, giving a 22.5% baseline. I used
`map` to compute default rates across seven borrower characteristics,
then addressed the Director’s DTI hard-cap question with a tolerance
table, ran formal hypothesis tests (`prop.test` for Texas, `chisq.test`
for states), and tested the Institute’s three beliefs. I found that
credit grade and interest rate dominate risk, Texas is statistically
unremarkable, and a single national DTI cap is justified and I closed
with concrete recommendations. See `Question_3` folder for knitted file.

- Question 4: Netflix Analysis

I combined three datasets (Titles, Movie_Info, Credits) through helper
loaders, then ran a broad catalogue exploration: movies-vs-shows mix,
release trends, top producing countries, genre preferences by country,
IMDb score distributions, age certifications, runtime analysis, and a
functional text-analysis pipeline of movie descriptions using `map`,
plus the most prolific actors and directors. I framed my takeaways for
an investor launching a competing platform and Netflix is movie-heavy
and largely “average” in quality, so a rival could differentiate through
original series, underserved geographies and a quality-over-quantity
strategy. See `Question_4` folder for knitted file.
