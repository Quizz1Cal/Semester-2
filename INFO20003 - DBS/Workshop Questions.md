# Questions

## Workshop 2

Case study:

CINEMA
- CinemaID (PK)
- Name (STRING, NN)
- Address (STRING, NN)
- YearlyRent (INT)

SCREEN
- Number (INT, NN)
? - CinemaID (FK)
- Size (INT ft, NN)
- Capacity (INT, NN)
- offersGold (BOOL, NN)
? - ProjectorSerial (NN)

PROJECTOR
- SerialNumber (PK, NN)
- format (STRING)
- ModelNumber (INT, NN)
- Resolution (STRING, NN)
- HoursUsed (INT, NN)

MOVIE
- Title (STRING, PK, NN)
- YearOfRelease (DATETIME, NN)
- Rating (STRING, NN)

MOVIE-SCREEN RELATIONSHIP
- LastTimeSeen (DATETIME)

CINEMA-SCREEN RELATIONSHIP
- Cinema has many screens (1:many) --> store cinema key in screen

SCREEN-PROJECTOR RELATIONSHIP
- Each screen has one projector (1:1)

CINEMA-MOVIE RELATIONSHIP
- Each cinema has many movies, but these are shown many places (many:many)

How to link movies to cinemas? LATER

What we call 'business rules' for now means 'n:m' type relationships.

## Workshop Wk 3

- Why no red diamon on Projector foreign key in the Screen?
- Weak/strong relationships?
- Weak/strong associative entity relationships?

### Lab Task 3.1

- Bank Accnt Balance: DECIMAL(12,2)
- Full name: VARCHAR(100)
- Home address: VARCHAR(100)... But it's composite
- Postcode: CHAR(4) OR SMALLINT
- LinkedIn Page: VARCHAR(100)
- Website: VARCHAR(100)
- Text message sent at: DATETIME
- Time started working: DATE (time irrelevant)
- Duration of song: TIME
- Unimelb room #: VARCHAR(5) ... not sure a/b future lengths
- Unimelb assigment grade: ENUM('N','P','H3','H2B','H2A','H1') .. categorical, finite
- Comment on article: MEDIUMTEXT ... it's long but not THAT long