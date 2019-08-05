# Questions

## Workshop 1

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
- Cinema has many screens (1:many)

SCREEN-PROJECTOR RELATIONSHIP
- Each screen has one projector (1:1)

CINEMA-MOVIE RELATIONSHIP
- Each cinema has many movies, but these are shown many places (many:many)

How to link movies to cinemas? LATER

What we call 'business rules' for now means 'n:m' type relationships.