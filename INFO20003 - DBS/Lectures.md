# Notes for Database Systems 2019 Sem 2

### Author: Callum H

# Random

**Data Dictionary**:

**Data Model**: A collection of concepts for describing data

**Schema**: A descr. of a particular collection of data, using a given data model

#### EXTRA: Query Languages
- *Data definition language* (DDL): Defines and sets up a DB
- *Data manipulation language* (DML): Maintains and uses the DB 
- *Data control language* (DCL): Control access to DB

# Recent Notes

## Lecture 1

**Data**: Known facts stored and recorded (e.g. text, numbers etc.)
- The RAW INFORMATION

**Information**: Data presented in context (e.g. summarised data; processed data that *increases* users knowledge)
> The core difference: *data* is known and available, but *information* is processed and useful

**Metadata**: Data describing other data
- Includes *structure*, *rules*, *constraints*
- Provides CONSISTENCY and MEANING

**Database**: A large, integrated, structured collection of data
- Usually intended to model some real-world examples

**DBMS**: DataBase Management System. A software system designed to *store*, *manage* and *facilitate access to* databases
- BENEFIT: Manage data in structured way
- Predominantly relational databases, i.e. *rows and columns form RELATIONS* and *keys/foreign keys link RELATIONS*

### Benefits of DBMS (rel. to FPS)
- Data independence: Separation of DATA and PROGRAM/LOGIC with a *data repository* and *central management*
- Minimal data redundancy: Controlled by *normalisation* (to break down a relationship b/t two relations into a set of smaller relations)
- Improved data consistency: Single store, no disagreements, update problems, minmal storage space required
- Improved data sharing: Data shared, corporate resource, external users could access data, and multiple (simultaneous?) views possible
- Reduced program maintenance: Data structure can change WITHOUT application data changing
- Novel ad hoc data access 'without programming': Query languages e.g. SQL

## File Processing Systems

This is a collection of programs that store/manage *files* in computer. Each program has its own set of data files, often restructured to meet needs of new application.

### Flaws (rel. to DBMS)

- Program-data dependence: Changing the file necessitates program change; program 'knows too much a/b low-level data structure'
- Duplication of data: Wasteful, loss of integrity (e.g. same name, different data)
- Limited data sharing: Data is tied to application, adhoc reports difficult
- Lengthy dev times: Application must do low-level data management due to dependence = SLOW
- Excessive program maintenance: "Up to 80% of dev time is for maintenance"

# Lecture 2: Database Development

## Database Development Cycle (in order)
- **Database Planning**:  How to do the project, how does the enterprise work, what is the business?
- **Systems Definition**: "Scope and boundaries"; how would it interfere with org. systems?
- **Req. Definition & Analysis**: Collection & analysis of requirements for new DBMS; what issues does it need to solve, the rules it needs to follow?
- **Design** (see below) & **Application Design** (Designs interface/application to use/process DB)
- **Implementation**: Realisation of DB/application physically. 
- **Data Conversion and Loading**: Transfer data into DB, convert (non trivial). Not always needed - unless you're updating an older DBMS and thus need to move it across.
- **Testing**: Running DB, catch errors in design/setup (physical/logical), improving performance/robustness/recoverability/adaptability/scalability
- **Operational Maintenance**: Infinite process of monitoring/maintaining DBMS post-commission e.g. new requirements, changes

## Database Design
- **Conceptual**: Constructing a model of data to be used: independent of physical considerations (i.e. Data Models, ER Diagrams)
- **Logical**: Constructing relational model based on conceptual design. "This is the data model"
- **Physical**: Actual creation, description of how the logical design will be implemented for a specific DBMS including
    - Basic relations
    - File org.
    - Indexing
    - Data types
> Notice that each of these stages feeds into the next

Benefits of appropriate data types
- Efficient storage/use of DBMS
- May improve data integrity
- Consistency
- CONSIDER: Does the data type support or impede manipulation required?

Other considerations:
- Store LOOKUPs or keep them ad-hoc?
- Data field integrity
- Missing/NULL data
- **Normalisation**: Formal method to validate/improve on logical design (e.g. grouping attributes) prior to physical implementation
> Benefits of de-normalisatiion: better performance HOWEVER wasted storage space and potential integrity threats

### Data Dictionary
Key fields:
- *key*: Type of key (primary, foreign, neither)
- *attribute*, *data type*
- *not null*: Required or optional
- *unique*: Is the field unique
- *Description*: Useful info on attribute to designer/devprs e.g. attr sizes, valid values etc.

### Database Planning

# Lecture 3: ER Modelling Introduction

## ER Model Basics

**Entity**: Real-world object distinguishable from other objects. 
- Described in DB using set of attributes
- The same entity can participate in different relationship sets or even different 'roles' **in the same set** (e.g. reporting to two different other employees)
- Never include the business itself as an entity

**Entity Set**: Collection of entities of same type
- All entities in an E.S. have same attributes
- Each entity has a key (set) to uniquely identify it

**Relationship**: Association b/t two or more entities. These are determined by the business rules
- *Can have their own attributes, sometimes called **descriptive attributes** (e.g. duration of relationship)*
- E.g. Fred works in Pharmacy Dept.

**Relationship Set**: Collection of relationships of same type.
- E.g. Employees work in depts.

**Weak Entity**: Identified uniquely ONLY by considering (primary key of) another entity (set) (called the owner). 
- Requires that:
    1. Two entity sets have a one-to-many relationship set (many weak ones)
    2. Weak entity set must have total participation in (what is called) the **identifying relationship**
- Weak entities have a partial key -> only uniquely identified when considering primary key of **owner entity**

## Constraints

**Key constraints** determine the cardinality of relationship -> the no. of objects taking part in relationship set. An 'upper bound'
- Many-to-many (a line)
- One-to-many (a unidirectional arrow). Officially a **key constraint**
- One-to-one

**Participation Constraint** asks if all entities (of a given relationship) take part. A 'lower bound'
- **total** or **partial** participation

## n-ary Relationships

E.g. a relationship is ternary if three entity sets are related.

**WARNING**: Just because D sells K and K is bought by P doesn't mean D sells to P. Be careful with relationships.

## Attribute Types

**Multi-valued attributes**: Can have multiple values (from a finite set) of same type e.g. Employees have a work AND phone number, both captured under 'phone num'.

**Composite attributes**: Have a hidden structure (elements can be of different type) e.g. "Address" consists of Postcode, Streetname, Streetnum.

## Conceptual Design with the ER Model

Design of an ER model is ultimately subjective with many solutions. However, the basics:
> C.D. follows requirements analysis (yield a high-level description of data)
> C.D. popularly uses (some variation of) ER models (constructs are expressive, follows thought process)
> Uses basic constructs (entities, relationships, attributes) and more complex ones (weak entities etc.)

**Design choices**:
- Should a concept be an entity, or attribute?
> If you have several values of it per (something else), perhaps better an entity with a relationship
- Should a concept be modelled as entity, or rxnship?
- Should a relationship be unary, ternary, unary, n-ary...?

**Constraints**:
- Lots of data semantics can and **should** be captured

# Lecture 4: Relational Models, Translating ER Diagrams

## Relational Model
**Data Model**: Allows us to translate real world things into structures for a computer. E.g. *relational*, *ER*, *Object-oriented*, *Network*, *Hierarchical*

**Relational Model**: 
- Rows and columns for tuples/records and attributes/fields respectively
- Keys and foreign keys to link relations & identify records

**Relational Database**: A set of *relations*, which themselves are made of:
1. A **schema**: Specifies name of relation, plus name and type of each column/attribute e.g Students(*sid*: string, *name*: string, *gpa*: real...)
2. An **instance**: A table with rows and columns, where (# rows) is *cardinality* and (# cols/fields) is *degree/arity*
> Think of it as a set of rows/tuples: all rows are distinct and unordered.

E.g. a Students relation is a table with headings for each field, and each row is a distinct student.

## Keys and ICs

**Keys**: Associate tuples in different relations, and a form of **integrity constraint (IC)**.
- e.g. "Only students can be enrolled in subjects"

**Superkey**: A set of fields where no two distinct tuples have the same values in all these fields
- Note that for some sets a superkey cardinality may be more than one due to duplicate values

**candidate key**: A **superkey** with no (strict) subset that is (also) a **superkey** (wrt same relation).
- Sets can have multiple candidate keys (each of which can consist of 1 or more columns)
- In such sets, one key (for each relation) is made **primary key**. The rest **COULD** have been the primary key, but were not chosen.

**Foreign key**: A set of fields in one relation that 'refers to' a tuple in another. They MUST correspond to the primary key of the other relation.

**Referential integrity**: Where all foreign key constraints are enforced in a DBMS.
- If a foreign key'd tuple is inserted, reject it
- If a PRIMARY key'd tuple is deleted/updated must debate: delete what refers it, disallow this deletion, have some default PRIMARY key (e.g. `null`)

**Integrity Constraint (IC)**: A condition that must be true for any instance of the database (e.g. **domain constraints*)
- Specified at definition of a schema
- Are checked when relations are modified
- **Legal instances of a relation**: Are those that satisfy all ICs. DBMS should **NOT ALLOW** illegal instances

SQL and keys: 
```SQL
CREATE TABLE Enrolled 
(sid CHAR(20), 
 cid CHAR(20),
grade CHAR(2),
PRIMARY KEY(sid,cid),
FOREIGN KEY (sid) REFERENCES Students ) -- To say that sid in Students is a foreign key acquired from Students

-- to write PRIMARY KEY(sid), UNIQUE(cid, grade) is to say students take one course and no two students in a course get the same grade. It is NOT to say that for a given student, course pair there is a single grade.
```

## Converting to Logical and Physical Models

Conceptual: ER Diagram >>> Logical: "Employee(_ssn_, name, age)" (ssn underlined). 
- Multi-valued & composite attributes need flattening (e.g. `home_num, work_num`, `postcode`, `street_name`, `street_num`) OR a lookup table
    - **Lookup table**: store multi-valued attributes as an instance in a weak entity. CON of this would be the need to 'JOIN' later on. **TODO: PRETTY SURE THIS IS LOOKUP TABLE REFERRED TO**
- When NOT dealing with many-to-many relationship sets, no new relation is necessary (but still usable). "The primary key(s) from the 'many' becomes a foreign, non-primary key in the 'one'" (usually NOT right next to the primary keys to distinguish its role as a foreign key)
    - E.g. Each department has one manager; the manager is MANY because one can manage many depts, but NOT the other way. So the manager's keys are put in department.
- For total participation, include `NOT NULL` next to the relevant field
- For weak entities, the dependent uses the key of the identifying entity as a PFK, not just a FK
- When dealing with BINARY one-to-one relationship sets: *The optional side gets the key of the mandatory side* (e.g. since all CareCenters have a nurse, but not all nurses must have a CareCenter (but max 1), the NURSE is mandatory but the center is OPTIONAL to the nurse - so put the key in the CareCenter)
- When dealing with many-to-many relationship sets, a new relation must be made, whose attributes must include (somewhat intuitively):
    1. Keys for each participating entity set as foreign keys (forms a superkey for the relation)
    2. All descriptive attributes (e.g. start/end date of relationship)
    - E.g. Employee(...), Department(...), Works_In(ssn_u, did_u, since). The latter is called an **associative entity**. Note the keys of the connecting entities become the *primary foreign key* (set)

**NOTE**: PK are underlined, FK are underlined and italic, PFK are underlined and bold.

Logical >>> Physical: "Employee(_ssn_ CHAR(11), name VARCHAR(20), age INTEGER)" and likewise for the other two, with appropriate stylisation of the keys.

Physical >>> Implementation: SQL (more specifically DDL, Data Definition Language)
```SQL
-- Note the participation should always be specified.
CREATE TABLE Employee
    (ssn CHAR(11) auto_increment, -- OR not NULL as they did in the example 
    name VARCHAR(20) NULL,  -- Partial participation constraint
    age INTEGER NOT NULL,  -- Total participation constraint 
    salary DECIMAL(10,2) NOT NULL,
    PRIMARY KEY (ssn))

-- A similar definition for Department

/* Assocative entity's relation. Note the key being referenced is NOT explicitly declared 
   i.e. Employee(ssn) is not used.
 */ 
CREATE TABLE Works_In 
    (ssn CHAR(11) NOT NULL,
     did INTEGER NOT NULL,
     since DATE NULL,
     PRIMARY KEY(ssn, did),
     FOREIGN KEY (ssn) REFERENCES Employee,
     FOREIGN KEY (did) REFERENCES Department)

 -- If this (weak entity set) was dependent on (identifying relationship set) Department 
 CREATE TABLE Dependent (
     dname CHAR(20) NOT NULL,
     ...,
     PRIMARY KEY(dname, ssn),
     FOREIGN KEY(ssn) REFERENCES Employees
       ON DELETE CASCADE)
 )
```

Finally actual instances are defined, where an **Instance** is an actual table with entries.

# Lecture 5: Workbench

## ER Modelling - Notations
- LIGHTBULB = key
- MANY LIGHTBULBS = each one a partial identifier, together forms the key
- NOT NULL = Blue diamond, NULL if empty diamond
- RED = Foreign key **I THINK. TODO**
- TODO: **CHECK REFERENTIAL INTEGRITY**
- [someAttr] is a derived (i.e. calculated field) *AND IS OMITTED AT PHYSICAL DESIGN*
- (someAttr) is multi-valued
- someAttr(item1, item2) is a composite
- Connection types:
    - **FORK FOR VARIANCE**: Forked connection = no key constraint ("many"), OTHERWISE = key constraint "the one"
    - **O for OPTIONAL**: Circled connection = partial part., OTHERWISE = total participation
    - **DOTS FOR WEAK DEPENDENCE**: Dotted lines = strong entity, solid = weak

In order to toggle participation, go to foreign key. Whichever side is NOT mandatory, you indicate so. E.g. ALL albums must pair with a song, but not all songs MUST pair with an album -> you make the 'album' side NOT mandatory in MySQL. In other words, **if you have a circle, you don't HAVE to pair up with something. If you have a line (total), you MUST pair up with something.**
- ALTERNATE: Think of the circle as a vague arrow. Whoever's arrowtip points with a vague arrow, does not require that thing to exist.
```SQL
/* Coding an associative entity. Note how the foreign keys are ALSO primary keys, and
   Also note the explicit declaration of the field that is referenced. 
 */ 
CREATE TABLE CustomerAddress (
    CustomerID smallint,
    AddressID smallint,
    AddressDateFrom DATE,
    AddressDateTo DATE,
    PRIMARY KEY (CustomerID, AddressID, AddressDateFrom).
    FOREIGN KEY (CustomerID) REFERENCES Customer(CustomerID)
        ON ...,
    FOREIGN KEY (AddressID) REFERENCES Address(AddressID)
        ON ...
)
```

# Lecture 6: Unary Relationships & Hands On Modelling

## Unary Relationships

Very similar operation and solution as a binary relation:
- **One-to-one**: Put a foreign key in it, unique each time
- **One-to-many**: Put a foreign key in it, will be non-unique
- **Many-to-many**: Use an associative entity, place two fields of foreign keys, use a different name for each = combined key

```SQL
-- Unary relation physical model
CREATE TABLE Employee (
    ID smallint NOT NULL,
    ...,
    ManagerID smallint,
    PRIMARY KEY(ID),
    FOREIGN KEY(ManagerID)
    REFERENCES Employee(ID)
)
```

## Ternary Relationships

For many-to-many ternary relationships, create an associative entity and map each to it in a one-to-many (so each entity can have many relationships). Each entity's key (set) will together form the FPK for the associative entity.

## Notation Summary

... Note that the ARROW in Chen's notation points towards the UNIQUE value. Analogously, in crow's foot notation the FEATHERS of the arrow are the many, the ARROWTIP the one (target). 

### Chen's Notation for ER Modelling

Use *squares* for name of set, *ovals* for attribute, *underline* for key, *diamond* for relationship. 
- Use a dotted border for **derived atttributes** (those calculated from other attributes and not physically stored)
- WEAK ENTITY: Represented as **bold rectangle**
- WEAK (IDENTIFYING) RELATIONSHIP: **Bold diamond**
- A self-relating set has two lines b/t it and the relationship.
- MULTI-VALUED: Oval with a double-lined (=) border
- COMPOSITES: Ovals extending off an attribute oval
- KEYS: Underlined
- FOREIGN KEYS: ...? TODO
- WEAK/PARTIAL KEY: Dotted underline
- ONE in the key constraint: The MANY points at the ONE. E.g. "Every department has at max one manager", the Department points TO the relationship (i.e. to the employee)
- MANY in the key constraint: generic line
- **Bold a total participation line**

### Crow's Foot Notation (used in MySQLWorkbench)

... Just note that partial jeys are underlined dotted.

Also note that the one-to-many relationships is the same direction as the arrow in Chen's. The FORK is the feathers of the arrow; the arrow points AT the unique one.

In WEAK relationships, the arrow points to the CONTAINER (only one container), the unique thing that everything depends on.  

# Relational Algebra

**Relational algebra** are operations with input/output relations. 

## Core Operations
1. Selection (\sigma): Selects a subset of rows. **Horizontal filtering**
    - Schema (structure/name/layout) identical to input
    - No need for duplicate elimination (input implied unique)
    - Call the condition a *selection condition*. Uses `<,>,<=,>=,=,!=` and `\vee, \wedge` for complex conditions
    - `\sigma_{rating>8}(S2)`
2. Projection (\pi): Selects a subset of columns **vertical filtering**
    - Schema - Only the fields in the proj. list; retain same names
    - Eliminates *duplicates* (not in REAL systems though), as from a theoretical standpoint relations have unique tuples
    - `\pi_{age}(S2)`
3. Cross-product (x): Allows combination of two relations
4. Set-difference (-): Tuples (rows) in one relation but not the other
    - Inputs must be *union-compatible*: Same # fields, same field types
    - No need to eliminate duplicates as `S1-S2\subsetS1`
    - Non-commutative operation (duh)
5. Union (U): Tuples (rows) in one relation AND/OR the other
    - Inputs must be *union-compatible*: Same # fields, same field types
    - Eliminates *duplicates*

## Cross-Product

Combines two relations by merging the rows of the inputs so that the final relation has attributes of **both inputs**.
- This is cartesian: there is no key matching, it is NxM.
- Schema: One field per field of BOTH inputs, inheriting names where possible and renamed otherwise


## Renaming

`\rho(C(1->sid1, 5->sid2),S1xR1)` where `C` is the name of the output and `1,5` the indices (starting at 1) of the cols to rename

## Compound Operators
- Composition is achieved by layering functions in the input of another.
- Intersection (\cap): Rows in both relations. Equivalent to `R-(R-S)`.          - Inputs must be union-compatible
    - No need to eliminate duplicates

**Joins** involve cross-product, selection and at times, projection.
- **Natural join (join)**: `R|X|S` (an hourglass, v weird) matches rows where attributes that are in BOTH relations have EQUAL VALUES, omitting duplicates.
    - Must Compute `\pi_{Uniques, attr11, attr21...}(\sigma_{attr11=attr12, ...}(RxS))` (product, select those with equals, project)
    - REMOVES one copy of the overlapping columns
- **Condition Join (theta-join)**: `R |X|_c S = \sigma_c (RxS)`. 
    - Schema is identical to that of the cross-product, but it filters that on some condition
- **Equi-Join** is a condition join where the condition is only composed of equalities. This is NOT a natural join since the fields to equate are not fixed to the shared attributes (and may not even include them)

### Practice:
**TIP: The older sailor has the worser rating** -> cross product on itself, but since the pairings are unordered, but the CROSS PRODUCT IS, you only need to waorry about 'half' e.g. age1 < age2 and rating1 > rating2 .. and nothing else.
- The name of sailors whose rating is above 9:
    - `\pi_{sname}\sigma_{rating>9} S`
- Sailors who reserved a boat prior to Nov 1 1996
    - `\pi_{sname}\sigma_{day < 1/11/96} (R|x|S)`
- Names of boats reserved at least once
    - `\pi_{name} (B |X| R)` (dupe elim is nice)
- Pairs of sailors with same rating
    - `\rho(C(1->sid1, 2->sid5), S |X| S)`
    - `\pi_{sid1, sid2}\sigma_{rating1=rating2} (S |x| S)` (need natural to avoid (same,same))

# SQL

**SQL** is a structured query language used in relational databases, supporting (like a DBMS) creation, reading/select, update and deletion commands.
- **Data Definition Language (DDL)**: Defines and sets up a database. `CREATE, ALTER, DROP`
- **Data Manipulation Language (DML)**: Maintain and use the database. `SELECT, INSERT, DELETE, UPDATE`
- **Data Control Language (DCL)**: Controls access to database. `GRANT, REVOKE`
- Other: Administer database, transaction control

## Common Commands

```SQL
INSERT INTO Customer 
    (CustFirstName, otherCols...) -- If you exclude, ALL columns need entry
    VALUES ("Peter", ...);

-- NULL means ...

-- Select is equivalent to PROJECT in SQL. No removal of dupes so
SELECT * from Table;
SELECT DISTINCT <rows,...> FROM <Table>;

... WHERE Field LIKE "<annoying REG_EXP....>" -- Regular expressions

SELECT [ALL|DISTINCT]                           -- Rows/calc'd fields. DISTINCT to discard dupes
    FROM <tables to cross product,...>          -- Source
    <JOIN SPECIFICATIONS>
    WHERE cond1 AND cond2 OR ...                -- Self-exp.
    GROUP BY {col_name | expr} [ASC|DESC]...,   -- Duh 
    HAVING cond                                 -- A selection condition on the GROUPS to include
    ORDER BY {col_name|expr|position} [ASC|DESC, ..] -- Sorting 
    LIMIT {[offset, ] row_count | row_count OFFSET offset}; -- Output constriction

-- ORDER IS IMPORTANT

-- Some aggregators
AVG(), MIN(), MAX(), COUNT(<Field>), SUM(*) etc.. are nice
SELECT COUNT(Field) FROM Table GROUP BY OtherField -- SELECTS from EACH GROUP and collates

SELECT AVG(Balance) 
FROM Source1, Source2 -- CROSS-PRODUCT
GROUP BY Field
HAVING AVG(Balance) > <some_num>;

-- Column renaming
SELECT CustType, Count(<Field>) AS Count, ... AS ..., 
FROM <etc>...;

-- Ordering
SELECT Field1, Field2
FROM Customer
ORDER BY Field2 [ASC|DESC] <etc...>; -- Default ASC

SELECT ... LIMIT 5;  -- Max 5 output
SELECT ... OFFSET 5;  -- Drop first 5 output
```

### Joins in SQL

```SQL
-- Inner AKA Equi. An AND
"NOTE THE SUBSCRIPTING FOR DIFFERENT SOURCE, SAME COLNAME"
SELECT ... FROM Customer INNER JOIN Account
    ON Customer.CustomerID = Account.CustomerID;

-- Natural. Note there's no need to state the key. An AND
"ASSUMES KEY(S..?) TO JOIN ON HAVE SAME NAME"
SELECT ... FROM Customer NATURAL JOIN Account;

-- Outer join.
-- This is an OR operation, The DIRECTION you specify who MUST appear in the join. E.g. a LEFT JOIN has all in left, with potentially null matches from RIGHT.
SELECT ... FROM Customer [LEFT|RIGHT] OUTER JOIN Account
    ON <condition> ... ;
```

## Datatypes

- `VARCHAR(n)` occupies up to n length. It is dynamic
- `CHAR(n)` always occupies n length. Strings can be shorter, but datasize fixed. Great if size is always the same
- `ENUM(..)` e.g. `ENUM('G,'PG','M')` is for categoricals
- `TEXT` can have HUGE strings, `MEDIUMTEXT` 16MB (still, p big)
- `TINYINT`, `SMALLINT`, `MEDIUMINT`, `INT`, `BIGINT` in increasing size.
- Can use `UNSIGNED` on the Int types to allow positives only 
- `DOUBLE`, has round error just like C
- `DECIMAL(n,p)` has n total digits and p decimal places. NO ROUND ERROR
- `DATE`, `YEAR`, `TIME`, `DATETIME` has formats `YYYY-MM-DD`, `YYYY`, `HH:MM:SS` and `YYYY-MM-DD HH:MM:SS` respectively. VERY precise

# Query Processing & Optimisation


# Questions
- Clarify the elements of the tables on slide 20; what is data, information, metadata?
- your definitio of key, superkey, candidate key DIFFER to the internet.
    - can a key be a set of fields?
    - (other stuff)
- Is it necessary to assert 'NOT NULL' for what is declared as a key in SQL?
- Meaning of `ON DELETE RESTRICT ON DELETE CASCADE`? DONT NEED TO LEARN IT, BUT LOOK IT UP
- SUPPOSEDLY partial keys are underlined in crow's foot notation
- Rho symbol slide 22 Lec 7
- So in Lec 7 you do rho(ANS(renames)...) but lec 8 you do renaming prior to the cross-product. What's better?
- CARDINALITY(Cross-Product) = CARD(S1) x CARD(S2)... clarify the meaning

- TODO: Need to check possibility of duplicates when using joins.
- TODO: I think the `|` means 'a choice of following - so investigate alternate inputs for the SELECT statements.

Review MONDAY 12.08 lecture for EXACT notations below:

```Markdown
------ OPTIONAL MANY 0..m
====== MANDATORY MANY 1..m
-----> OPTIONAL ONE 0..1
=====> MANDATORY ONE 1..1

Chen's
BOOK === <> <=== PAGE
This 1..m   1..1
Books have 1 to many pages
Pages have 1 to 1 books

Crow's Foot
BOOK ||------|E PAGE
Books have 1 to many pages
Page have 1 to 1 books
OR
CUSTOMER |0-----oE CARD
Customers have 0 to many cards
Cards have 0 to 1 customer
```

