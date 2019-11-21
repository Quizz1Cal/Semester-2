# Notes for Database Systems 2019 Sem 2

### Author: Callum H

# Random

**Data Model**: A collection of concepts for describing data

**Schema**: A descr. of a particular collection of data, using a given data model

# Lecture 1: Introductions

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


**Benefits of DBMS (rel. to FPS)**:
- Data independence: Separation of DATA and PROGRAM/LOGIC with a *data repository* and *central management*
- Minimal data redundancy: Controlled by *normalisation* (to break down a relationship b/t two relations into a set of smaller relations)
- Improved data consistency: Single store, no disagreements, update problems, minmal storage space required
- Improved data sharing: Data shared, corporate resource, external users could access data, and multiple (simultaneous?) views possible
- Program-data independence (a weakness of File processing systems)
- Novel ad hoc data access 'without programming': Query languages e.g. SQL

## File Processing Systems

This is a collection of programs that store/manage *files* in computer. Each program has its own set of data files, often restructured to meet needs of new application.

Flaws:
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

### Data Dictionary
Key fields:
- *key*: Type of key (primary, foreign, neither)
- *attribute*, *data type*
- *not null*: Required or optional
- *unique*: Is the field unique
- *Description*: Useful info on attribute to designer/devprs e.g. attr sizes, valid values etc.

# Lecture 3: ER Modelling Introduction

## ER Model Basics

**Entity**: Real-world object distinguishable from other objects. 
- Described in DB using set of attributes
- Always unique rows

**Entity Set**: Collection of entities of same type
- All entities in an E.S. have same attributes
- Each entity has a key (set) to uniquely identify it

**Relationship**: Association b/t two or more entities. These are determined by the business rules
- *Can have their own attributes, sometimes called **descriptive attributes** (e.g. duration of relationship)*

**Relationship Set**: Collection of relationships of same type. It's an entity as well.
- E.g. Employees work in depts have a start/end date UNIQUE to that relationship.

**Weak Entity**: Identified uniquely ONLY by considering (primary key of) another entity (set) (called the owner). 
- Requires that:
    1. Two entity sets have a one-to-many relationship set (many weak ones)
    2. Weak entity set must have total participation in (what is called) the **identifying relationship**
- Weak entities have a partial key -> only uniquely identified when considering primary key of **owner entity**
- Can be 0:m

## Constraints

**Key constraints** determine the cardinality of relationship -> the no. of objects taking part in relationship set. An 'upper bound'
- Many-to-many (a line)
- One-to-many (a unidirectional arrow). Officially a **key constraint**
- One-to-one

**Participation Constraint** asks if all entities (of a given relationship) take part. A 'lower bound'
- **total** or **partial** participation

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

**Superkey**: A set of fields where no two distinct tuples have the same values in all these fields
- Note that for some sets a superkey cardinality may be more than one due to duplicate values

**candidate key**: A **superkey** with no (strict) subset that is (also) a **superkey** (wrt same relation).
- Sets can have multiple candidate keys (each of which can consist of 1 or more columns)
- In such sets, one key (for each relation) is made **primary key**. The rest **COULD** have been the primary key, but were not chosen.

**Foreign key**: A set of fields in one relation that 'refers to' a tuple in another. 
- They MUST correspond to the *primary key* of the other relation.

**Referential integrity**: Where all foreign key constraints are enforced in a DBMS.
- If a foreign key'd tuple is inserted, reject it
- If a PRIMARY key'd tuple is deleted/updated must debate: delete what refers it, disallow this deletion, have some default PRIMARY key (e.g. `null`)

**Integrity Constraint (IC)**: A condition that must be true for any instance in the database (e.g. *domain constraints*)
- Specified at definition of a schema
- Are checked when relations are modified
- **Legal instances of a relation**: Are those that satisfy all ICs. DBMS should **NOT ALLOW** illegal instances

## Converting to Logical and Physical Models

Conceptual: ER Diagram >>> Logical: "Employee(_ssn_, name, age)" (ssn underlined). 
- Multi-valued & composite attributes need flattening (e.g. `home_num, work_num`, `postcode`, `street_name`, `street_num`) OR a lookup table
    - **Lookup table**: store multi-valued attributes as an instance in a weak entity. CON of this would be the need to 'JOIN' later on.
- When NOT dealing with many-to-many relationship sets, no new relation is necessary (but still usable). "The primary key(s) from the 'many' becomes a foreign, non-primary key in the 'one'" (usually NOT right next to the primary keys to distinguish its role as a foreign key)
    - E.g. Each department has one manager; the manager is MANY because one can manage many depts, but NOT the other way. So the manager's keys are put in department.
- For total participation, include `NOT NULL` next to the relevant field
- For weak entities, the dependent uses the key of the identifying entity as a PFK, not just a FK
- When dealing with BINARY one-to-one relationship sets: *The optional side gets the key of the mandatory side* (e.g. since all CareCenters have a nurse, but not all nurses must have a CareCenter (but max 1), the NURSE is mandatory but the center is OPTIONAL to the nurse - so put the key in the CareCenter)
- When dealing with many-to-many relationship sets, a new relation must be made, whose attributes must include (somewhat intuitively):
    1. Keys for each participating entity set as foreign keys (forms a superkey for the relation)
    2. All descriptive attributes (e.g. start/end date of relationship)
    - E.g. Employee(...), Department(...), Works_In(ssn_u, did_u, since). The latter is called an **associative entity**. Note the keys of the connecting entities become *primary foreign key* (set) but there may be other keys necessary and so is not necessarily a identifying relationship.

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
       ON DELETE CASCADE) -- If Employees has a deletion, THIS gets updated
 )
```

Finally actual instances are defined, where an **Instance** is an actual table with entries.

# Lecture 5: Workbench

## ER Modelling - Notations (Conceptual and Physical)
- LIGHTBULB = key
- MANY LIGHTBULBS = each one a partial identifier, together forms the key
- NOT NULL = Blue diamond, NULL if empty diamond
- RED = Foreign key
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
    CustomerID smallint auto_increment, -- If you don't have your own ID form
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

### Chen's Notation for ER Modelling

Use *squares* for name of set, *ovals* for attribute, *underline* for key, *diamond* for relationship. 
- KEYS: Underlined
- FOREIGN KEYS: ...? TODO 

# Lecture 7: Relational Algebra

**Relational algebra** are operations with input/output relations. 

## Core Operations
1. Selection (\sigma): Selects a subset of rows. **Horizontal filtering**
    - Schema (structure/name/layout of output) identical to input
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
- Pairs of sailors with same rating
    - `\rho(C(1->sid1, 2->sid5), S |X| S)`
    - `\pi_{sid1, sid2}\sigma_{rating1=rating2} (S |x| S)` (need natural to avoid (same,same))

# Lecture 8: SQL

## Common Commands

```SQL

-- NOTE: Case INsensitive

-- Select is equivalent to PROJECT in SQL. No removal of dupes so
SELECT * from Table as ALIAS;
SELECT DISTINCT <rows,...> FROM <Table>;

... WHERE Field LIKE "<annoying REG_EXP....>" -- Regular expressions
... WHERE Field IN (subquery)


-- ORDER IS IMPORTANT
-- General format
-- Aliases DON'T WORK in WHERE
SELECT [ALL|DISTINCT]                           -- Rows/calc'd fields. DISTINCT to discard dupes
    FROM <tables to cross product,...>          -- Source
    <JOIN SPECIFICATIONS>
    WHERE cond1 AND cond2 OR ...                -- Self-exp.
    GROUP BY {col_name | expr} [ASC|DESC]...,   -- Duh 
    HAVING cond                                 -- A selection condition on the GROUPS to include
    ORDER BY {col_name|expr|position} [ASC|DESC, ..] -- Sorting 
    LIMIT {[offset, ] row_count | row_count OFFSET offset}; -- Output constriction

-- Some non-aggregators (they just map data)
SQRT(), CONCAT(field1, ' ', ...) -- (str concat)
-- Some aggregators. Not that excluding count, they all ignore nulls. THESE SUMMARISE RESULTS
AVG(), MIN(), MAX(), COUNT(<Field>), SUM(*) etc.. are nice
SELECT COUNT(Field) as Totals FROM Table GROUP BY OtherField -- SELECTS from EACH GROUP and collates

-- Comparators
ANY (), ALL (), IN, NOT IN

SELECT AVG(Balance) 
FROM Source1 as Alias1, Source2 as Alias2 -- CROSS-PRODUCT
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
-- Inner AKA Equi-Join. An AND with condition
"NOTE THE SUBSCRIPTING FOR DIFFERENT SOURCE, SAME COLNAME"
SELECT ... FROM Customer INNER JOIN Account
    ON Customer.CustomerID = Account.CustomerID;
    -- supposedly,`USING (CustomerID)` is also valid syntax

-- Natural. Note there's no need to state the key. An AND
"ASSUMES KEY(S..?) TO JOIN ON HAVE SAME NAME"
SELECT ... FROM Customer NATURAL JOIN Account;

-- Outer join.
-- This is an OR operation, The DIRECTION you specify who MUST appear in the join. E.g. a LEFT JOIN has all in left, with potentially null matches from RIGHT.
SELECT ... FROM Customer 
[LEFT|RIGHT|FULL] JOIN Account
    ON <condition> ... ;
```

You can union tables through `Table1 <all the code> UNION Table2 <moarcode>`

### More Advanced DML

**Query Nesting**: Another select query within some other query, e.g. for a set test.

### More Advanced DDL

### Views

These are relations NOT in the conceptual/logical model, but are made available to the user as a virtual relation.
- Hide query complexity
- Hide data from users

To create them, rather than `CREATE TABLE`, just do `CREATE VIEW`.

```SQL
-- Insertion. Table must already exist
-- REPLACE == INSERT with the power to override
INSERT INTO Customer 
    (CustFirstName, otherCols...) -- If you exclude, ALL columns need entry
    VALUES ("Peter", ...);

-- Update an existing table
UPDATE Hourly
    SET Col = Col*1.10
    WHERE cond; -- This way you don't update whole table

    -- Use of the 'if' statement
    SET Col2 = 
        CASE
            WHEN ...
            THEN ...
            ELSE ...
        END;

-- DELETE
DELETE FROM Employee; -- KILLS ALL
    WHERE ...; -- Be safe

-- ALTER: Add or remove attributes
ALTER TABLE Table [ADD|DROP] ....

-- RENAME TABLE
RENAME TABLE Table1 TO BetterName
```

#### Advanced

Question: "List the first and last names of bosses who supervise more than two staff and who also manage a department."

**NOTE**: The aliasing scope is the ENTIRE query even if the aliasing is done in Joins.

```SQL
SELECT boss.employeeid, boss.firstname, boss.lastname,
 COUNT(emp.employeeid) AS employee_count
FROM employee AS emp
INNER JOIN employee AS boss ON emp.bossid = boss.employeeid
WHERE boss.employeeid IN (SELECT managerid
 FROM department)
GROUP BY boss.employeeid
HAVING COUNT(emp.employeeid) > 2;
```

**ALL, ANY**: Basically allows `WHERE sal > ALL(200,300,400)` rather than `where sal > 200 AND sal > 300 AND sal > 400`, similar for ANY

### Datatypes

- `VARCHAR(n)` occupies up to n length. It is dynamic
- `CHAR(n)` always occupies n length. Strings can be shorter, but datasize fixed. Great if size is always the same
- `ENUM(..)` e.g. `ENUM('G,'PG','M')` is for categoricals
- `TEXT` can have HUGE strings, `MEDIUMTEXT` 16MB (still, p big)
- `TINYINT`, `SMALLINT`, `MEDIUMINT`, `INT`, `BIGINT` in increasing size.
- Can use `UNSIGNED` on the Int types to allow positives only 
- `DOUBLE`, has round error just like C
- `DECIMAL(n,p)` has n total digits and p decimal places. NO ROUND ERROR
- `DATE`, `YEAR`, `TIME`, `DATETIME` has formats `YYYY-MM-DD`, `YYYY`, `HH:MM:SS` and `YYYY-MM-DD HH:MM:SS` respectively. VERY precise

# Storage and Indexing

Any DBMS must support:
- Insertion/deletion/modification of records
- Reading records (specified with some record id)
- Scanning records

**File**: Collection of pages with a collection of records.
- **Heap File**: Records unordered. Alloc/deallocates *disk pages* as the file grows/shrinks. Can be structured with pointers much like a LL
    - +: Good when records are accessed uniformly/altogether.
    - +: Fast insert
- **Sorted Files**: Records sorted by some condition. Similar LL structure BUT pages/records are ordered.
    - +: Good where records are pulled in some order, retrieving some range
    - +: Fast search (binary) on range queries (log2 B); hard to maintain (due to resorting in insert)
- **Index File Organisations**: Special DS, has the fastest retrieval in some order.

**NOTE**: The data is typically stored in pages on Hard Disks (HDD), which is processed/analyzed in memory (RAM).

An **index** on a file is a DS built on top of data pages. It is located/stored elsewhere, such as an index file, containing a collection of *data entries*.
- Built over *search key fields* e.g. Department Name. CAN have many (e.g. a subset of features)
- Helps to speed up searches on these fields as it stores pointers to a range of data entries (any subset of fields), which in turn link to the data records themselves

### Types of Index
- Clustered vs Unclustered: Order of data records matches order of index data entries (I.e. are the small things proximal or sparsely spread in the file?)
    - IDEA: Already sorted, just have an index to aid lookup
    - Can only have one search key combination clustered (forces all others to be unsorted)
    - Always cheaper (range) search/retrieval if clustered, but expensive to maintain (need to reorganize files to do so)
        - Clustered: Roughly costs **# pages with matches**
        - Unclustered: Roughly costs **# entries matched**
- Primary vs Secondary: Includes the table's primary key; secondary otherwise.
    - Primary never contains duplicates, but secondary can.
- Single vs Composite Key: Whether the index is constructed of one or more search keys
    - Data entries in INDEX sorted by search keys
    - Easier search on composite conditions
- Indexing Techniques:
    - **Tree-based**: (What we've done so far): Uses a binary (B+) tree, (no need to sort files for this), nodes point to lower levels (left is low, right is high). Leaves with data entries sorted by s.k values. GREAT FOR RANGE, ALSO OK FOR EQUALITY
        - Height typically small, 2-4 I/O
        - **B+ trees**: Store data in LEAVES only; all internal nodes store keys that map to children. Note that the DATA is still linked files
    - **Hash-based**: Index is a collection of buckets; function maps search key to the bucket. IDEAL FOR EQUALITY (ONLY)
        - Height typically small, 1.2 I/O operations

## Comparing Heap/Sorted Files

Use **disk I/Os** to measure performance. One cost per page access.
- E.g. accessing a file of 100 records in 10 pages is 10 I/O in cost

### TABLE OF COST COMPARISON

Operation | Heap File | Sorted File
Scan | B | B
1 Eq Search | 0.5B avg | log2B
Range Search | B | log2B + #matches
Insert | 2 | log2B + 2*(B/2)
Delete | 0.5B + 1 | log2B + 2*(B/2)

# Lecture 11/12: Query Processing

**Query workflow idea**:
- Query parser interprets SQL/query (maybe rewrite for machine readability)
- Query optimisers create plans and cost estimates
    - Communicates with catalog manager (with access to schema/stats)
- This yields a plan evaluator

## Selection (sigma) Implementation

Optimal selection depends on
- *available indexes/access paths*
- *expected result size* (# tuples or # pages) approx. ~ size(relation) * PRODUCT(reduction factors)

A **reduction factor/selectivity** estimates the prop. of relation will qualify the condition/predicate. 
- Estimated by optimiser

### Ultimate Approach to Selections
1. Find cheapest access path (see alternatives below)
2. Retrieve tuples using it
    - Where predicates MATCH this index, the # tuples retrieved is reduced (hence, cost saved)
3. Apply predicates that DON'T match index later on
    - Does NOT affect # tuples/pages fetched; a post-retrieval task

Alternatives for simple selection:
- NO INDEX, UNSORTED: Cost = N (NPages(R))
- NO INDEX, SORTED: Cost = log2(N) + (RF*N) i.e. log2(N) + result_size
- INDEX ON SELECTION ATTR: See below

### Using an Index for Selections
- Cost: 
    - Clustered: RFs * (NPages(I) + NPages(R))
    - Unclustered: RFs * (NPages(I) + NTuples(R))
- Steps:
    1. Find qualifying data ENTRIES (typically small) (RF) & go through one-by-one and look up data records (NPages(I))
    2. Retrieve data records (either pages, or tuples, dep. on clustering)

### General Selection Conditions
- A B-tree index matches predicates that are a PREFIX of the search key (**matching predicates/primary conjuncts**)
    - E.g. <a,b,c> index can do (a,b,c), (a,b) or (a)
    - E.g. <a,b,c> can do (a=5,b=3) but NOT (b=3)
    - HENCE only RF(predicates in the prefix) can be used to det. cost

## Projection (pi) Implementation

Motivation for algorithm: extracting attributes is easy: removing duplicates (to remain a true relation) is the task to optimise.

### Sort-Based Projection
(Basic) Steps: 
    1. Scan R, extract necessary attributes
    2. Sort result set (typically external merge sort)
    3. Remove *adjacent* duplicates

**Total Cost**:
- ReadTable (keeping only proj. attrs) = NPages(R)
- WriteProjectedPages (wriing pages wt projected attrs to disk) = NPages(R)*PF
- SortingCost (sorting with proj. attrs with external sort) = 2 * NumPasses * ReadProjectedPages
    - **TODO**: HOW TF DOES 20 MEMORY PAGES LEAD TO 2 PASSES
    - The 2 comes from the fact it's # reads + # writes
- ReadProjectedPages (read sorted, proj. pages to discard adj. dupes)

**Projection Factor** measures how much we are projecting (e.g. keepin 1/4 of all attrs)

**External merge sort**: Sort on large data consisting of several passes and use of **buffer pages**
- Sort run: Make each B pages sorted
- Merge run: Make multiple pages, in order to merge runs
    - At pass K, produce runs of length B(B-1)^(K-1)
    **WE WILL BE TOLD # PASSES P**
    - **TODO**: VERY CONFUSING. Wiki helps a bit but still...

### Hash-Based Projection
(Basic) Steps:
1. Scan R & extract necessary attrs
2. Hash into BUCKETS:
    - Apply h1 to choose one of B output buffers
3. Remove adjacent duples from a bucket

**TODO**: It says B above, but acc. diagram it's ONE input bucket and B-1 hashing buckets.

For External Hashing:
1. Partition data into B partitions with h1
    - Read R with ONE input buffer
    - For each tuple: discard unwanted attrs, and apply h1 to map to a B-1 *output buffer* (main memory)
        - Note that tuples in diff buckets are necessarily distinct by nature of hash
2. Load each partition, hash with h2 and eliminate duplicates
    - For each partition: Read and build an in-memory hash table using h2 (<> h1), discarding duplicates
    - If partition not fit in memory: apply hash-based projection algorithms (NOT FOR US)

**Cost**:
- ReadTable (read and project) = NPages(R)
- WriteProjectedPages (h1-partitioning ('buckets'))= NPages(R)*PF
- ReadProjectedPages (h2-partitioning and discard dupes) = NPages(R)*PF
- Overall: NPages(R) * (1+2*PF)

<!---Lecture 12--->
## Join (|X|) Implementation
NOTE: Expensive.

### Nested loops Join

#### Simple NLJ (SNLJ)
IDEA: 
- Foreach tuple r in R do
    - Foreach tuple s in S do
        - If rj==sj then add <r,s> to result

**Cost(SNLJ)** = NPages(Outer) + NTuples(Outer)*NPages(Inner)
**TODO**: Clarify why OUTER specifically.

#### Page-Oriented NLJ (PNLJ, NLJ)
IDEA:
```Pseudocode
Foreach page b_R in R do
  Foreach page b_S in S do
    Foreach tuple r in b_R do
      Foreach tuple s in b_S do
        if rj == sj then add <r.s> to result
```

**Cost(PNLJ)** = NPages(Outer) + NPages(Outer)*NPages(Inner)
- **TODO**: Why no tuple cost mentioned here?

#### Block NLJ (BNLJ)
An alternative approach to PNLJ (which fails to exploit extra memory buffers):
- Use one page as an *input buffer* for scanning S
- Use on page as the *output buffer*
- Use rest to hold ONE block of R (a chunk read)

Algorithm: 
Foreach Rblock B, foreach tuple r in B, foreach tuple s in S, where rj==sj add <r,s> to result.

**Cost (BNLJ)** = NPages(Outer) + NBlocks(Outer) * NPages(Inner)
- NBlocks(Outer) = ceil(NPages(Outer) / (B-2)) (B is # buffer pages)

### Sort-Merge Join (SMJ)
IDEA: Sort R and S on the join column, THEN scan to do a merge (on join column) and output result tuples.
- Sorted R is scanned ONCE
- Each S group of the same key values is scanned ONCE per matching R tuples (typically, sorted S scanned ONCE too)

Uses:
- One/both outputs already sorted in the join attr(s)
- Output is required to be sorted on join attr(s)

**Cost (SMJ)** = Sort(O) + Sort(I) + NPages(O) + NPages(I)
- First two for sorting, second two for merging (cost)
- Sort(R) = External Sort Cost = 2 * NumPasses * NPages(R)

### Hash Join (HJ)
IDEA: Partition BOTH relations with h -> only tuples in same partition can match. Then foreach partition of R, hash with h2 (<> h), scan matching partition of S and probe for matches (after applying h2)

**Cost (HJ)** = 3* NPages(O) + 3* NPages(I)
- In partitioning, read+write (contributes 2); in matching, read both (once)

### General Join Conditions
For equalities over several attributes:
- For SMJ and HJ, sort/partition on combination of the TWO join columns

Inequality conditions (e.g. R.name < S.name):
- CANNOT use HJ (due to use of hashing)
- CANNOT use SMJ **TODO**: Don't know why
- Use BNLJ

<!--- Lecture 13 --->
# Lecture 13/14: Query Optimisation
GOAL: Find the execution strategy (choice of algorithms, etc.) with the lowest cost.

**Query Plan**: A tree, with relational algebra operators as nodes, labelled with a choice of algorithm.
- Leaves are the relations
- Example nodes: \pi_{attrs}, \sigma_{conditions}, |X|_{conditions}
- Example labels: On-the-fly, Heap scan, names for algorithms

**Optimisation Steps**:
1. Break into query blocks.
    - **Query block**: Any statement starting with select; a unit of optimisation
    - Typically optimise innermost block and work outwards
2. Convert each block into relational algebra
    - Just like in Lecture 7, e.g. `\pi_{S.sid}(\sigma_{B.color='red'}(S |x| R |x| B))`
3. Foreach block, consider alternative query plans
4. Plan with lowest estimated cost selected

## Relational Algebra Equivalences
- **Selection Cascading**: A sigma on N conditions is equivalent to N nested selections
- **Selection Commuting**: Nested selections can be composed in any order
- **Projection Cascading**: Projection on a_1 is equivalent to composing on projections of smaller and smaller supersets that converge to a_1.
    -  `\pi_{a_1}(R)\equiv\pi_{a_1}(\dots(\pi_{a_n}(R)))` where `a_i\subseteq a_{i+1} \forall i\in\{1,\dots,n-1\}`
- **Proj/Selection Commuting**: Proj/selections can be composed in any order IF the selection condition only uses attributes retained by the projection.
- **Join Associativity**: Joins are associative (always) (any composition)
- **Join Commutativity**: Joins are commutative (always) (any choice of inner/outer)
- **Two-sided Selection/Cross-Product**: Selections on a cross-product are equivalent to joins with that condition.
- **One-sided Selection/Join**: Selections on ONE side of any join (even with conditions) can be applied to that side before OR after the join.
- **Projection/Join**: Projecting on a join is equivalent if you first project on the relations (whilst retaining join attrs), then project after the join. I.e. you can trim the fat before joining, but you still have to choose afterwards.
    - NOTE: Ensure that the superset containing BOTH projection attrs and the join condition attrs is used if done prior to join (otherwise you can't actually do the join)

## Cost Estimation
For each plan considered:
- Estimate size of result (for each operation in tree)
    - Exploit information a/b input relation (from system catalogs) & apply rules
- Estimate cost of operations in plan tree
    - Depends on input cardinalities (row counts)
    - This is done with the discussions from **query processing**
    - Use these to calculate cost of entire plans

**Catalogs**: Information on the relations and indexes involved. Often contain (at least) the following *statistics*:
- NTuples, NPages, for each relation
- NKeys for indexes (or relation attrs)
- Low/High key values for indexes (or relation attrs)
- Height(I) of index for each tree index
- NPages(I) for each tree index

### Result Size Estimation
- *Single Table*: ResultSize = NTuples(R) * PRODUCT(RF's)
- *Joins on k tables*: ResultSize = PRODUCT(NTuples(Rj)) * PRODUCT(RF's)
    - **TODO**: Nested product or independent products? (big difference)
- I.e. maximum NTuples from a result is the product of cardinalities (size) of relations in `FROM`; but reduction factors on `WHERE` predicates reflects their impact on reducing result size (hence, called 'selectivity')

**RF Calculations**:
- Col = Val, do 1 / NKeys(col) (NOTE: assumes uniformity)
- Col > Value, do (High(Col) - Value) / (High(Col) - Low(Col)) (NOTE: again assumes uniformly spread)
- Col < Value, do (val - Low(Col)) / (High(Col) - Low(Col)) = 1 - (above)
- ColA = ColB (for joins), do 1 / MAX(NKeys(ColA), NKeys(ColB))
- Without any info on NKeys or interval range, use magic number RF = 0.1

## Plan Enumeration

### Single-Relation Plans
- Each *available* access path (file scan/index) is considered - choose lowest estimated cost
    - Heap scan is ALWAYS one alternative
    - ALT: Each index (as long as it matches selection predicates)
- Other operations can be performed on top of access paths but typically DON'T incur additional cost as they are done on-the-fly

Examples:
- Heap scan: NPages(R)
- Index on primary key: 
    - Cost(B+Tree) = Height(I)+1
    - Cost(HashIndex) = ProbeCost(I) + 1 (here, about 1.2)
- Clustered index on one or more predicates
    - Cost(B+Tree) = PRODUCT(RF's) * (NPages(I) + NPages(R))
    - Cost(HashIndex) = PRODUCT(RF's) * NPages(R) * 2.2 (p. sure this is ProbeCost(I)+1)
- Non-clustered index on one or more predicates
    - Cost(B+tree) = PRODUCT(RF's) * (NPages(I) + NTuples(R))
    - Cost(HashIndex) = PRODUCT(RF's) * NTuples(R) * 2.2

**NOTE/REMEMBER**: If the index is NOT related to the condition you'll have to check all entries - in other words, RF = 1.

**TODO**: Don't understand the exact derivation of all formulae in Slide 6 Lec 14.

### Multi-Relation Plans
1. Select order of relations (N!)
2. For each join, select an algorithm
3. For each input relation, select an access method
4. (from all available paths, choose that with least est. cost)

This understandably yields a large search space.
- In System R (first DBMS), reduce by asserting *only left-deep join trees are considered*
    - generates fully pipelined plans, where intermediate results are NOT written to temp files; they are immediately joined with another relation, etc.
    - This means you don't have to count the reading of previously joined data (pipelining) (e.g. in ONLJ, remove one NPages(Outer); in HJ, only do 2* NPages(Outer) instead of 3*NPages(Outer) as it's already read)
- Can also prune any plans with cross-products (if there are conditions to be met), as this is less efficient then first applying those conditions in a join
- **TODO**: Why 40000 and not 100000 on Slide 14 lec 14? NKeys is NTuples coz it's id's right? 
- **TODO**: Motivate why the structure chosen was used in Lec 14; it is because B had the least # Pages, or just that only R |X| S was known?s


# Lecture 15: Normalisation

Denormalised data is essentially the join of different entities in one database. For example, a Student entity that has their course details as part of their record - so many records have identical course details.

Motivation: Data that is not normalised can suffer from:
- *Insertion Anomaly*: A new 'inner object' cannot be added until another 'outer object' has been recorded 
- *Deletion Anomaly*: Inner object/data can be wiped from existence upon deletion of an outer object/data
- *Update Anomaly*: Changes to any inner data (e.g. course details) have to be rippled through the dataset due to repetition

The tradeoff:
- Normalised relations have
    - Minimal redundancy
    - Allows insert, modify/update and delete without error/inconsistency
    - Poor(er) query speed
- Denormalised relations have
    - greater query speed (all un-nested, essentially O(1) access)
    - Poor(er) update speed

**Normalisation**: A technique used to remove undesired redundancy from databases; breaking one large table into several smaller tables.
- A relation is **normalised** if all determinants are candidate keys
- Normalise by isolating non-key determinants into separate tables, recursively (adding foreign keys to link), until the normalisation condition is met for ALL tables.
- Requires lateral thinking: anything from customer identity to calculated fields needs to be optimised and isolated.

**Functional Dependency (X -> Y)**: In regards to attrs in a relation, a set of attributes X determines a set Y if each value of X associates with one Y. We call any set X a **determinant**
    - Can consist of *key* and *non-key attributes*
    - E.g. `employee_id` determines `first_name`, `last_name`, `date_of_birth` etc. but NOT vice-versa
    - Determinants may not be candidate keys (e.g. `course_name` determines `course_details` but not the student details that form the bulk of the record)
    - **Partial FD**: An FD between a strict subset of the primary key, and one or more non-key attributes
    - **Transitive Dependency**: An FD between 2 or more non-key attributes.

**Armstrong's Axioms** denote functional dependencies:
- `A = (X1,X2,...,Xn)` and `B = (Y1,Y2,...,Yn)`
- **TODO**: How to denote the dependencies with this notation (wasn't in lecture slides)
- *Reflexivity*: B \subseteq A => A -> B (supersets determine any subsets)
- *Augmentation*: A -> B => AC -> BC (you can add 'redundant' attrs)
- *Transitivity*: A -> B, B -> C => A -> C

## Steps in Normalisation
1. First Normal Form *1NF*: remove repeating groups (keep atomic data)
    - Removing repetitions: Turn "Order-Item(<u>Order#<\u>, Customer#, (<u>Item#<\u>, Desc, Qty))" into `Order-Item(<u>Order#, Item#<\u>, Desc, Qty)` and `Order(<u>Order#<\u>, Customer#)`
    - Analogous if a cell had multiple values
2. Second Normal Form *2NF*: Remove partial dependencies
    - *IDEA*: A non-key attribute cannot be identified by PART of a composite key. 
    - So "Order-Item(<u>Order#, Item#<\u>, Desc, Qty)" becomes "Item(<u>Item#<\u>,Desc)" and "Order-Item(<u>Order#, Item#<\u>,Qty)" since you can determine Desc just by Item#
3. Third Normal Form *3NF*: Remove transitive dependencies
    - *IDEA*: A non-key attribute cannot be identified by another non-key attribute (this would imply Key -> Attrs -> other-attrs which is inefficient)
    - So "Employee(<u>Emp#<\u>,Ename, Dept#, Dname)" will become "Employee(<u>Emp#<\u>, Ename, Dept#)" and "Department(<u>Dept#<\u>, Dname)" as Dept# determines Dname (transitive dep.)

# Lecture 16: Guest Lecturing on Adaptive Databases

# Lecture 17: Database Administration

Core aspect of a DA role (which cloud-based DBMS effectively do)
- **Capacity Planning**: Predicting future load levels and determining cost-effective ways to delay system saturation
    - In System Design: Consider disk space requirements, transaction thoroughput (traffic)
    - In Systems Maintenance/Review: Monitor/predict the above
- **Backup & recovery**: Types of failure, failure responses, types of backups, general protection
- EXTRA: **Performance Improvement**
- EXTRA: **Security** (threats, SQL injections etc.)


## Capacity Planning (REVIEW CALCULATIONS)
 **To estimate disk space requirements**:
- Treat database size as sum of ALL table sizes, where size = cardinality * average row width
    - Row width = sum of datatype sizes
    - This will differ b/t vendors
    **TODO**: There was a lot of detail in the slides, unsure if necessary
    **TODO**: What is 'go-live'?

**To estimate transaction load**:
- Transaction load = PRODUCT(frequency * load(SQL statements)) for all products (informally)
- Need to consider:
    - Peak vs Average load (just because you allow 200 chars max for an email, about 66% of that is used on average, so use 66% * 200 in estimation IF truncated)
    - Acceptable response time
    - Availability

## Backup & Recovery
**Backups** are (basically) copies of your data that allows restoration in the event of:
- Human error, hard/software malfunction, malicious activity, disasters, and government regulation

**Categories of failure**:
- Statement failure: Syntactically incorrect
- User Process failure: Process fails (dies, errors)
- Network failure: Connection b/t DB and user fails
- User error: User accidentally drops rows/tables/DB
- Memory failure: Memory fails/corrupts
- Media failure: Disk failure/corruption/deletion

**Categories of Backups**:
- Physical:
    - Raw copies 
    - Preferably offline DB when backup occurs
    - Backup is EXACT COPY of directory and files
    - Backup include logs
    - Only portable to machines with similar config
    - To restore: shut down DBMS, copy backup over, restart DBMS
- Logical:
    - Backup completed by SQL queries
    - SLOWER than physical (due to selects)
    - Output LARGER than physical
    - No log/config files
    - Machine independent
    - Server available during backup
    - Restore/backup through MySQL / other DBMS
- Online:
    - Backups occur whilst 'live'
    - Clients unaware
    - Need appropriate **locking** to ensure integrity
- Offline:
    - Occur when database is stopped
    - SIMPLER
    - Preferable BUT not always possible due to need to stay online
- Full: 
    - The complete DB is backed up (with any categorisation of the above)
    - Includes everything necessary to get an operational DB in failure
- Incremental:
    - Only changes since last backup are backed up (e.g. for many DB, just backup log files)
    - Restoration: stop database, copy backed up log files to disk, start, and redo log files
- Offsite:
    - Data is stored on disk separate from main disks (e.g. CLOUD)
    - Allows disaster recovery

Basic Backup Policy:
- Most backup strategies are a combination of full and incremental backups (e.g. full weekly, daily incremental).
- Should conduct when database load is low
- If using replication, backup to the mirror database to negate performance concerns to primary
- TEST backup before you NEED the backup

# Lecture 18: Transactions

**Transaction**: A logical unit of work that must either be entirely completed, or aborted (*indivisible*). 
    - User-defined
    - In essence, a sequence of DML statements (manipulation)

**Desired properties (ACID)** (defining a unit of work):
- Atomicity: Transactions are single, indivisible, logical UOW and ALL must be completed or NONE of them.
- Consistency: Data constraints must hold post-operation (e.g. PKs, FKS)
    - This would be broken if transactions were divisible - partial inconsistent states could error in failures
- Isolation: Data used by one transaction cannot be used in parallel by another until the first completes
    - Serial execution (a queue)
- Durability: Complete transactions permanently alter, even in system failure

IN SQL: Use `START TRANSACTION` or `BEGIN` and `COMMIT;` to wrap a series of SQL statements. `ROLLBACK` undoes everything.
- `UPDATE` helps too...? 
- A single DML/DDL command that fails halfway through its processing, upon restart, will NOT have changed the DB

**Concurrent Access** (the second problem):
- **Lost Update Problem**: Concurrent transactions have different ideas of the state (updates made by concurrents are 'lost')
    - E.g. multiple withdrawals: they'll have different ideas of the final balance
- **Uncommitted Data Problem** occurs when two transactions execute concurrently * the first is rolled back AFTER the second has already accessed it. 
    - E.g. cancelling a withdrawal just after someone else started one; they'll read the wrong (lower) balance
- **Inconsistent Retrieval Problem** occurs when one transaction calculates an aggregate whilst others are updating the data.

Transactions ideally are serializable (appear serial in execution, but allows concurrent execution yielding consistent result). Thus is slow and unrealistic.

**Concurrency Control Methods**:
- Scheduling r/w operations for concurrent transactions
- Interleaves operation execution based on CC algorithms (e.g. locking, time-stamping)

**Locking**: Guarantees exclusive use of a data item to a current transaction
- Forces concurrents to WAIT to ensure consistent data is retrieved
- **Lock Managers** assign and police the locks used
- Lock Granularities:
    - **Database-level**: Locks entire DB, so even if different tables are uesd the transactions are still locked.
    - **Table-level**: Less strict, but not suitably for highly multi-user DBMSs (or highly dependent systems)
    - **Page-level**: Uncommon
    - **Row-level**: Best availability to data, but high overhead. MOST COMMON
    - **Field-level**: Most flexible, but highest overhead. UNCOMMON
- Lock types:
    - **Binary**: Either locked (1) or unlocked (0). Solves LOST UPDATE
        - ISSUE... **TODO**
    - **Exclusive**: Access reserved to the locking transaction; important for transactions that intend to write; only possible if no other locks held
    - **Shared**: Other transactions can read; only okay if transaction wants to READ and no Exclusive lock is held

**Deadlock**: (Permanent) error that occurs where multiple users place locks that prevent either party from completing their transaction, creating a standstill (waits).
- Solution: Allow simultaneous read AND write; deny READ and WRITE (exclusivity); allow backing out of transactions; prevent cycles

**Alternative CC Methods**:
- **Timestamp**: Transactions get a global, unique timestamp, which attaches to data accessed. Transactions are allowed access depending on the timestamp.
- **Optimistic**: Assuming majority of operations DON'T conflict, transactions are free to execute BUT on commit, DBMS checks if anything READ has been altered, and rollbacks anything that doesn't add up

**Transaction Logs** track all updates to data. It contains records for transaction starts, the SQL statements being performed and the impacts, the before/after values, and pointers to prior/next log entries, AND the commit.
- Allows restoration (just look at the transactions and restore)

**DO THE TRANSACTION DEMONSTRATION**

## Data Warehousing

**Informational Database**: A single database that allows *all* organisations' data to be stored in a form that can be used to support organisational decision processes.

**Data Warehouse**: Single repo. of organisational data:

Basic features:
- TERABYTES of data
- Integrates data from multiple sources (Extracts from source systems, transforms loads)
- Makes it available to managers/users
- Supports analysis and decision making

Characteristics of a dW:
- Subject oriented: organised a/b particular subjects e.g. sales, customers
- Validated, Integrated data: Data converted to a COMMON FORMAT and then VALIDATED before storing in DW
- Time variant: COntains HISTORICAL data, allows TREND ANALYSIS, and is basically a series of SNAPSHOTS that are time-stamped
- Non-volatile: Users have READ ACCESS ONLY = updates done automatically by ETL process and periodically by DBA

**DW Supports Analytical Queries** such as :
- How many, average, total (numerical/fact aggregations)
- By state by cust. type ... (these are called *dimensions*)

Example architecture:
- Source systems 
    - Internal
    - External
- Data Staging Area
    - Processing e.g. clean, derive, amtch, combine...
- Data & Metadata Storage Area
- Analytics & Reporting
    - querying, dashboards etc.

**Star Schema Design (aka dimensional modelling)**: Consists of 
- Fact table:  
    - Contains actual business MEASURES (additive, aggregates) called facts. 
    - Also has FOREIGN KEYs pointing to DIMENSIONS
    - E.g. a 'Sale' fact table has 'time, store, customer' KEYS and 'dollar sales, unit sales' FACTS (AGGREGATED)
- Several dimensional tables:  
    - Consists of a KEY and potentially a hierarchy of categories
        - E.g. 'Product key, product name, product type'. The first is the KEY, then type is broad cat, name a sub-cat.
        - Hierarchy is BOTTOM to TOP
- (sometimes) hierarchies in dimensions
- E.G. ITS A SIMPLE, RESTRICTED ER MODEL

The *star schema* model has a Fact in the CENTRE, and the dimension TABLES (also fact tables) connected to it.
- Relationships b/t fact and dimension is (1:m) to (1:1)

**Designing a DM**:
1. Choose a business process (e.g. sales)
2. Choose measured facts (e.g. quantity, price)
3. Choose granularity (e.g. # categories)
4. Choose dimensions (the categories)
5. Complete dimensional tables

*Technically*, a dimensional table has *embedded hierarchies*: categories like industry group, class sector in a way are ANOTHER hierarchy of dimensions. 
- In such a drawing (each box is a RECTANGLE Btw), typically a (1:1) and (0:m) relation (all customers have one industry class, not all industry classes may have a customer, but can have many)
- This causes a "snowflake schema": a fact has dimensions, which themselves have embedded hierarchies.

## Distributed Databases

**Distributed Database**: Single, logical database PHYSICALLY spread a/c MANY computers, MANU locations, connected by a data comm link
    - But APPEARS to USERS as one database

**Decentralized Database**: Collection of indept databases NOT networked together as one logical db.
    - APPEARS to USERS as many databases

**Advantages of Distributed DBMS**:
- Good fit for geo. distributed orgs/users (utilize Internet)
- Data located near site with greatest demand (e.g. sporting)
- Faster data access to LOCAL data
- Faster data processing (workload SHARED)
- Allows MODULAR growth (horizontal scaling ... more servers)
- Increased reliability/availability (less danger of SINGLE point of failure (SPOF))
- Supports database recovery (replicated a/c multiple sites)

**Disadvantages of Distributed DBMS**:
- Complexity of mgmt, control (stitching, version control, who updates, how web understands all this)
- Data integrity (improper updating, who 'wins' if two updates occur) .. solved by Transaction manager, Master-slave design
- Security (higher chance of breaches due to more sites) ... lots of infrastructure
- Standards (different vendors use diff protocols)
- Training/maintenance costs (complex infrastructure; disks + fast + clustering = $$$)
- Storage req. (replication model)

**Objectives of Distributed DBMS**:
- Location transparency: 
    - geo location IRREl. to program/user
    - Requests are forwarded by system to site(s) relevant
    - Data appears as SINGLE logical database at one site to any user
    - Queries can join data from tables in many sites
- Local autonomy: 
    - Nodes can function locally if network connxn lost, allowing for local security, data control, transaction logs
    - Can RECOVER from local failures
    - Provide full access to LOCAL data
- Trade-offs:
    - availability vs consistency
    - Synchronous vs Asynchronous updates
    - CAP theorem (see NoSQL)

**Distribution Options**:
- Data replication: data copied a/c sites
    - Advantages:
        - High reliability (due to copies)
        - Fast access
        - Avoid complicated integrity routines (just refresh @ scheduled intervals)
        - Decoupling aok
        - Reduced traffic... if delay updates
        - POPULAR
    - Disadvantage:
        - More storage
        - Data Integrity ... be careful a/b out-of-date data, performance impacted for busy nodes, and incorr. retrieval could occur
        - Takes time for updates
            - Lots of back/forth commxn
        - Network communication capabilities
            - High demand on telecomms
            - Costs$$$$$$
    - THEREFORE better for non-volatile, read-only data
    - Synchronous updating:
        - Continuously maintained; updates spread to all databases OR is aborted (it has to update everywhere) ATOMIC UPDATE
        - Ensures data integrity, minimise complexity of knowing where 'recent' is
        - = SLOW, high usage of networks
    - Asynchronous updating:
        - There is DELAY in propagating updates to remote dbs (with some degree of temporary inconsistency allowed)
        - Acceptable time (updates locally, replicas synchronized in BATCHES at intervals)
        - More complex to plan/design to 'get the level right'
        - Suits some things more than others (e.g. commerce vs social media)
- Horizontal partitioning: Table rows distributed b/t sites
- Vertical partitioning: Columns distributed b/t sites
    - Can split based on a dimension/feature (e.g. city), choice is yours really
    - Advantages for BOTH:
        - Efficient: stored near use point
        - Performance: local optimization for access
        - Security: Only relevant data is local
        - Ease: Unions a/c partitions are easy
    - Disadvantages for BOTH:
        - Inconsistent speed: due to accessing ALL partitions
        - SPOF: No data replication
    - Relative pro of Horizontal:
        - Combining data is harder for vert. due to joins instead of unions
    - SAME ADV
- Centralised: all in one place

REFER TO THE GREAT SLIDE 26 IN THE LECTURE

## NoSQL

**Pros of Relational DB**:
- Simple
- Can integrate multiple appxns (via shared data store)
- Standard interface SQL
- Ad-hoc queries, across/within aggregates
- Fast, reliable, concurrent, consistent

**Cons of Relational DB**:
- Object relational impedance mismatch (doesn't have encapsulation, abstraction/inheritance, accessibility)
- Bad with BIG DATA
- Bad with CLUSTERED/replicated servers

**Big Data**: Data that exists in VERY LARGE VOLUMES and many different VARIETIES (types) that needs processing at high speed (e.g. mobile sensors, web clicks)
- **3 V's**: Volume, variety, velocity, are what make it big

Characteristics of Big Data:
- **Schema on Read**:  data model determined LATER, capture and store now and worry a/b that later. 
    - RDBS are Schema on Write, requires a pre-existent model
- **Data Lake**: Large integrated repo for internal/external data not following a predefined schema; captures ALL, dive anywhere, flexible

Approach outline:
- Traditional (SoW): Gather requirements ; formally model ; schema ; use database based on schema
- Big Data (SoR): Collect with local structures (XML, JSON) ; store in data lake ; analyse lake for meaningful structure; structure in analysis process

**NoSQL Database**: 
- NOT relational OR SQL
- Runs well on distributed servers
- Most are open-source
- Built for modern web
- Schema-less (implicit is possible)
- Supports SoR
- NOT ACID compliant
- EVENTUALLY consistent

Goals of NoSQL:
- Improve programmer productivity (OR mismatch)
- Handle larger data volumes and thoroughput (big data)

Types of NoSQL:
- Graph; key-value; document; column-family
- Key-value stores (K-V stores):
    - Key = PRIMARY Key
    - Value = Anything (#, array, json, img) ; up to appxn, no meaning otherwise
    - Operations are Put, Get and Update
    - Uses: If you need 'everything' in one place
        - Storing web session info (store in a single step; fast, all in one object)
        - User Profiles
        - Shopping Cart Data
    - Don't Use:
        - Relationships b/ data
        - Multi-operation transactions (e.g. involving many K)
        - Query by Data, that req. viewing data to get keys
        - Operations by Key Sets (can't; operations limited to ONE key at a time)
- Document: SOMEWHAT CONFUSING
    - *Document*: Self-describing pieces of data
        - E.g. hierarchical trees, nested maps, collections, scalars, XML, JSON
    - Documents should be 'similar' (schema can differ though)
    - Basically, it's K-V but the document (the value) is detailed and strutured to allow element manipulation
    - E.g. in Mongo, JSON documents are in a collection (table), with a unique id (in that collection); these collections form a dbs (schema)
    - `db._collection_.find({type: 'snacks', age: {$gt:18}}).sort({age:1})` - There's also `db._collxn_.insert()` and `.update({criteria}, {$set: {qty:10}})` (which is a modifier), and `upsert:true` will make a new doco if nothing matches
- Column families:
    - Columns rather than rows stored together on disk as 'families' (often related somehow)
    - FASTER ANALYSIS (less is fetched) (like auto vertical partitioning)
    - SUITABLE FOR LARGE DATA/QUERY THOROUGHPUT
    - Can see as sparse tables OR multiD. (nested) maps
    - Data distribution is via row key (as normal)
    - E.g. Cassandra, uses CQL
- Graph:
    - Stores entities and their relationships which could be modelled in a graph i.e. node-and-arc network (e.g. friendship graphs)
    - Deduce knowledge from the graph
    - SUPER FAST to answer 'extended friends' compared to RDBMS (for large requests)
    - Types of graphs:
        - **Single-relational**: Edges are homogenous (same meaning, associative)
        - **Multi-relational (property)**: Edges are typed/labelled (directional and can differ), e.g. friend vs colleague, vertices and edges maintain a set of K-V pairs
            - Use when non-graphical data (e.g. properties) important, e.g. name or weight of edge
    - Different types of relationships b/t nodes is ok, limitless # and kind
    - All relationships have a TYPE, START NODE, END NODE, and PROPERTIES

**Aggregate-oriented DBS**: Includes K-V, document, column-family.
- ENTIRE aggregate of data is stored together (no transactions)
- Efficient storage on clustered/distributed databases
- BUT hard to analysis a/c subfields (e.g. summing over products, a subfield of order)

## Further NoSQL

**CAP Theorem**: AKA Brewer's Theorem, says only two of the three below are possible:
- **Consistency**: Readers post-update in a Dist. system (with replication) see the same data.
- **Availability**: Working nodes (servers) can read and write data.
- **Partition Tolerance**: System can operate even if two sets of servers become isolated (e.g. a partition of the entire server network occurs)

**Fowler's Version of CAP Theorem**: Given a distributed dbs, when a partition occurs, one must choose b/t consistency and availability.

**ACID vs BASE**:
- BA = Basically available - the system guarantees availability but can be inconsistent or changing.
- S = Soft state - The state COULD change over time even during times without input (due to 'eventual consistency')
- E = 'Eventual Consistency' - The system will EVENTUALLY become consistent once it stops receiving input. Then the data will propagate where it needs to. The system cannot do this until all inputs are processed/silent, and it cannot check consistency of every transaction.
- This is a model of availability OVER consistency, whereas ACID is more concerned with consistency.

# Questions
- Clarify the elements of the tables on slide 20; what is data, information, metadata?
- your definitio of key, superkey, candidate key DIFFER to the internet.
    - can a key be a set of fields?
    - (other stuff)
- Is it necessary to assert 'NOT NULL' for what is declared as a key in SQL?
- Meaning of `ON DELETE RESTRICT ON DELETE CASCADE`:
    - Former: If the REFERENCE is deleted, it fails
    - Latter: If the REFERENCE is deleted, it updates the current table
    - `UPDATE` refers to when the data is altered
- SUPPOSEDLY partial keys are underlined in crow's foot notation
- So in Lec 7 you do rho(ANS(renames)...) but lec 8 you do renaming prior to the cross-product. What's better? WHATEVER
- CARDINALITY(Cross-Product) = CARD(S1) x CARD(S2)... clarify the meaning
- Heap Scan vs Unclustered costs: wouldn't a heap scan also incur NTuples cost as well?
- CRUCIAL: Lec 11 Slide 17 Q3: CHECK ANSWER (i think no)

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

