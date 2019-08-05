# Notes for Database Systems 2019 Sem 2

### Author: Callum H

## Questions
- Clarify the elements of the tables on slide 20; what is data, information, metadata?

## Random

**Data Dictionary**:

**Data Model**: A collection of concepts for describing data

**Schema**: A descr. of a particular collection of data, using a given data model

#### EXTRA: Query Languages
- *Data definition language* (DDL): Defines and sets up a DB
- *Data manipulation language* (DML): Maintains and uses the DB 
- *Data control language* (DCL): Control access to DB

## Recent Notes

### Lecture 1

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

#### Benefits of DBMS (rel. to FPS)
- Data independence: Separation of DATA and PROGRAM/LOGIC with a *data repository* and *central management*
- Minimal data redundancy: Controlled by *normalisation* (to break down a relationship b/t two relations into a set of smaller relations)
- Improved data consistency: Single store, no disagreements, update problems, minmal storage space required
- Improved data sharing: Data shared, corporate resource, external users could access data, and multiple (simultaneous?) views possible
- Reduced program maintenance: Data structure can change WITHOUT application data changing
- Novel ad hoc data access 'without programming': Query languages e.g. SQL

### File Processing Systems

This is a collection of programs that store/manage *files* in computer. Each program has its own set of data files, often restructured to meet needs of new application.

#### Flaws (rel. to DBMS)

- Program-data dependence: Changing the file necessitates program change; program 'knows too much a/b low-level data structure'
- Duplication of data: Wasteful, loss of integrity (e.g. same name, different data)
- Limited data sharing: Data is tied to application, adhoc reports difficult
- Lengthy dev times: Application must do low-level data management due to dependence = SLOW
- Excessive program maintenance: "Up to 80% of dev time is for maintenance"

## Lecture 2: Database Development

### Database Development Cycle (in order)
- **Database Planning**:  How to do the project, how does the enterprise work
- **Systems Definition**: Scope and boundaries; interference with org. systems
- **Req. Definition & Analysis**: Collection & Analysis of req. for new DBMS
- **Design** (see below) & **Application Design** (Designs interface/application to use/process DB)
- **Implementation**: Realisation of DB/application physically.
- **Data Conversion and Loading**: Transfer data into DB, convert (non trivial)
- **Testing**: Running DB, catch errors in design/setup (physical/logical), improving performance/robustness/recoverability/adaptability
- **Operational Maintenance**: Process of monitoring/maintaining DBMS post-commission e.g. new requirements, changes

### Database Design
- Conceptual: Constructing a model of data to be used: indept of physical considerations (i.e. Data Models, ER Diagrams)
- Logical: Constructing relational model based on conceptual d.
- Physical: Actual creation, description of how the logical design will be implemented for a specific DBMS including
> Basic relations
> File org.
> Indexing
> Data types

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

#### Data Dictionary
Key fields:
- *key*: Type of key (primary, foreign, neither)
- *attribute*, *data type*
- *not null*: Required or optional
- *unique*: Is the field unique
- *Description*: Useful info on attribute to designer/devprs e.g. attr sizes, valid values etc.

#### Database Planning



## Database Modelling

## Entity-Relationship Modelling

## SQL

## Query Processing & Optimisation