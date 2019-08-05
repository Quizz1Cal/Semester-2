# Notes for Database Systems 2019 Sem 2

### Author: Callum H

# Questions
- Clarify the elements of the tables on slide 20; what is data, information, metadata?

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
- Each entity has a key (underlined) to uniquely identify it

**Relationship**: Association b/t two or more entities. These are determined by the business rules
- *Can have their own attributes, sometimes called **descriptive attributes** (e.g. duration of relationship)*
- E.g. Fred works in Pharmacy Dept.

**Relationship Set**: Collection of relationships of same type.
- E.g. Employees work in depts.

**Weak Entity**: Identified uniquely ONLY by considering (primary key of) another entity (set) (called the owner). 
- Represented as **bold rectangle**
- Requires that:
    1. Two entity sets have a one-to-many relationship set (many weak ones)
    2. Weak entity set must have total participation in (what is called) the **identifying relationship**
- Weak entities have a partial key -> only uniquely identified when considering primary key of **owner entity**

### Chen's Notation for ER Modelling

Use *squares* for name of set, *ovals* for attribute, *underline* for key, *diamond* for relationship

A self-relating set has two lines b/t it and the relationship.

## Constraints

**Key constraints** determine the cardinality of relationship -> the no. of objects taking part in relationship set. An 'upper bound'
- Many-to-many (a line)
- One-to-many (a unidirectional arrow). Officially a **key constraint**
- One-to-one

**Participation Constraint** asks if all entities (of a given relationship) take part. A 'lower bound'
- **total** or **partial** participation
> **Bold a total participation line**

## n-ary Relationships

E.g. a relationship is ternary if three entity sets are related.

**WARNING**: Just because D sells K and K is bought by P doesn't mean D sells to P. Be careful with relationships.

## Attribute Types

**Multi-valued attributes**: Can have multiple values (from a finite set) of same type e.g. Employees have a work AND phone number, both captured under 'phone num'.
> Oval with a double-lined (=) border

**Composite attributes**: Have a hidden structure (elements can be of different type) e.g. "Address" consists of Postcode, Streetname, Streetnum.
> Ovals extending off an attribute oval

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

# Database Modelling

# Entity-Relationship Modelling

# SQL

# Query Processing & Optimisation