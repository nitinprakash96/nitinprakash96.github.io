---
layout:     post
title:      Understanding Postgres Storage System
author:     Nitin Prakash
category:   Blog
tags:       Database
---

I've been trying to figure out the internals of Postgres storage
system just to understand what has kept it around for so long. In the
process, I stumbled upon [this
paper](https://dsf.berkeley.edu/papers/ERL-M87-06.pdf) which outlines
the architectural ideas involved in the early development of postgres.
Note that this paper was published in 1987, so the current version of
Postgres does not adhere completely to what the paper suggests. But
some of the ideas presented are remarkable regardless.

In a nutshell, the Postgres storage engine can be understood as a
collection of 3 design principles:
- Provide transaction management without the necessity of writing a
 large amount of specialized crash recovery code. Why? Because it's
 hard to write safe code this way and hard to debug consequently.
- Accomodate the historical state of the data base on a
write-once-read-many (WORM) optical disk (or other archival medium) in
addition to the current state on an ordinary magnetic disk. This was
the birth of __Vacuum cleaner__.
- Hardware is a resource. The storage system can take advantage
  ofexistence of non-volatile main memory in some reasonable quantity.

Before diving into the above principles in detail, lets see how the
transaction system works in postgres.

### Transactions

- Transactions can be thought of as unique identifiers for postgres
  operations.
- Each transaction used to be a 40 bit unsigned integer that are
  sequentially assigned starting at 1 and increments for each new
  transaction. These integers are called __Transaction identifier
  (XID)__. However, modern Postgres uses 32 bit unsigned integers.
- Simplifying further, these XIDs can be used to determine the order
  of transactions: Eg: If transaction A has an `XID = 100` and
  transaction B has an `XID = 101`, then transaction B occurred after
  transaction A.
- These XIDs were what Postgres used for crash recovery instead of
  Write Ahead Logs (WAL). But this is no longer true. Modern Postgres
  system makes use of WAL logs.
- XIDs are used for multi-version concurrency control (MVCC). When a
  row in a table is updated or deleted, a new version of the row is
  created with a new XID, while the old version remains in the table
  with its original XID. This allows multiple transactions to read and
  write to the same table at the same time without interfering with
  each other.

### Relational storage

Per row metadata:

| Field | Description |
---------------------- | -------------------------------------------: |
| OID   | a system-assigned unique record identifier |
| X<sub>min</sub> | transaction identifier of the interaction inserting the record |
| T<sub>min</sub> | commit time of X<sub>Min</sub> (whenthe record became valid) |
| C<sub>min</sub> | command identifier of the interaction inserting the record |
| X<sub>max</sub> | transaction identifier of the interaction deleting the record |
| T<sub>max</sub> | commit time of X<sub>max</sub> (when the record stopped being valid) |
| C<sub>max</sub> | command identifier of the interaction deleting the record |
| PTR  | a forward pointer |

<br>

The current row per metadata can be found
[here](https://www.postgresql.org/docs/14/storage-page-layout.html#STORAGE-TUPLE-LAYOUT)

1. `INSERT`: Fields OID, X<sub>min</sub> and C<sub>min</sub> were
   set. Rest of the fields were left blank.
2. `UPDATE`: 2 operations happen at this point.
    * X<sub>Max</sub> and C<sub>max</sub> are set to the identity of
the current interaction in the record being replaced to indicate that
it has now become invalid.
    * A new record is inserted into the data base with the proposed
replacement value for the data fields. OID is set to the OID of the
record being replaced, and X<sub>min</sub> and C<sub>min</sub> are set
to the identity of the current interaction. PTR of the new interation
now pointed to the older interaction.
3. `DELETE`: X<sub>max</sub> and C<sub>max</sub> were set. Tmin was
   also set to indicate the transaction has been committed.

In case of `UPDATE` transaction, only a few fields would differ as
OID, X<sub>min</sub> and C<sub>min</sub> etc was being
reused. Therefore, in order to optimize space complexity, following
steps were taken:

- Initial record is stored uncompressed (known as _Anchor point_).
- We find the difference of the record being updated to that of Anchor
  point and only the changes were stored.
- PTR is altered on the anchor point to point to the updated record
  (known as _Delta record_).
- Consecutive updates would generate a singly linked list of delta
  records where the head of the list is the anchor point.

> NOTE: It looks like the above above mechanism does not exist
anymore. It currently has __[Heap Only Tuples
(HOT)](https://github.com/postgres/postgres/blob/master/src/backend/access/heap/README.HOT)__. From
the README, long story short:
>
> HOT eliminates redundant index entries and allows the re-use of
space taken by DELETEd or obsoleted UPDATEd tuples without performing
a table-wide vacuum. It does this by allowing single-page vacuuming,
also called "defragmentation".

### Record access

Each page:
- there is a line table containing pointers to the starting byte of
each anchor point record on that page.
- a pointer to the next and the previous logical page

Hence, Postgres can scan a relation by following the forward linked
list. Although it can execute query plans backward as well, therefore,
needing a pointer to the previous page.

Secondary indexes can be constructed for any base relation, and each
index is maintained by an access method that provides procedures for
access to the index, such as get-record-by-key, insert-record, and
delete-record. The term `secondary index` is used vaguely in the
modern times. I recommend getting an idea from
[here](https://stackoverflow.com/a/51087864/6244324).

When a record is inserted - an anchor point is constructed for the
record along with index entries for each secondary index. Each index
record contains a key or a collection of keys along with a pointer to
an entry in the line table on the page where the indexed record
resides.

When an existing record is updated - a delta record is constructed and
chained onto the appropriate anchor record. If an indexed field has
been modified, an entry is added to the appropriate index containing
the new key(s) and a pointer to the anchor record.

An important thing to note for modern postgres system, _it does
secondary index maintenance for all secondary indexes unless no
indexed columns have changed_. So if there 3 secondary indexes and an
`UPDATE` operation changes a column used by 1 of them then maintenance
is done for all of them -- unless HOT is used.

There's a lot of other noticeable things around secondary indexes but
I won't go into depth. Feel free to read the paper.

### Vacuuming the disk

An asynchronous demon (called **vacuum cleaner**) was responsible for
sweeping records which are no longer valid to the archive. The syntax
to invoke this cleaner was:

`vacuum rel-name after T`

where T is time, eg: '30 days'

Conditions for vacuum cleaner to find valid candidates for archival:
- X<sub>max</sub> is non empty and is a committed transaction and <code>now - T<sub>max</sub> >= T</code>.
- X<sub>max</sub> is non empty and is an aborted transaction.
- X<sub>min</sub> is non empty and is an aborted transaction.

There were certain _status_ assigned to those records so that cleaner
can ensure no data is irrecoverably lost. In the first case, a record
wascopied to the archive unless **no-archive** status is set for this
relation. Whereas, in the second and third case, cleaner would simply
reclaim the space.

### Vacuum process

Vacuuming was done in three phases, namely:

1. Write an archive record and its associated index records
2. Write a new anchor point in the current database
3. Reclaim the space occupied by the old anchor point and its delta records

What postgres was doing at the time wasn't crash safe. But it managed
to handle it correctly. Several scenarios:

- If a crash occured while the vacuum cleaner is writing the
  historical record in phase 1, then the data still existed in the
  magnetic disk database and could be revacuumed some later time.

- If a crash occured after some index records was written, then it was
  possible for the same record to be accessed in a magnetic disk
  relation and in an archive relation.

In either case, the duplicate record consumed system resources but it
wasn't really convcerning because of postgres is a relational system
and removed duplicate records during processing.

### Vacuuming cost

The paper discusses two scenarios:

- A record is inserted, updated K times and then deleted. The whole
chain of records from insertion to deletion is _vacuumed at once_.
- The vacuum is run after K updates, and a new anchor record must be
inserted.

The above is accompanied by several constant parameters:
- There are Z secondary indexes for both the archive and magnetic disk
  relation
- No key changes are made during these K updates
- Anchor point and all its delta records reside on the same page

| | whole chain | K updates |
---- | -------------------- | --------------- |
| archive writes   | 1 + Z | 1 + Z |
| disk reads | 1 | 1 |
| disk writes | 1 + Z | 1 + Z |

<br>
The above table reflects the IO count for vacuuming. The cost PER
RECORD of the vacuum cleaner is inversely proportional to the length
of the chain. As long as an anchor point and several delta records are
vacuumed together, the cost claimed was marginal.

There are other things that paper discusses like the archival medium,
magnetic disk indexes etc but I'm refraining from writing those just
yet because I don't understand them completely.
