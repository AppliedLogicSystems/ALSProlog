ALS Prolog provides a module system to facilitate the creation and maintenance of
large programs. The main purpose of the module system is to partition procedures
into separate groups to avoid naming conflicts between those groups. The module
system provides controlled access to procedures within those groups.
The ALS module system only partitions procedures, not constants. This means that
the procedure foo/2 may have different meanings in different modules, but that
the constant bar is the same in every module.

##3.1 Declaring a Module