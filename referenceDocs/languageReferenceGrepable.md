accept.md
# [[BabyCobol](index.html)]{.ff .lang}: [[ACCEPT](accept.html)]{.ff .used}

a statement to read user input and store it in variables

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIzNDBweCIgaGVpZ2h0PSI4MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDQwIDIwIDM2IDIwIDQ0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiA0MCAyOCAzNiAyOCA0NCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgNDAgaCAyMCIgLz48dGV4dCB4PSI1NiIgeT0iNDQiPkFDQ0VQVDwvdGV4dD48cGF0aCBkPSJNIDExNiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMTM2IDQwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjE1NiIgeT0iNDQiPklkZW50aWZpZXI8L3RleHQ+PHBhdGggZD0iTSAyNTYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDI3NiA0MCB2IC0yMCBoIC0xNDAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjEzNiAzOSAxMzIgMzEgMTQwIDMxIj48L3BvbHlnb24+PHBhdGggZD0iTSAyNzYgNDAgaCAyMCIgLz48cG9seWdvbiBwb2ludHM9IjMwNCA0MCAyOTYgMzYgMjk2IDQ0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzMDQgNDAgMzEyIDM2IDMxMiA0NCI+PC9wb2x5Z29uPgo8L3N2Zz4=)

## Remarks

-   the input is expected to obey the picture clause definition of the data fields being read
-   Essentially, the `PICTURE` clause is a binding between the actual data and its displayed representation. For a software language engineer, it might be tempting to implement this binding as two mappings between consistent formats. The `ACCEPT` statement prevents this, since it demands a binding between an unreliable displayable representation (entered by the user) and the consistent internal representation that must be created from it, available at runtime.
-   In real compilers of legacy languages, the internal representation also must conform to legacy character encodings like [EBCDIC](https://en.wikipedia.org/wiki/EBCDIC) and legacy binary encodings like [BCD](https://en.wikipedia.org/wiki/Binary-coded_decimal).
-   If the input is erroneous, the end result in the target field must represent the fragment of entered data from the start until the error position.

## Origins

[[CLIST](clist.html)]{.ff .lang}   [[READ](clist.html "read user input and store it in variables")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[ACCEPT](cobol.html "accept user data as the new value of an identifier")]{.ff .used}\
[[FORTRAN](fortran.html)]{.ff .lang}   [[READ](fortran.html "accepts data from input cards, tape or drum")]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[PULL](rexx.html "syntactic sugar for PARSE UPPER PULL")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[DATA-INTO](rpg.html "parse a document into a variable")]{.ff .used}\

------------------------------------------------------------------------
add.md
# [[BabyCobol](index.html)]{.ff .lang}: [[ADD](add.html)]{.ff .used}

a statement to add two or more values together

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI2ODBweCIgaGVpZ2h0PSIxMDBweCI+CiAgICA8ZGVmcz4KICAgICAgICA8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgICAgICAgICBAbmFtZXNwYWNlICJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI7CiAgICAgICAgICAgIHN2ZyB7YmFja2dyb3VuZC1jb2xvcjogd2hpdGU7fQogICAgICAgICAgICBwYXRoIHtmaWxsOiBub25lOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgcG9seWdvbiB7ZmlsbDogYmxhY2s7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICB0ZXh0IHtmb250LXNpemU6MTZweDtmaWxsOmJsYWNrO2ZvbnQtd2VpZ2h0OmJvbGQ7Zm9udC1mYW1pbHk6bW9ub3NwYWNlO30KICAgICAgICAgICAgdGV4dC5pIHtmb250LXN0eWxlOml0YWxpYzt9CiAgICAgICAgPC9zdHlsZT4KICAgIDwvZGVmcz4KPHBvbHlnb24gcG9pbnRzPSIyOCA0MCAyMCAzNiAyMCA0NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMzYgNDAgMjggMzYgMjggNDQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM2IDQwIGggMjAiIC8+PHRleHQgeD0iNTYiIHk9IjQ0Ij7CoEFERDwvdGV4dD48cGF0aCBkPSJNIDk2IDQwIGggMjAiIC8+PHBhdGggZD0iTSAxMTYgNDAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMTM2IiB5PSI0NCI+QXRvbWljPC90ZXh0PjxwYXRoIGQ9Ik0gMTk2IDQwIGggMjAiIC8+PHBhdGggZD0iTSAyMTYgNDAgdiAtMjAgaCAtMTAwIHYgMjAiIC8+PHBvbHlnb24gcG9pbnRzPSIxMTYgMzkgMTEyIDMxIDEyMCAzMSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMjE2IDQwIGggMjAiIC8+PHRleHQgeD0iMjM2IiB5PSI0NCI+VE88L3RleHQ+PHBhdGggZD0iTSAyNTYgNDAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMjc2IiB5PSI0NCI+QXRvbWljPC90ZXh0PjxwYXRoIGQ9Ik0gMzM2IDQwIGggMjAiIC8+PHBhdGggZD0iTSAzNTYgNDAgaCAyODAiIC8+PHBvbHlnb24gcG9pbnRzPSI2NDQgNDAgNjM2IDM2IDYzNiA0NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iNjQ0IDQwIDY1MiAzNiA2NTIgNDQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM1NiA0MCB2IDQwIGggMjAiIC8+PHBhdGggZD0iTSAzNzYgODAgaCAyMCIgLz48dGV4dCB4PSIzOTYiIHk9Ijg0Ij5HSVZJTkc8L3RleHQ+PHBhdGggZD0iTSA0NTYgODAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNDc2IiB5PSI4NCI+SWRlbnRpZmllcjwvdGV4dD48cGF0aCBkPSJNIDU3NiA4MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNTk2IDgwIHYgLTIwIGggLTIyMCB2IDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMzc2IDc5IDM3MiA3MSAzODAgNzEiPjwvcG9seWdvbj48cGF0aCBkPSJNIDU5NiA4MCBoIDIwIHYgLTQwIiAvPjxwb2x5Z29uIHBvaW50cz0iNjE2IDQxIDYxMiA0OSA2MjAgNDkiPjwvcG9seWdvbj4KPC9zdmc+)

## Remarks

-   adds each of the first arguments to the second argument and overwrites the old value of the second argument, unless the third argument specifies a different storage place
-   so, for example, `ADD 1 2 3 TO X` will increase the value of `X` by 6
-   all three arguments obey the rules of sufficient qualification
-   the first argument can be a literal
-   if the second argument is a literal, the third argument is mandatory
-   any of the three arguments can be an identifier defined with a numeric picture clause (free from `A` and `X`)
-   if the first argument denotes a composite field, then the second argument should also denote a composite field, and the addition works correspondingly: for instance, if the first argument is a field `X` with inner numeric fields `A`, `B` and `C`, and the second argument is a field `Y` with inner numeric fields `A`, `C` and `D`, then `A OF Y` is increased by `A OF X` and `C OF Y` is increased by `C OF X`, and all other fields remain unchanged
-   if all three arguments are composite fields, combine the description above with a `MOVE` to the target location

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[ADD](cobol.html "sum two or more numeric operands")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[ADD](rpg.html "add two numbers together")]{.ff .used} [[Z-ADD](rpg.html "add a number to a field of zeroes")]{.ff .used}\

------------------------------------------------------------------------
alter.md
# [[BabyCobol](index.html)]{.ff .lang}: [[ALTER](alter.html)]{.ff .used}

a statement to change the target of an existing GO TO statement

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI2NDBweCIgaGVpZ2h0PSI2MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDMwIDIwIDI2IDIwIDM0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiAzMCAyOCAyNiAyOCAzNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgMzAgaCAyMCIgLz48dGV4dCB4PSI1NiIgeT0iMzQiPsKgQUxURVI8L3RleHQ+PHBhdGggZD0iTSAxMTYgMzAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMTM2IiB5PSIzNCI+wqBQcm9jZWR1cmVOYW1lPC90ZXh0PjxwYXRoIGQ9Ik0gMjc2IDMwIGggMjAiIC8+PHRleHQgeD0iMjk2IiB5PSIzNCI+VE88L3RleHQ+PHRleHQgeD0iMzE2IiB5PSIzNCI+wqBQUk9DRUVEPC90ZXh0Pjx0ZXh0IHg9IjM5NiIgeT0iMzQiPlRPPC90ZXh0PjxwYXRoIGQ9Ik0gNDE2IDMwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjQzNiIgeT0iMzQiPsKgUHJvY2VkdXJlTmFtZTwvdGV4dD48cGF0aCBkPSJNIDU3NiAzMCBoIDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iNjA0IDMwIDU5NiAyNiA1OTYgMzQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjYwNCAzMCA2MTIgMjYgNjEyIDM0Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Remarks

-   Essentially, the dreaded `GO TO` that has been considered harmful since 1968, is only hard on the end developer, but not on a software language engineer. Its implementation in a compiler is almost trivial and relies on two components: one collecting all possible targets and one fetching a target from that collection and generating an unconditional jump/branch there. `ALTER` is a complication because it forces the software language engineer to move all this machinery from compile time to runtime.
-   Unlike FORTRAN\'s `ASSIGN` statement which can be at least partially optimised because the set of possible values is known apriori for each `GO TO`, BabyCobol follows COBOL and does not restrict alterations in any way.

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[ALTER](cobol.html "change the transfer point of an existing GO TO statement")]{.ff .used}\
[[FORTRAN](fortran.html)]{.ff .lang}   [[ASSIGN](fortran.html "change the target of an assigned GO TO statement")]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[ADDRESS](rexx.html "change the destination of commands")]{.ff .used}\

------------------------------------------------------------------------
appbuilder.md
# [[BabyCobol](index.html)]{.ff .lang}: The [[AppBuilder](appbuilder.html)]{.ff .lang} Origins

------------------------------------------------------------------------

## Features:

[[CASEOF](evaluate.html "branch to multiple locations based on several conditions")]{.ff .used} [[CLEAR](move.html "return a field or a view to its freshly initialised state")]{.ff .used} [CONVERSE]{.ff title="engage in user interaction"} [[DCL](datadivision.html "a declaration block for local fields and views")]{.ff .used} [[DO](loop.html "a multi-purpose loop statement with a number of detachable clauses")]{.ff .used} [HANDLER]{.ff title="assign a handler to an event"} [[IF](if.html "branch conditionally within a program")]{.ff .used} [[MAP](move.html "name-based deep structural asssignment")]{.ff .used} [OVERLAY]{.ff title="serialise one data structure bytewise over another structure"} [[PERFORM](perform.html "call a local procedure")]{.ff .used} [PROC RETURN]{.ff title="terminate the execution of a local procedure"} [[RETURN](stop.html "terminate the execution of a rule")]{.ff .used} [[SET](move.html "an alternative assignment statement")]{.ff .used} [SQL]{.ff title="execute an embedded SQL query"} [[USE](call.html "call a rule or a component from into another rule")]{.ff .used}

## Sources:

-   Castle / CSL --- Specification, Version 1.97.0.0, 20 May 2020, Raincode

------------------------------------------------------------------------
call.md
# [[BabyCobol](index.html)]{.ff .lang}: [[CALL](call.html)]{.ff .used}

an instruction to execute another program

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI3MDBweCIgaGVpZ2h0PSIxNDBweCI+CiAgICA8ZGVmcz4KICAgICAgICA8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgICAgICAgICBAbmFtZXNwYWNlICJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI7CiAgICAgICAgICAgIHN2ZyB7YmFja2dyb3VuZC1jb2xvcjogd2hpdGU7fQogICAgICAgICAgICBwYXRoIHtmaWxsOiBub25lOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgcG9seWdvbiB7ZmlsbDogYmxhY2s7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICB0ZXh0IHtmb250LXNpemU6MTZweDtmaWxsOmJsYWNrO2ZvbnQtd2VpZ2h0OmJvbGQ7Zm9udC1mYW1pbHk6bW9ub3NwYWNlO30KICAgICAgICAgICAgdGV4dC5pIHtmb250LXN0eWxlOml0YWxpYzt9CiAgICAgICAgPC9zdHlsZT4KICAgIDwvZGVmcz4KPHBvbHlnb24gcG9pbnRzPSIyOCA0MCAyMCAzNiAyMCA0NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMzYgNDAgMjggMzYgMjggNDQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM2IDQwIGggMjAiIC8+PHRleHQgeD0iNTYiIHk9IjQ0Ij5DQUxMPC90ZXh0PjxwYXRoIGQ9Ik0gOTYgNDAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMTE2IiB5PSI0NCI+RmlsZU5hbWU8L3RleHQ+PHBhdGggZD0iTSAxOTYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDIxNiA0MCBoIDQ2MCIgLz48cG9seWdvbiBwb2ludHM9IjY4NCA0MCA2NzYgMzYgNjc2IDQ0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSI2ODQgNDAgNjkyIDM2IDY5MiA0NCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMjE2IDQwIHYgNDAgaCAyMCIgLz48dGV4dCB4PSIyMzYiIHk9Ijg0Ij7CoFVTSU5HPC90ZXh0PjxwYXRoIGQ9Ik0gMjk2IDgwIGggNDAiIC8+PHBhdGggZD0iTSAzMzYgODAgaCAyMCIgLz48dGV4dCB4PSIzNTYiIHk9Ijg0Ij5CWSBSRUZFUkVOQ0U8L3RleHQ+PHBhdGggZD0iTSA0NzYgODAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNDk2IiB5PSI4NCI+SWRlbnRpZmllcjwvdGV4dD48cGF0aCBkPSJNIDU5NiA4MCBoIDQwIiAvPjxwYXRoIGQ9Ik0gNjM2IDgwIHYgLTIwIGggLTMyMCB2IDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMzE2IDc5IDMxMiA3MSAzMjAgNzEiPjwvcG9seWdvbj48cGF0aCBkPSJNIDYzNiA4MCBoIDIwIHYgLTQwIiAvPjxwb2x5Z29uIHBvaW50cz0iNjU2IDQxIDY1MiA0OSA2NjAgNDkiPjwvcG9seWdvbj48cGF0aCBkPSJNIDMzNiA4MCB2IDIwIGggMjAiIC8+PHRleHQgeD0iMzU2IiB5PSIxMDQiPkJZIENPTlRFTlQ8L3RleHQ+PHBhdGggZD0iTSA0NTYgMTAwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjQ3NiIgeT0iMTA0Ij5BdG9taWM8L3RleHQ+PHBhdGggZD0iTSA1MzYgMTAwIGggODAiIC8+PHBhdGggZD0iTSAzMzYgODAgdiA0MCBoIDIwIiAvPjx0ZXh0IHg9IjM1NiIgeT0iMTI0Ij5CWSBWQUxVRTwvdGV4dD48cGF0aCBkPSJNIDQzNiAxMjAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNDU2IiB5PSIxMjQiPkF0b21pYzwvdGV4dD48cGF0aCBkPSJNIDUxNiAxMjAgaCA4MCIgLz48cGF0aCBkPSJNIDU5NiAxMjAgaCAyMCB2IC00MCIgLz48cG9seWdvbiBwb2ludHM9IjYxNiA4MSA2MTIgODkgNjIwIDg5Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Alternative format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI3MjBweCIgaGVpZ2h0PSIzMjBweCI+CiAgICA8ZGVmcz4KICAgICAgICA8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgICAgICAgICBAbmFtZXNwYWNlICJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI7CiAgICAgICAgICAgIHN2ZyB7YmFja2dyb3VuZC1jb2xvcjogd2hpdGU7fQogICAgICAgICAgICBwYXRoIHtmaWxsOiBub25lOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgcG9seWdvbiB7ZmlsbDogYmxhY2s7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICB0ZXh0IHtmb250LXNpemU6MTZweDtmaWxsOmJsYWNrO2ZvbnQtd2VpZ2h0OmJvbGQ7Zm9udC1mYW1pbHk6bW9ub3NwYWNlO30KICAgICAgICAgICAgdGV4dC5pIHtmb250LXN0eWxlOml0YWxpYzt9CiAgICAgICAgPC9zdHlsZT4KICAgIDwvZGVmcz4KPHBvbHlnb24gcG9pbnRzPSIyOCA0MCAyMCAzNiAyMCA0NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMzYgNDAgMjggMzYgMjggNDQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM2IDQwIGggNDAiIC8+PHRleHQgeD0iNzYiIHk9IjQ0Ij5DQUxMPC90ZXh0PjxwYXRoIGQ9Ik0gMTE2IDQwIGggNDAiIC8+PHBhdGggZD0iTSAxNTYgNDAgdiAyMCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIxNzYiIHk9IjY0Ij5GdW5jdGlvbk5hbWU8L3RleHQ+PHBhdGggZD0iTSAyOTYgNjAgaCA0MCIgLz48dGV4dCB4PSIzMzYiIHk9IjY0Ij5PRjwvdGV4dD48cGF0aCBkPSJNIDM1NiA2MCBoIDIwIHYgLTIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMzc2IDQxIDM3MiA0OSAzODAgNDkiPjwvcG9seWdvbj48cGF0aCBkPSJNIDE1NiA0MCBoIDI4MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNDM2IiB5PSI0NCI+wqBQcm9ncmFtTmFtZTwvdGV4dD48cGF0aCBkPSJNIDU1NiA0MCBoIDEyMCIgLz48cGF0aCBkPSJNIDIwIDEwMCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNDAgMTAwIGggNjQwIiAvPjxwYXRoIGQ9Ik0gNDAgMTAwIHYgNDAgaCAyMCIgLz48dGV4dCB4PSI2MCIgeT0iMTQ0Ij7CoFVTSU5HPC90ZXh0PjxwYXRoIGQ9Ik0gMTIwIDE0MCBoIDQwIiAvPjxwYXRoIGQ9Ik0gMTYwIDE0MCBoIDE4MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMzQwIiB5PSIxNDQiPkF0b21pYzwvdGV4dD48cGF0aCBkPSJNIDQwMCAxNDAgaCAyMCIgLz48cGF0aCBkPSJNIDQyMCAxNDAgaCAxODAiIC8+PHBhdGggZD0iTSA2MDAgMTQwIHYgLTIwIGggLTQ2MCB2IDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMTQwIDEzOSAxMzYgMTMxIDE0NCAxMzEiPjwvcG9seWdvbj48cGF0aCBkPSJNIDYwMCAxNDAgaCAyMCB2IC00MCIgLz48cG9seWdvbiBwb2ludHM9IjYyMCAxMDEgNjE2IDEwOSA2MjQgMTA5Ij48L3BvbHlnb24+PHBhdGggZD0iTSAxNjAgMTQwIHYgMjAgaCAyMCIgLz48dGV4dCB4PSIxODAiIHk9IjE2NCI+QlkgUkVGRVJFTkNFPC90ZXh0PjxwYXRoIGQ9Ik0gMzAwIDE2MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMTYwIDE0MCB2IDQwIGggMjAiIC8+PHRleHQgeD0iMTgwIiB5PSIxODQiPkJZIENPTlRFTlQ8L3RleHQ+PHBhdGggZD0iTSAyODAgMTgwIGggNDAiIC8+PHBhdGggZD0iTSAxNjAgMTQwIHYgNjAgaCAyMCIgLz48dGV4dCB4PSIxODAiIHk9IjIwNCI+QlkgVkFMVUU8L3RleHQ+PHBhdGggZD0iTSAyNjAgMjAwIGggNDAiIC8+PHBhdGggZD0iTSAzMDAgMjAwIGggMjAgdiAtNjAiIC8+PHBvbHlnb24gcG9pbnRzPSIzMjAgMTQxIDMxNiAxNDkgMzI0IDE0OSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNDIwIDE0MCB2IDIwIGggMjAiIC8+PHRleHQgeD0iNDQwIiB5PSIxNjQiPkFTIFBSSU1JVElWRTwvdGV4dD48cGF0aCBkPSJNIDU2MCAxNjAgaCAyMCIgLz48cGF0aCBkPSJNIDQyMCAxNDAgdiA0MCBoIDIwIiAvPjx0ZXh0IHg9IjQ0MCIgeT0iMTg0Ij7CoEFTIFNUUlVDVDwvdGV4dD48cGF0aCBkPSJNIDU0MCAxODAgaCAyMCIgLz48cGF0aCBkPSJNIDU2MCAxODAgaCAyMCB2IC00MCIgLz48cG9seWdvbiBwb2ludHM9IjU4MCAxNDEgNTc2IDE0OSA1ODQgMTQ5Ij48L3BvbHlnb24+PHBhdGggZD0iTSAyMCAyMjAgaCAyMCIgLz48cGF0aCBkPSJNIDQwIDIyMCBoIDY0MCIgLz48cG9seWdvbiBwb2ludHM9IjY4OCAyMjAgNjgwIDIxNiA2ODAgMjI0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSI2ODggMjIwIDY5NiAyMTYgNjk2IDIyNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNDAgMjIwIHYgMjAgaCAyMCIgLz48dGV4dCB4PSI2MCIgeT0iMjQ0Ij7CoFJFVFVSTklORzwvdGV4dD48cGF0aCBkPSJNIDE2MCAyNDAgaCAyMCIgLz48cGF0aCBkPSJNIDE4MCAyNDAgaCAxODAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjM2MCIgeT0iMjQ0Ij5JZGVudGlmaWVyPC90ZXh0PjxwYXRoIGQ9Ik0gNDYwIDI0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNDgwIDI0MCBoIDE2MCIgLz48cGF0aCBkPSJNIDY0MCAyNDAgaCAyMCB2IC0yMCIgLz48cG9seWdvbiBwb2ludHM9IjY2MCAyMjEgNjU2IDIyOSA2NjQgMjI5Ij48L3BvbHlnb24+PHBhdGggZD0iTSAxODAgMjQwIHYgMjAgaCAyMCIgLz48dGV4dCB4PSIyMDAiIHk9IjI2NCI+QlkgUkVGRVJFTkNFPC90ZXh0PjxwYXRoIGQ9Ik0gMzIwIDI2MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMTgwIDI0MCB2IDQwIGggMjAiIC8+PHRleHQgeD0iMjAwIiB5PSIyODQiPkJZIENPTlRFTlQ8L3RleHQ+PHBhdGggZD0iTSAzMDAgMjgwIGggNDAiIC8+PHBhdGggZD0iTSAxODAgMjQwIHYgNjAgaCAyMCIgLz48dGV4dCB4PSIyMDAiIHk9IjMwNCI+QlkgVkFMVUU8L3RleHQ+PHBhdGggZD0iTSAyODAgMzAwIGggNDAiIC8+PHBhdGggZD0iTSAzMjAgMzAwIGggMjAgdiAtNjAiIC8+PHBvbHlnb24gcG9pbnRzPSIzNDAgMjQxIDMzNiAyNDkgMzQ0IDI0OSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNDgwIDI0MCB2IDIwIGggMjAiIC8+PHRleHQgeD0iNTAwIiB5PSIyNjQiPkFTIFBSSU1JVElWRTwvdGV4dD48cGF0aCBkPSJNIDYyMCAyNjAgaCAyMCIgLz48cGF0aCBkPSJNIDQ4MCAyNDAgdiA0MCBoIDIwIiAvPjx0ZXh0IHg9IjUwMCIgeT0iMjg0Ij7CoEFTIFNUUlVDVDwvdGV4dD48cGF0aCBkPSJNIDYwMCAyODAgaCAyMCIgLz48cGF0aCBkPSJNIDYyMCAyODAgaCAyMCB2IC00MCIgLz48cG9seWdvbiBwb2ludHM9IjY0MCAyNDEgNjM2IDI0OSA2NDQgMjQ5Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Remarks

-   Executes another program denoted with *`FileName`*, from the start of its [`PROCEDURE DIVISION`](proceduredivision.html) until its end or until its [`STOP`](stop.html), whatever comes first, and the continues to execute the current program from the next statement.
-   If the call uses arguments, they should match exactly the arguments of the [`PROCEDURE DIVISION`](proceduredivision.html) of the program being called.
-   `BY REFERENCE` arguments can be accessed and modified by the called program.
-   `BY CONTENT` arguments be accessed by the called program, but cannot be modified.
-   `BY VALUE` arguments can be accessed and modified by the called program, but the modification do not propagate back to the caller.
-   The basic format (first diagram above) is enough to implement simple BabyCobol-to-BabyCobol calls; the alternative format (the second diagram) can be used for complex scenarios like BabyCobol-to-C calls from [the GPCE'23 paper](http://grammarware.net/writes/#Crossover2023#Crossover2023).

## Origins

[[AppBuilder](appbuilder.html)]{.ff .lang}   [[USE](appbuilder.html "call a rule or a component from into another rule")]{.ff .used}\
[[CLIST](clist.html)]{.ff .lang}   [[EXEC](clist.html "call another program")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[CALL](cobol.html "transfer control from one program to another")]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[CALL](rexx.html "call a built-in or external function")]{.ff .used}\

------------------------------------------------------------------------
clist.md
# [[BabyCobol](index.html)]{.ff .lang}: The [[CLIST](clist.html)]{.ff .lang} Origins

------------------------------------------------------------------------

## Features:

[[ATTN](signal.html "define a handler for an attention interrupt")]{.ff .used} [CLOSFILE]{.ff title="close a previously opened QSAM file"} [CONTROL]{.ff title="define processing options"} [DATA]{.ff title="statements for TSO/E to execute"} [DATA PROMPT]{.ff title="respond to prompts by TSO/E commands"} [[DO](loop.html "execute a sequence of commands repeatedly")]{.ff .used} [[END](https://slebok.github.io/baby/end.html "end DO, SELECT or a procedure")]{.ff .used} [[ERROR](signal.html "check for non-zero return codes of other commands")]{.ff .used} [[EXEC](call.html "call another program")]{.ff .used} [[EXIT](stop.html "terminate the program")]{.ff .used} [GETFILE]{.ff title="read a record from a QSAM file"} [GLOBAL]{.ff title="share values between nested programs"} [[GOTO](goto.html "branch unconditionally within a program")]{.ff .used} [[IF](if.html "branch conditionally within a program")]{.ff .used} [LISTDSI]{.ff title="get information about a dataset"} [NGLOBAL]{.ff title="share values between procedures within a program"} [OPENFILE]{.ff title="open a QSAM file for input and output"} [[PROC](https://slebok.github.io/baby/section.html "define a procedure")]{.ff .used} [PUTFILE]{.ff title="write a record to a QSAM file"} [[READ](accept.html "read user input and store it in variables")]{.ff .used} [READDVAL]{.ff title="read a &SYSDVAL control variable"} [RETURN]{.ff title="return from an error handler or a procedure"} [[SELECT](evaluate.html "branch to multiple locations based on several conditions")]{.ff .used} [[SET](move.html "assign a value to a variable")]{.ff .used} [[SYSCALL](perform.html "call a local procedure")]{.ff .used} [SYSREF]{.ff title="define which caller's data can a procedure access"} [TERMIN]{.ff title="temporary pass control to the terminal user"} [TERMING]{.ff title="temporary pass control to the terminal user from a nested program"} [[WRITE](display.html "display text on the terminal")]{.ff .used} [[WRITENR](display.html "display text on the terminal without advancing a cursor to the next line")]{.ff .used}

## Sources:

-   IBM SA32-0978-00, z/OS TSO/E CLISTs Version 2 Release 1, 1988--2013

------------------------------------------------------------------------
cobol.md
# [[BabyCobol](index.html)]{.ff .lang}: The [[COBOL](cobol.html)]{.ff .lang} Origins

------------------------------------------------------------------------

## Features:

[\*CBL]{.ff title="a directive to select what is being printed during compilation"} [\*CONTROL]{.ff title="a directive to select what is being printed during compilation"} [[ACCEPT](accept.html "accept user data as the new value of an identifier")]{.ff .used} [[ADD](add.html "sum two or more numeric operands")]{.ff .used} [[ALTER](alter.html "change the transfer point of an existing GO TO statement")]{.ff .used} [BASIS]{.ff title="a directive to provide a program to compile"} [[CALL](call.html "transfer control from one program to another")]{.ff .used} [CANCEL]{.ff title="clear the state of a called subprogram"} [CBL]{.ff title="a directive to specify compilation options"} [CLOSE]{.ff title="revoke access to a file or volume"} [COMPUTE]{.ff title="assign a variable with a result of a computation of an expression"} [[CONTINUE](nextsentence.html "do nothing")]{.ff .used} [[COPY](copy.html "a directive to include another file")]{.ff .used} [[DATA DIVISION](datadivision.html "the top level program unit with variable definitions")]{.ff .used} [DELETE]{.ff title="remove a record from a file or statements from the program under compilation"} [[DISPLAY](display.html "print contents of data fields on the display")]{.ff .used} [[DIVIDE](divide.html "divide one numeric operand into others")]{.ff .used} [[DIVISION](division.html "the top level unit of any program")]{.ff .used} [EJECT]{.ff title="a directive to use a new page when printing the listing"} [ENTER]{.ff title="a directive to support compilation of programs written in other languages than COBOL"} [ENTRY]{.ff title="specify a different entry point for a program"} [ENVIRONMENT DIVISION]{.ff title="the top level program unit specifying input and output files"} [[EVALUATE](evaluate.html "shorthand notation for a series of nested IF statements")]{.ff .used} [[EXIT](nextsentence.html "do nothing")]{.ff .used} [EXIT METHOD]{.ff title="terminate the execution of a method"} [[EXIT PROGRAM](stop.html "terminate the execution of a subprogram")]{.ff .used} [[GO TO](goto.html "branch unconditionally to another section or paragraph")]{.ff .used} [[GOBACK](stop.html "act like an EXIT PROGRAM or like a STOP RUN")]{.ff .used} [[IDENTIFICATION DIVISION](identificationdivision.html "the top level program unit with the program ID and other metadata")]{.ff .used} [[IF](if.html "branch conditionally within a program")]{.ff .used} [INITIALIZE]{.ff title="assign default values to data fields"} [INSERT]{.ff title="a directive to add source code lines to the basis program"} [INSPECT]{.ff title="count or convert characters in a string"} [INVOKE]{.ff title="create object instances and call methods"} [MERGE]{.ff title="combine two or more files"} [[MOVE](move.html "transfer data from one storage area to another")]{.ff .used} [[MULTIPLY](multiply.html "multiply numeric values")]{.ff .used} [[NEXT SENTENCE](nextsentence.html "a special clause of the IF statement to transfer control to the statement after the next dot")]{.ff .used} [OPEN]{.ff title="initiates file processing"} [[PERFORM (in-line)](loop.html "execute a list of statements repeatedly")]{.ff .used} [[PERFORM (out-of-line)](perform.html "call a local procedure")]{.ff .used} [[PROCEDURE DIVISION](proceduredivision.html "the top level program unit with executable code")]{.ff .used} [PROCESS]{.ff title="a directive to specify compilation options"} [READ]{.ff title="read the next record from an opened file"} [READY TRACE]{.ff title="a directive to activate the debugging mode"} [RELEASE]{.ff title="prepare data for sorting"} [[REPLACE](copy.html "a directive to switch ongoing replacements on or off")]{.ff .used} [RESET TRACE]{.ff title="a directive to reset the debugging mode"} [RETURN]{.ff title="transfer sorted or merged records to the output"} [REWRITE]{.ff title="replace an existing record in a file"} [SEARCH]{.ff title="search a table for an element satisfying the given condition"} [SERVICE]{.ff title="directives inserted by CICS"} [SET]{.ff title="perform specific assignments"} [SKIP]{.ff title="a directive to add blank source code lines to the listing"} [SORT]{.ff title="sort records from one or more files"} [START]{.ff title="adjust the cursor position in a file"} [[STOP RUN](stop.html "terminate the program")]{.ff .used} [[STRING](display.html "format and concatenate strings")]{.ff .used} [[SUBTRACT](subtract.html "subtract one or more numeric values from another value")]{.ff .used} [TITLE]{.ff title="a directive to specify a page title to print the listing"} [UNSTRING]{.ff title="split data over several data fields"} [[USE](signal.html "a directive to activate debugging or exception handling")]{.ff .used} [WRITE]{.ff title="add a record to a file or another output"} [XML]{.ff title="convert data to or from XML"}

## Sources:

-   IBM SC23-8528-01, Enterprise COBOL for z/OS Language Reference Version 4 Release 2, 1991--2009

------------------------------------------------------------------------
copy.md
# [[BabyCobol](index.html)]{.ff .lang}: [[COPY](copy.html)]{.ff .used}

an instruction to insert contents from a different file

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI3NDBweCIgaGVpZ2h0PSIxMDBweCI+CiAgICA8ZGVmcz4KICAgICAgICA8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgICAgICAgICBAbmFtZXNwYWNlICJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI7CiAgICAgICAgICAgIHN2ZyB7YmFja2dyb3VuZC1jb2xvcjogd2hpdGU7fQogICAgICAgICAgICBwYXRoIHtmaWxsOiBub25lOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgcG9seWdvbiB7ZmlsbDogYmxhY2s7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICB0ZXh0IHtmb250LXNpemU6MTZweDtmaWxsOmJsYWNrO2ZvbnQtd2VpZ2h0OmJvbGQ7Zm9udC1mYW1pbHk6bW9ub3NwYWNlO30KICAgICAgICAgICAgdGV4dC5pIHtmb250LXN0eWxlOml0YWxpYzt9CiAgICAgICAgPC9zdHlsZT4KICAgIDwvZGVmcz4KPHBvbHlnb24gcG9pbnRzPSIyOCAzMCAyMCAyNiAyMCAzNCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMzYgMzAgMjggMjYgMjggMzQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM2IDMwIGggMjAiIC8+PHRleHQgeD0iNTYiIHk9IjM0Ij5DT1BZPC90ZXh0PjxwYXRoIGQ9Ik0gOTYgMzAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMTE2IiB5PSIzNCI+RmlsZU5hbWU8L3RleHQ+PHBhdGggZD0iTSAxOTYgMzAgaCAyMCIgLz48cGF0aCBkPSJNIDIxNiAzMCBoIDQ0MCIgLz48cG9seWdvbiBwb2ludHM9IjY2NCAzMCA2NTYgMjYgNjU2IDM0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSI2NjQgMzAgNjcyIDI2IDY3MiAzNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMjE2IDMwIHYgNDAgaCAyMCIgLz48dGV4dCB4PSIyMzYiIHk9Ijc0Ij7CoFJFUExBQ0lORzwvdGV4dD48cGF0aCBkPSJNIDMzNiA3MCBoIDQwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIzNzYiIHk9Ijc0Ij7CoExpdGVyYWw8L3RleHQ+PHBhdGggZD0iTSA0NTYgNzAgaCAyMCIgLz48dGV4dCB4PSI0NzYiIHk9Ijc0Ij5CWTwvdGV4dD48cGF0aCBkPSJNIDQ5NiA3MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSI1MTYiIHk9Ijc0Ij7CoExpdGVyYWw8L3RleHQ+PHBhdGggZD0iTSA1OTYgNzAgaCAyMCIgLz48cGF0aCBkPSJNIDYxNiA3MCB2IC0yMCBoIC0yNjAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjM1NiA2OSAzNTIgNjEgMzYwIDYxIj48L3BvbHlnb24+PHBhdGggZD0iTSA2MTYgNzAgaCAyMCB2IC00MCIgLz48cG9seWdvbiBwb2ludHM9IjYzNiAzMSA2MzIgMzkgNjQwIDM5Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Remarks

-   argument literals are quoted with `===` on each side, like this: `COPY FILENAME REPLACING ===XXX=== BY ===YYY===`
-   Essentially, this statement is a big challenge for static code analysis tools and techniques, because it forces an additional phase of the analysis to collect all possible replacements done on the code. It can be implemented as a separate phase, known among legacy compiler developers as "`COPY` book expansion", in which case the emphasis is shifted to the problem of traceability (because expansion changes line numbers, and error messages must report the original line numbers).

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[COPY](cobol.html "a directive to include another file")]{.ff .used} [[REPLACE](cobol.html "a directive to switch ongoing replacements on or off")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[%INCLUDE](pli.html "incorporate external text into the source program")]{.ff .used}\

------------------------------------------------------------------------
datadivision.md
# [[BabyCobol](index.html)]{.ff .lang}: [[DATA DIVISION](datadivision.html)]{.ff .used}

the second division of a program

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMjIwcHgiIGhlaWdodD0iMTAwcHgiPgogICAgPGRlZnM+CiAgICAgICAgPHN0eWxlIHR5cGU9InRleHQvY3NzIj4KICAgICAgICAgICAgQG5hbWVzcGFjZSAiaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciOwogICAgICAgICAgICBzdmcge2JhY2tncm91bmQtY29sb3I6IHdoaXRlO30KICAgICAgICAgICAgcGF0aCB7ZmlsbDogbm9uZTsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHBvbHlnb24ge2ZpbGw6IGJsYWNrOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgdGV4dCB7Zm9udC1zaXplOjE2cHg7ZmlsbDpibGFjaztmb250LXdlaWdodDpib2xkO2ZvbnQtZmFtaWx5Om1vbm9zcGFjZTt9CiAgICAgICAgICAgIHRleHQuaSB7Zm9udC1zdHlsZTppdGFsaWM7fQogICAgICAgIDwvc3R5bGU+CiAgICA8L2RlZnM+Cjxwb2x5Z29uIHBvaW50cz0iMjggNDAgMjAgMzYgMjAgNDQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjM2IDQwIDI4IDM2IDI4IDQ0Ij48L3BvbHlnb24+PHBhdGggZD0iTSAzNiA0MCBoIDIwIiAvPjx0ZXh0IHg9IjU2IiB5PSI0NCI+REFUQSBESVZJU0lPTi48L3RleHQ+PHBhdGggZD0iTSAxOTYgNDAgaCA0MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMjM2IiB5PSI0NCI+wqBMZXZlbDwvdGV4dD48cGF0aCBkPSJNIDI5NiA0MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIzMTYiIHk9IjQ0Ij5JZGVudGlmaWVyPC90ZXh0PjxwYXRoIGQ9Ik0gNDE2IDQwIGggMjAiIC8+PHBhdGggZD0iTSA0MzYgNDAgaCAzMDAiIC8+PHBhdGggZD0iTSA3MzYgNDAgaCAzODAiIC8+PHRleHQgeD0iMTExNiIgeT0iNDQiPsKgLjwvdGV4dD48cGF0aCBkPSJNIDExMzYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDExNTYgNDAgdiAtMjAgaCAtOTQwIHYgMjAiIC8+PHBvbHlnb24gcG9pbnRzPSIyMTYgMzkgMjEyIDMxIDIyMCAzMSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMTE1NiA0MCBoIDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMTE4NCA0MCAxMTc2IDM2IDExNzYgNDQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjExODQgNDAgMTE5MiAzNiAxMTkyIDQ0Ij48L3BvbHlnb24+PHBhdGggZD0iTSA0MzYgNDAgdiAyMCBoIDIwIiAvPjx0ZXh0IHg9IjQ1NiIgeT0iNjQiPlBJQ1RVUkUgSVM8L3RleHQ+PHBhdGggZD0iTSA1NTYgNjAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNTc2IiB5PSI2NCI+UmVwcmVzZW50YXRpb248L3RleHQ+PHBhdGggZD0iTSA3MTYgNjAgaCAyMCIgLz48cGF0aCBkPSJNIDQzNiA0MCB2IDQwIGggMjAiIC8+PHRleHQgeD0iNDU2IiB5PSI4NCI+TElLRTwvdGV4dD48cGF0aCBkPSJNIDQ5NiA4MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSI1MTYiIHk9Ijg0Ij5JZGVudGlmaWVyPC90ZXh0PjxwYXRoIGQ9Ik0gNjE2IDgwIGggMTAwIiAvPjxwYXRoIGQ9Ik0gNzE2IDgwIGggMjAgdiAtNDAiIC8+PHBvbHlnb24gcG9pbnRzPSI3MzYgNDEgNzMyIDQ5IDc0MCA0OSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNzM2IDQwIGggMjAiIC8+PHBhdGggZD0iTSA3NTYgNDAgdiAyMCBoIDIwIiAvPjx0ZXh0IHg9Ijc3NiIgeT0iNjQiPk9DQ1VSUzwvdGV4dD48cGF0aCBkPSJNIDgzNiA2MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSI4NTYiIHk9IjY0Ij5JbnRlZ2VyTGl0ZXJhbDwvdGV4dD48cGF0aCBkPSJNIDk5NiA2MCBoIDIwIiAvPjx0ZXh0IHg9IjEwMTYiIHk9IjY0Ij7CoFRJTUVTPC90ZXh0PjxwYXRoIGQ9Ik0gMTA3NiA2MCBoIDIwIHYgLTIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMTA5NiA0MSAxMDkyIDQ5IDExMDAgNDkiPjwvcG9seWdvbj4KPC9zdmc+)

## Remarks

-   unlike the preceding [`IDENTIFICATION DIVISION`](identificationdivision.html), this division is optional and can be skipped if there is no data to declare
-   each entry declares a field (an elementary variable) or a record (a composite structure)
-   the level numbers show hierarchy: entries with higher level numbers are included in the record defined above them at a lower level number
-   each level number is exactly two digits long
-   the level numbers can never go below the level number of the first entry (which is traditionally `01`, but doesn't have to be)
-   the `PICTURE` clause defines a field, specifying its format as a representation string, containing the following characters:
    -   `9` --- any numerical digit
    -   `A` --- any alphabetic character or whitespace
    -   `X` --- any single character
    -   `Z` --- a leading digit, disappearing into space if zero
    -   `S` --- a sign, positive or negative, space is treated as a plus
    -   `V` --- a decimal separator (usually . or ,)
    -   the `S` and `V` symbols may occur once in the representation string
-   the `LIKE` clause copies the type from elsewhere, and thus declares either a field (by essentially copying its representation string) or a record (by copying its entire structure)
-   if neither the `PICTURE` nor the `LIKE` clause are used, such an entry defines a record
-   the `OCCURS` clause turns an individual record or field into an array of them
-   `LIKE` inherits the basic type, not the occurring one
-   comment lines and [`COPY`](copy.html) instructions are allowed inside this division as well
-   the data division can be followed by the [`PROCEDURE DIVISION`](proceduredivision.html)

## Origins

[[AppBuilder](appbuilder.html)]{.ff .lang}   [[DCL](appbuilder.html "a declaration block for local fields and views")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[DATA DIVISION](cobol.html "the top level program unit with variable definitions")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[DECLARE](pli.html "declare a variable and specify its attributes")]{.ff .used} [[DEFINE](pli.html "define a structure or an alias for the data type")]{.ff .used}\

------------------------------------------------------------------------
display.md
# [[BabyCobol](index.html)]{.ff .lang}: [[DISPLAY](display.html)]{.ff .used}

a statement to display text on the terminal

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI4NDBweCIgaGVpZ2h0PSIxMjBweCI+CiAgICA8ZGVmcz4KICAgICAgICA8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgICAgICAgICBAbmFtZXNwYWNlICJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI7CiAgICAgICAgICAgIHN2ZyB7YmFja2dyb3VuZC1jb2xvcjogd2hpdGU7fQogICAgICAgICAgICBwYXRoIHtmaWxsOiBub25lOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgcG9seWdvbiB7ZmlsbDogYmxhY2s7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICB0ZXh0IHtmb250LXNpemU6MTZweDtmaWxsOmJsYWNrO2ZvbnQtd2VpZ2h0OmJvbGQ7Zm9udC1mYW1pbHk6bW9ub3NwYWNlO30KICAgICAgICAgICAgdGV4dC5pIHtmb250LXN0eWxlOml0YWxpYzt9CiAgICAgICAgPC9zdHlsZT4KICAgIDwvZGVmcz4KPHBvbHlnb24gcG9pbnRzPSIyOCA0MCAyMCAzNiAyMCA0NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMzYgNDAgMjggMzYgMjggNDQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM2IDQwIGggMjAiIC8+PHRleHQgeD0iNTYiIHk9IjQ0Ij7CoERJU1BMQVk8L3RleHQ+PHBhdGggZD0iTSAxMzYgNDAgaCA0MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMTc2IiB5PSI0NCI+QXRvbWljPC90ZXh0PjxwYXRoIGQ9Ik0gMjM2IDQwIGggMjAiIC8+PHBhdGggZD0iTSAyNTYgNDAgaCAyNDAiIC8+PHBhdGggZD0iTSAyNTYgNDAgdiAyMCBoIDIwIiAvPjx0ZXh0IHg9IjI3NiIgeT0iNjQiPkRFTElNSVRFRCBCWTwvdGV4dD48cGF0aCBkPSJNIDM5NiA2MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNDE2IDYwIGggNDAiIC8+PHRleHQgeD0iNDU2IiB5PSI2NCI+U0laRTwvdGV4dD48cGF0aCBkPSJNIDQ5NiA2MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNTE2IDYwIGggMjAgdiAtMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI1MzYgNDEgNTMyIDQ5IDU0MCA0OSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNDE2IDYwIHYgMjAgaCAyMCIgLz48dGV4dCB4PSI0MzYiIHk9Ijg0Ij7CoFNQQUNFPC90ZXh0PjxwYXRoIGQ9Ik0gNDk2IDgwIGggNDAiIC8+PHBhdGggZD0iTSA0MTYgNjAgdiA0MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSI0MzYiIHk9IjEwNCI+wqBMaXRlcmFsPC90ZXh0PjxwYXRoIGQ9Ik0gNTE2IDEwMCBoIDIwIHYgLTYwIiAvPjxwb2x5Z29uIHBvaW50cz0iNTM2IDQxIDUzMiA0OSA1NDAgNDkiPjwvcG9seWdvbj48cGF0aCBkPSJNIDQ5NiA0MCBoIDYwIiAvPjxwYXRoIGQ9Ik0gNTU2IDQwIHYgLTIwIGggLTQwMCB2IDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMTU2IDM5IDE1MiAzMSAxNjAgMzEiPjwvcG9seWdvbj48cGF0aCBkPSJNIDU1NiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNTc2IDQwIGggMjQwIiAvPjxwb2x5Z29uIHBvaW50cz0iODI0IDQwIDgxNiAzNiA4MTYgNDQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjgyNCA0MCA4MzIgMzYgODMyIDQ0Ij48L3BvbHlnb24+PHBhdGggZD0iTSA1NzYgNDAgdiAyMCBoIDIwIiAvPjx0ZXh0IHg9IjU5NiIgeT0iNjQiPsKgV0lUSCBOTyBBRFZBTkNJTkc8L3RleHQ+PHBhdGggZD0iTSA3NzYgNjAgaCAyMCB2IC0yMCIgLz48cG9seWdvbiBwb2ludHM9Ijc5NiA0MSA3OTIgNDkgODAwIDQ5Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Remarks

-   `WITH NO ADVANCING` clause prints the text without appending newline character(s)
-   values `DELIMITED BY SPACE` are printed by converting them to strings and trimming empty spaces from both sides
-   values `DELIMITED BY SIZE` are printed by converting them to strings of fixed size corresponding to their defined (identifiers) or inferred (literals) type
-   values `DELIMITED BY `*`Literal`* are printed up to the first occurrence of the substring expressed by the literal

## Origins

[[CLIST](clist.html)]{.ff .lang}   [[WRITE](clist.html "display text on the terminal")]{.ff .used} [[WRITENR](clist.html "display text on the terminal without advancing a cursor to the next line")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[DISPLAY](cobol.html "print contents of data fields on the display")]{.ff .used} [[STRING](cobol.html "format and concatenate strings")]{.ff .used}\
[[FORTRAN](fortran.html)]{.ff .lang}   [[FORMAT](fortran.html "prepare data for printing")]{.ff .used} [[PRINT](fortran.html "display data according to the given format")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[DISPLAY](pli.html "display a message on user's screen")]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[SAY](rexx.html "display a line on the terminal")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[DSPLY](rpg.html "display a message")]{.ff .used}\

------------------------------------------------------------------------
divide.md
# [[BabyCobol](index.html)]{.ff .lang}: [[DIVIDE](divide.html)]{.ff .used}

a statement to divide a numeric value by other numbers

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAwcHgiIGhlaWdodD0iMTIwcHgiPgogICAgPGRlZnM+CiAgICAgICAgPHN0eWxlIHR5cGU9InRleHQvY3NzIj4KICAgICAgICAgICAgQG5hbWVzcGFjZSAiaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciOwogICAgICAgICAgICBzdmcge2JhY2tncm91bmQtY29sb3I6IHdoaXRlO30KICAgICAgICAgICAgcGF0aCB7ZmlsbDogbm9uZTsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHBvbHlnb24ge2ZpbGw6IGJsYWNrOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgdGV4dCB7Zm9udC1zaXplOjE2cHg7ZmlsbDpibGFjaztmb250LXdlaWdodDpib2xkO2ZvbnQtZmFtaWx5Om1vbm9zcGFjZTt9CiAgICAgICAgICAgIHRleHQuaSB7Zm9udC1zdHlsZTppdGFsaWM7fQogICAgICAgIDwvc3R5bGU+CiAgICA8L2RlZnM+Cjxwb2x5Z29uIHBvaW50cz0iMjggNDAgMjAgMzYgMjAgNDQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjM2IDQwIDI4IDM2IDI4IDQ0Ij48L3BvbHlnb24+PHBhdGggZD0iTSAzNiA0MCBoIDIwIiAvPjx0ZXh0IHg9IjU2IiB5PSI0NCI+RElWSURFPC90ZXh0PjxwYXRoIGQ9Ik0gMTE2IDQwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjEzNiIgeT0iNDQiPkF0b21pYzwvdGV4dD48cGF0aCBkPSJNIDE5NiA0MCBoIDIwIiAvPjx0ZXh0IHg9IjIxNiIgeT0iNDQiPklOVE88L3RleHQ+PHBhdGggZD0iTSAyNTYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDI3NiA0MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIyOTYiIHk9IjQ0Ij5BdG9taWM8L3RleHQ+PHBhdGggZD0iTSAzNTYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDM3NiA0MCB2IC0yMCBoIC0xMDAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjI3NiAzOSAyNzIgMzEgMjgwIDMxIj48L3BvbHlnb24+PHBhdGggZD0iTSAzNzYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDM5NiA0MCBoIDU2MCIgLz48cG9seWdvbiBwb2ludHM9Ijk2NCA0MCA5NTYgMzYgOTU2IDQ0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSI5NjQgNDAgOTcyIDM2IDk3MiA0NCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzk2IDQwIHYgNDAgaCAyMCIgLz48dGV4dCB4PSI0MTYiIHk9Ijg0Ij5HSVZJTkc8L3RleHQ+PHBhdGggZD0iTSA0NzYgODAgaCA0MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNTE2IiB5PSI4NCI+SWRlbnRpZmllcjwvdGV4dD48cGF0aCBkPSJNIDYxNiA4MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNjM2IDgwIHYgLTIwIGggLTE0MCB2IDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iNDk2IDc5IDQ5MiA3MSA1MDAgNzEiPjwvcG9seWdvbj48cGF0aCBkPSJNIDYzNiA4MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNjU2IDgwIGggMjYwIiAvPjxwYXRoIGQ9Ik0gOTE2IDgwIGggMjAgdiAtNDAiIC8+PHBvbHlnb24gcG9pbnRzPSI5MzYgNDEgOTMyIDQ5IDk0MCA0OSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNjU2IDgwIHYgMjAgaCAyMCIgLz48dGV4dCB4PSI2NzYiIHk9IjEwNCI+wqBSRU1BSU5ERVI8L3RleHQ+PHBhdGggZD0iTSA3NzYgMTAwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9Ijc5NiIgeT0iMTA0Ij5JZGVudGlmaWVyPC90ZXh0PjxwYXRoIGQ9Ik0gODk2IDEwMCBoIDIwIHYgLTIwIiAvPjxwb2x5Z29uIHBvaW50cz0iOTE2IDgxIDkxMiA4OSA5MjAgODkiPjwvcG9seWdvbj4KPC9zdmc+)

## Remarks

-   divides each of the second arguments by the first argument and overwrites the old value of each second argument, unless the third argument specifies a different storage place
-   so, for example, `DIVIDE 2 INTO X Y Z` will halve the values of `X`, `Y` and `Z`
-   all four arguments obey the rules of sufficient qualification
-   the first argument can be a literal
-   if the second argument is a literal, the third argument is mandatory
-   if the third argument is present, there can be only one second argument
-   if the fourth argument is present, there can be only one third argument
-   if the four argument is not present, the remainder is lost
-   any of the four arguments can be an identifier defined with a numeric picture clause (free from `A` and `X`)

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[DIVIDE](cobol.html "divide one numeric operand into others")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[DIV](rpg.html "divide a number by another number")]{.ff .used} [[MVR](rpg.html "calculate a reminder of a division")]{.ff .used}\

------------------------------------------------------------------------
division.md
# [[BabyCobol](index.html)]{.ff .lang}: [[DIVISION](division.html)]{.ff .used}

the top unit of a BabyCobol program

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI1MjBweCIgaGVpZ2h0PSI2MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDIwIDIwIDE2IDIwIDI0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiAyMCAyOCAxNiAyOCAyNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgMjAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNTYiIHk9IjI0Ij5JZGVudGlmaWNhdGlvbjwvdGV4dD48cGF0aCBkPSJNIDE5NiAyMCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMjE2IDIwIGggMTAwIiAvPjxwYXRoIGQ9Ik0gMzE2IDIwIGggMTYwIiAvPjxwb2x5Z29uIHBvaW50cz0iNDg0IDIwIDQ3NiAxNiA0NzYgMjQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjQ4NCAyMCA0OTIgMTYgNDkyIDI0Ij48L3BvbHlnb24+PHBhdGggZD0iTSAyMTYgMjAgdiAyMCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIyMzYiIHk9IjQ0Ij5EYXRhPC90ZXh0PjxwYXRoIGQ9Ik0gMjc2IDQwIGggMjAgdiAtMjAiIC8+PHBvbHlnb24gcG9pbnRzPSIyOTYgMjEgMjkyIDI5IDMwMCAyOSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzE2IDIwIHYgMjAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMzM2IiB5PSI0NCI+wqBQcm9jZWR1cmU8L3RleHQ+PHBhdGggZD0iTSA0MzYgNDAgaCAyMCB2IC0yMCIgLz48cG9seWdvbiBwb2ludHM9IjQ1NiAyMSA0NTIgMjkgNDYwIDI5Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Remarks

-   every BabyCobol program consists of up to three divisions
-   the first division is the only one mandatory, and it is the [`IDENTIFICATION DIVISION`](identificationdivision.html), it contains some data that helps to identify this program and distinguish it from other programs
-   the second division is optional, and it is the [`DATA DIVISION`](datadivision.html), it contains field and structure declarations
-   the third division is also optional, and it is the [`PROCEDURE DIVISION`](proceduredivision.html), it contains executable code
-   [`COPY`](copy.html) is the only instruction that is allowed in any division

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[DIVISION](cobol.html "the top level unit of any program")]{.ff .used}\

------------------------------------------------------------------------
evaluate.md
# [[BabyCobol](index.html)]{.ff .lang}: [[EVALUATE](evaluate.html)]{.ff .used}

a composite statement allowing to branch to multiple locations based on several conditions

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDYwcHgiIGhlaWdodD0iMjIwcHgiPgogICAgPGRlZnM+CiAgICAgICAgPHN0eWxlIHR5cGU9InRleHQvY3NzIj4KICAgICAgICAgICAgQG5hbWVzcGFjZSAiaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciOwogICAgICAgICAgICBzdmcge2JhY2tncm91bmQtY29sb3I6IHdoaXRlO30KICAgICAgICAgICAgcGF0aCB7ZmlsbDogbm9uZTsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHBvbHlnb24ge2ZpbGw6IGJsYWNrOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgdGV4dCB7Zm9udC1zaXplOjE2cHg7ZmlsbDpibGFjaztmb250LXdlaWdodDpib2xkO2ZvbnQtZmFtaWx5Om1vbm9zcGFjZTt9CiAgICAgICAgICAgIHRleHQuaSB7Zm9udC1zdHlsZTppdGFsaWM7fQogICAgICAgIDwvc3R5bGU+CiAgICA8L2RlZnM+Cjxwb2x5Z29uIHBvaW50cz0iMjggNjAgMjAgNTYgMjAgNjQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjM2IDYwIDI4IDU2IDI4IDY0Ij48L3BvbHlnb24+PHBhdGggZD0iTSAzNiA2MCBoIDIwIiAvPjx0ZXh0IHg9IjU2IiB5PSI2NCI+RVZBTFVBVEU8L3RleHQ+PHBhdGggZD0iTSAxMzYgNjAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMTU2IiB5PSI2NCI+wqBBbnlFeHByZXNzaW9uPC90ZXh0PjxwYXRoIGQ9Ik0gMjk2IDYwIGggMjAiIC8+PHBhdGggZD0iTSAzMTYgNjAgaCAzMDAiIC8+PHBhdGggZD0iTSAzMTYgNjAgdiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMzM2IDEwMCBoIDIwIiAvPjx0ZXh0IHg9IjM1NiIgeT0iMTA0Ij5BTFNPPC90ZXh0PjxwYXRoIGQ9Ik0gMzk2IDEwMCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSI0MTYiIHk9IjEwNCI+wqBBbnlFeHByZXNzaW9uPC90ZXh0PjxwYXRoIGQ9Ik0gNTU2IDEwMCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNTc2IDEwMCB2IC0yMCBoIC0yNDAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjMzNiA5OSAzMzIgOTEgMzQwIDkxIj48L3BvbHlnb24+PHBhdGggZD0iTSA1NzYgMTAwIGggMjAgdiAtNDAiIC8+PHBvbHlnb24gcG9pbnRzPSI1OTYgNjEgNTkyIDY5IDYwMCA2OSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNTU2IDYwIGggODAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjYzNiIgeT0iNjQiPldoZW5DbGF1c2U8L3RleHQ+PHBhdGggZD0iTSA3MzYgNjAgaCA2MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNzk2IiB5PSI2NCI+wqBTdGF0ZW1lbnQ8L3RleHQ+PHBhdGggZD0iTSA4OTYgNjAgaCAyMCIgLz48cGF0aCBkPSJNIDkxNiA2MCB2IC0yMCBoIC0xNDAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9Ijc3NiA1OSA3NzIgNTEgNzgwIDUxIj48L3BvbHlnb24+PHBhdGggZD0iTSA5MTYgNjAgaCAyMCIgLz48cGF0aCBkPSJNIDkzNiA2MCB2IC00MCBoIC0zMjAgdiA0MCIgLz48cG9seWdvbiBwb2ludHM9IjYxNiA1OSA2MTIgNTEgNjIwIDUxIj48L3BvbHlnb24+PHBhdGggZD0iTSA5MzYgNjAgaCAyMCIgLz48dGV4dCB4PSI5NTYiIHk9IjY0Ij7CoEVORDwvdGV4dD48cGF0aCBkPSJNIDk5NiA2MCBoIDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMTAyNCA2MCAxMDE2IDU2IDEwMTYgNjQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjEwMjQgNjAgMTAzMiA1NiAxMDMyIDY0Ij48L3BvbHlnb24+PHRleHQgY2xhc3M9ImkiIHg9IjIwIiB5PSIxNjQiPldoZW5DbGF1c2U8L3RleHQ+PHBvbHlnb24gcG9pbnRzPSIxMjggMTYwIDEyMCAxNTYgMTIwIDE2NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMTM2IDE2MCAxMjggMTU2IDEyOCAxNjQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDEzNiAxNjAgaCAyMCIgLz48dGV4dCB4PSIxNTYiIHk9IjE2NCI+V0hFTjwvdGV4dD48cGF0aCBkPSJNIDE5NiAxNjAgaCAyMCIgLz48cGF0aCBkPSJNIDIxNiAxNjAgaCA2MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMjc2IiB5PSIxNjQiPkF0b21pYzwvdGV4dD48cGF0aCBkPSJNIDMzNiAxNjAgaCAyMCIgLz48cGF0aCBkPSJNIDM1NiAxNjAgdiAyMCBoIDIwIiAvPjx0ZXh0IHg9IjM3NiIgeT0iMTg0Ij7CoFRIUk9VR0g8L3RleHQ+PHBhdGggZD0iTSA0NTYgMTgwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjQ3NiIgeT0iMTg0Ij5BdG9taWM8L3RleHQ+PHBhdGggZD0iTSA1MzYgMTgwIGggMjAgdiAtMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI1NTYgMTYxIDU1MiAxNjkgNTYwIDE2OSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzU2IDE2MCBoIDIwMCIgLz48cGF0aCBkPSJNIDU1NiAxNjAgaCAyMCIgLz48cGF0aCBkPSJNIDU3NiAxNjAgdiAtMjAgaCAtMzIwIHYgMjAiIC8+PHBvbHlnb24gcG9pbnRzPSIyNTYgMTU5IDI1MiAxNTEgMjYwIDE1MSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNTc2IDE2MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNTk2IDE2MCBoIDIwIiAvPjx0ZXh0IHg9IjYxNiIgeT0iMTY0Ij5BTFNPPC90ZXh0PjxwYXRoIGQ9Ik0gNjU2IDE2MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNjc2IDE2MCB2IC00MCBoIC00NDAgdiA0MCIgLz48cG9seWdvbiBwb2ludHM9IjIzNiAxNTkgMjMyIDE1MSAyNDAgMTUxIj48L3BvbHlnb24+PHBhdGggZD0iTSA1OTYgMTYwIHYgMjAgaCAyMCIgLz48cGF0aCBkPSJNIDYxNiAxODAgaCA4MCIgLz48cG9seWdvbiBwb2ludHM9IjcwNCAxODAgNjk2IDE3NiA2OTYgMTg0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSI3MDQgMTgwIDcxMiAxNzYgNzEyIDE4NCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMjE2IDE2MCB2IDQwIGggMjAiIC8+PHBhdGggZD0iTSAyMzYgMjAwIGggMjAiIC8+PHRleHQgeD0iMjU2IiB5PSIyMDQiPsKgT1RIRVI8L3RleHQ+PHBhdGggZD0iTSAzMTYgMjAwIGggMzQwIiAvPjxwYXRoIGQ9Ik0gNjU2IDIwMCBoIDIwIHYgLTIwIiAvPjxwb2x5Z29uIHBvaW50cz0iNjc2IDE4MSA2NzIgMTg5IDY4MCAxODkiPjwvcG9seWdvbj4KPC9zdmc+)

## Remarks

-   Essentially, besides being a form of a branching, the `EVALUATE` statement brings infix operators into the language in a place where space-separated expressions are allowed.
-   Implementers are forced deal with parsing ambiguities between `A-B` (single field), `A -B` (two expressions: one field and one unary expression) and `A - B` (a binary subtraction of two fields).
-   Spaces in the example above serve a demonstrative purpose, and are ignored in lexical analysis.
-   Note that `WHEN`-branches consist of statements, not sentences.
-   There can be only one `WHEN OTHER` clause within each `EVALUATE`.
-   Unlike COBOL, BabyCobol does not support empty `WHEN` branches: grouping of multiple conditions is done by the `WHEN` clause itself, like in 4GLs like [AppBuilder](appbuilder.html), so `WHEN 1 WHEN 2 DISPLAY "OK"` in COBOL would look like `WHEN 1 2 DISPLAY "OK"` in BabyCobol.
-   Each branch ends with its last statement, and there is no fall-through or other form of control flow from one branch to another.
-   If `EVALUATE` has `ALSO` clauses, then each `WHEN` need to have that exact number of `ALSO` clauses, except for `WHEN OTHER`.
-   [`NEXT SENTENCE`](nextsentence.html) in the middle of `EVALUATE` statement works as usual, exiting it and continuing at the next sentence after it.

## Origins

[[AppBuilder](appbuilder.html)]{.ff .lang}   [[CASEOF](appbuilder.html "branch to multiple locations based on several conditions")]{.ff .used}\
[[CLIST](clist.html)]{.ff .lang}   [[SELECT](clist.html "branch to multiple locations based on several conditions")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[EVALUATE](cobol.html "shorthand notation for a series of nested IF statements")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[OTHERWISE](pli.html)]{.ff .used} [[WHEN](pli.html)]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[SELECT](rexx.html "branch to multiple locations based on several conditions")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[OTHER](rpg.html "define a branch in a SELECT group to be executed if no WHEN condition matches")]{.ff .used} [[SELECT](rpg.html "branch to multiple locations based on several conditions")]{.ff .used} [[WHEN](rpg.html "define a branch within a SELECT block")]{.ff .used}\

------------------------------------------------------------------------
fortran.md
# [[BabyCobol](index.html)]{.ff .lang}: The [[FORTRAN](fortran.html)]{.ff .lang} Origins

------------------------------------------------------------------------

## Features:

[[ASSIGN](alter.html "change the target of an assigned GO TO statement")]{.ff .used} [BACKSPACE]{.ff title="make the tape step back one record"} [[CALL](perform.html "invoke a subroutine")]{.ff .used} [[CONTINUE](nextsentence.html "do nothing")]{.ff .used} [DIMENSION]{.ff title="provide data sizes for memory allocation"} [[DO](loop.html "execute a statement repeatedly")]{.ff .used} [END FILE]{.ff title="write an end of file marker on tape"} [EQUIVALENCE]{.ff title="define which data structures are allowed to reuse each others' memory"} [[FORMAT](display.html "prepare data for printing")]{.ff .used} [FREQUENCY]{.ff title="provide an estimation of how often which lines will be executed"} [[GO TO](goto.html "branch unconditionally within a program (unconditional, assigned, computed)")]{.ff .used} [[IF](if.html "branch conditionally within a program, if the value is less than, equal to, or greater than zero")]{.ff .used} [PAUSE]{.ff title="temporary terminate the program"} [[PRINT](display.html "display data according to the given format")]{.ff .used} [PUNCH]{.ff title="punch cards"} [[READ](accept.html "accepts data from input cards, tape or drum")]{.ff .used} [[RETURN](stop.html "terminate a subprogram and return control to the calling program unit")]{.ff .used} [REWIND]{.ff title="rewind a tape unit"} [SENSE LIGHT]{.ff title="switch sense lights on the 704 console on or off"} [[STOP](stop.html "definitively terminate the program")]{.ff .used} [WRITE]{.ff title="write data to a tape or a drum"}

## Sources:

-   The FORTRAN Automatic Coding System for the IBM 704 EDPM, October 15, 1956

------------------------------------------------------------------------
goto.md
# [[BabyCobol](index.html)]{.ff .lang}: [[GO TO](goto.html)]{.ff .used}

a statement to branch unconditionally to a paragraph within a program

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIzNDBweCIgaGVpZ2h0PSI2MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDMwIDIwIDI2IDIwIDM0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiAzMCAyOCAyNiAyOCAzNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgMzAgaCAyMCIgLz48dGV4dCB4PSI1NiIgeT0iMzQiPsKgR08gVE88L3RleHQ+PHBhdGggZD0iTSAxMTYgMzAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMTM2IiB5PSIzNCI+wqBQcm9jZWR1cmVOYW1lPC90ZXh0PjxwYXRoIGQ9Ik0gMjc2IDMwIGggMjAiIC8+PHBvbHlnb24gcG9pbnRzPSIzMDQgMzAgMjk2IDI2IDI5NiAzNCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMzA0IDMwIDMxMiAyNiAzMTIgMzQiPjwvcG9seWdvbj4KPC9zdmc+)

## Remarks

-   branching outside an active `LOOP` terminates its iteration
-   branching outside paragraphs being `PERFORM`ed terminates the performance and forfeits the call place, acting as a redirected return
-   the argument can be a field name, possibly being reassigned at runtime with `MOVE`
-   See a [separate example](https://slebok.github.io/baby/perform-goto.html) showcasing the interplay between [`PERFORM`](perform.html) and `GO TO`.

## Origins

[[CLIST](clist.html)]{.ff .lang}   [[GOTO](clist.html "branch unconditionally within a program")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[GO TO](cobol.html "branch unconditionally to another section or paragraph")]{.ff .used}\
[[FORTRAN](fortran.html)]{.ff .lang}   [[GO TO](fortran.html "branch unconditionally within a program (unconditional, assigned, computed)")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[GO TO](pli.html "transfer control to a labelled statement")]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[SIGNAL](rexx.html "branch unconditionally within a program")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[GOTO](rpg.html "branch unconditionally within a program")]{.ff .used}\

------------------------------------------------------------------------
identificationdivision.md
# [[BabyCobol](index.html)]{.ff .lang}: [[IDENTIFICATION DIVISION](identificationdivision.html)]{.ff .used}

the first division of a program

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI3MjBweCIgaGVpZ2h0PSI2MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDQwIDIwIDM2IDIwIDQ0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiA0MCAyOCAzNiAyOCA0NCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgNDAgaCAyMCIgLz48dGV4dCB4PSI1NiIgeT0iNDQiPklERU5USUZJQ0FUSU9OIERJVklTSU9OLjwvdGV4dD48cGF0aCBkPSJNIDI5NiA0MCBoIDQwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIzMzYiIHk9IjQ0Ij5OYW1lPC90ZXh0PjxwYXRoIGQ9Ik0gMzc2IDQwIGggMjAiIC8+PHRleHQgeD0iMzk2IiB5PSI0NCI+wqAuPC90ZXh0PjxwYXRoIGQ9Ik0gNDE2IDQwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjQzNiIgeT0iNDQiPsKgVmFsdWU8L3RleHQ+PHBhdGggZD0iTSA0OTYgNDAgaCAyMCIgLz48dGV4dCB4PSI1MTYiIHk9IjQ0Ij7CoC48L3RleHQ+PHBhdGggZD0iTSA1MzYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDU1NiA0MCB2IC0yMCBoIC0yNDAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjMxNiAzOSAzMTIgMzEgMzIwIDMxIj48L3BvbHlnb24+PHBhdGggZD0iTSA1NTYgNDAgaCAyMCIgLz48cG9seWdvbiBwb2ludHM9IjU4NCA0MCA1NzYgMzYgNTc2IDQ0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSI1ODQgNDAgNTkyIDM2IDU5MiA0NCI+PC9wb2x5Z29uPgo8L3N2Zz4=)

## Remarks

-   this division is mandatory, and each correct BabyCobol program should start with it
-   there can be many clauses, and both names and values are not restricted: they can contain letters, numbers, spaces, punctuation, etc, except for a dot.
-   to make your BabyCobol program more COBOL-like, start with the clause named `PROGRAM-ID`, and use any of the following clause names: `AUTHOR`, `INSTALLATION`, `DATE-WRITTEN`, `DATE-COMPILED`, `SECURITY`
-   comment lines and [`COPY`](copy.html) instructions are allowed inside this division as well
-   the identification division can be followed by either the [`DATA DIVISION`](datadivision.html) or the [`PROCEDURE DIVISION`](proceduredivision.html)

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[IDENTIFICATION DIVISION](cobol.html "the top level program unit with the program ID and other metadata")]{.ff .used}\

------------------------------------------------------------------------
if.md
# [[BabyCobol](index.html)]{.ff .lang}: [[IF](if.html)]{.ff .used}

a statement to branch conditionally to a paragraph within a program

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI5NDBweCIgaGVpZ2h0PSIxMDBweCI+CiAgICA8ZGVmcz4KICAgICAgICA8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgICAgICAgICBAbmFtZXNwYWNlICJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI7CiAgICAgICAgICAgIHN2ZyB7YmFja2dyb3VuZC1jb2xvcjogd2hpdGU7fQogICAgICAgICAgICBwYXRoIHtmaWxsOiBub25lOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgcG9seWdvbiB7ZmlsbDogYmxhY2s7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICB0ZXh0IHtmb250LXNpemU6MTZweDtmaWxsOmJsYWNrO2ZvbnQtd2VpZ2h0OmJvbGQ7Zm9udC1mYW1pbHk6bW9ub3NwYWNlO30KICAgICAgICAgICAgdGV4dC5pIHtmb250LXN0eWxlOml0YWxpYzt9CiAgICAgICAgPC9zdHlsZT4KICAgIDwvZGVmcz4KPHBvbHlnb24gcG9pbnRzPSIyOCA0MCAyMCAzNiAyMCA0NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMzYgNDAgMjggMzYgMjggNDQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM2IDQwIGggMjAiIC8+PHRleHQgeD0iNTYiIHk9IjQ0Ij5JRjwvdGV4dD48cGF0aCBkPSJNIDc2IDQwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9Ijk2IiB5PSI0NCI+wqBCb29sZWFuRXhwcmVzc2lvbjwvdGV4dD48cGF0aCBkPSJNIDI3NiA0MCBoIDIwIiAvPjx0ZXh0IHg9IjI5NiIgeT0iNDQiPlRIRU48L3RleHQ+PHBhdGggZD0iTSAzMzYgNDAgaCA0MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMzc2IiB5PSI0NCI+wqBTdGF0ZW1lbnQ8L3RleHQ+PHBhdGggZD0iTSA0NzYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDQ5NiA0MCB2IC0yMCBoIC0xNDAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjM1NiAzOSAzNTIgMzEgMzYwIDMxIj48L3BvbHlnb24+PHBhdGggZD0iTSA0OTYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDUxNiA0MCBoIDI2MCIgLz48cGF0aCBkPSJNIDc3NiA0MCBoIDEyMCIgLz48cG9seWdvbiBwb2ludHM9IjkwNCA0MCA4OTYgMzYgODk2IDQ0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSI5MDQgNDAgOTEyIDM2IDkxMiA0NCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNTE2IDQwIHYgNDAgaCAyMCIgLz48dGV4dCB4PSI1MzYiIHk9Ijg0Ij5FTFNFPC90ZXh0PjxwYXRoIGQ9Ik0gNTc2IDgwIGggNDAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjYxNiIgeT0iODQiPsKgU3RhdGVtZW50PC90ZXh0PjxwYXRoIGQ9Ik0gNzE2IDgwIGggMjAiIC8+PHBhdGggZD0iTSA3MzYgODAgdiAtMjAgaCAtMTQwIHYgMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI1OTYgNzkgNTkyIDcxIDYwMCA3MSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNzM2IDgwIGggMjAgdiAtNDAiIC8+PHBvbHlnb24gcG9pbnRzPSI3NTYgNDEgNzUyIDQ5IDc2MCA0OSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNzc2IDQwIHYgMjAgaCAyMCIgLz48dGV4dCB4PSI3OTYiIHk9IjY0Ij7CoEVORDwvdGV4dD48cGF0aCBkPSJNIDgzNiA2MCBoIDIwIHYgLTIwIiAvPjxwb2x5Z29uIHBvaW50cz0iODU2IDQxIDg1MiA0OSA4NjAgNDkiPjwvcG9seWdvbj4KPC9zdmc+)

## Remarks

-   an `END`-less `IF` is terminated at the end of the sentence (by `.`)
-   [dangling `ELSE`](https://en.wikipedia.org/wiki/Dangling_else) is not allowed and should produce an error
-   note that both branches of the `IF` consist of statements, not sentences, so dots inside the entire `IF` construct are not allowed

## Origins

[[AppBuilder](appbuilder.html)]{.ff .lang}   [[IF](appbuilder.html "branch conditionally within a program")]{.ff .used}\
[[CLIST](clist.html)]{.ff .lang}   [[IF](clist.html "branch conditionally within a program")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[IF](cobol.html "branch conditionally within a program")]{.ff .used}\
[[FORTRAN](fortran.html)]{.ff .lang}   [[IF](fortran.html "branch conditionally within a program, if the value is less than, equal to, or greater than zero")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[IF](pli.html "branch conditionally within a program")]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[IF](rexx.html "branch conditionally within a program")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[CAB](rpg.html "branch conditionally within a program")]{.ff .used} [[ELSE](rpg.html "the optional part of an IF or IFxx operator")]{.ff .used} [[ELSEIF](rpg.html "a combination of ELSE and IF")]{.ff .used}\

------------------------------------------------------------------------
index.md
# [[BabyCobol](index.html)]{.ff .lang}: The Language Reference

BabyCobol is a project in language design aimed at creating a language that is, on one hand, small enough to be quickly implementable (fully or partially) within any framework that can support its features, and, on the other hand, complex enough to cover typical problems of legacy language processing. If you learn how to compile [MiniJava](http://www.cs.tufts.edu/~sguyer/classes/comp181-2006/minijava.html), you stand a good chance of implementing a reasonably good compiler for any contemporary programming language. If you show how your language extension works on [Featherweight Java](https://doi.org/10.1145/503502.503505), it has a good chance of being applicable to any reasonable modern object-oriented programming language. If you can handle [BabyCobol](index.html) with your tool and with your skills, you are ready to face the challenges of software modernisation, codebase migration and legacy language processing in general. At this day and age, being future proof means being able to handle software of the past.

------------------------------------------------------------------------

## Features:

[[ACCEPT](accept.html "a statement to read user input and store it in variables")]{.ff .used} [[ADD](add.html "a statement to add two or more values together")]{.ff .used} [[ALTER](alter.html "a statement to change the target of an existing GO TO statement")]{.ff .used} [[CALL](call.html "an instruction to execute another program")]{.ff .used} [[COPY](copy.html "an instruction to insert contents from a different file")]{.ff .used} [[DATA DIVISION](datadivision.html "the second division of a program")]{.ff .used} [[DISPLAY](display.html "a statement to display text on the terminal")]{.ff .used} [[DIVIDE](divide.html "a statement to divide a numeric value by other numbers")]{.ff .used} [[DIVISION](division.html "the top unit of a BabyCobol program")]{.ff .used} [[EVALUATE](evaluate.html "a composite statement allowing to branch to multiple locations based on several conditions")]{.ff .used} [[GO TO](goto.html "a statement to branch unconditionally to a paragraph within a program")]{.ff .used} [[IDENTIFICATION DIVISION](identificationdivision.html "the first division of a program")]{.ff .used} [[IF](if.html "a statement to branch conditionally to a paragraph within a program")]{.ff .used} [[LOOP](loop.html "a composite statement for structural execution of a sequence of commands")]{.ff .used} [[MOVE](move.html "a partial structural assignment statement")]{.ff .used} [[MULTIPLY](multiply.html "a statement to multiply two or more numeric values")]{.ff .used} [[NEXT SENTENCE](nextsentence.html "a statement to branch unconditionally to the beginning of the next sentence")]{.ff .used} [[PERFORM](perform.html "a statement to call a paragraph or section within the program")]{.ff .used} [[PROCEDURE DIVISION](proceduredivision.html "the code-containing division in a program")]{.ff .used} [[SIGNAL](signal.html "an instruction to specify an error handler")]{.ff .used} [[STOP](stop.html "an instruction to terminate the program")]{.ff .used} [[SUBTRACT](subtract.html "a statement to subtract numeric values from other values")]{.ff .used}

## Origins

[[AppBuilder](appbuilder.html)]{.ff .lang}   [[CLIST](clist.html)]{.ff .lang}   [[COBOL](cobol.html)]{.ff .lang}   [[FORTRAN](fortran.html)]{.ff .lang}   [[PL/I](pli.html)]{.ff .lang}   [[REXX](rexx.html)]{.ff .lang}   [[RPG](rpg.html)]{.ff .lang}   

## Mentions

-   [Vadim Zaytsev](http://grammarware.net), *[Software Evolution](https://osiris.utwente.nl/student/OnderwijsCatalogusSelect.do?selectie=cursus&cursus=201400225&collegejaar=2023&taal=en)*, a [CBL](https://en.wikipedia.org/wiki/Challenge-based_learning)-based [flipped](https://en.wikipedia.org/wiki/Flipped_classroom) course, Universiteit Twente, 7 February 2024 -- 16 April 2024
-   📜 [Mart van Assen](https://www.linkedin.com/in/mart-van-assen-a4803727b/), [Aimé Ntagengerwa](https://www.linkedin.com/in/aim%C3%A9-ntagengerwa-555b45b5/), [Ömer Sayilir](https://www.linkedin.com/in/omer-faruk-sayilir/), [Vadim Zaytsev](http://grammarware.net). [*[Crossover: Towards Compiler-Enabled COBOL-C Interoperability](http://grammarware.net/writes/#Crossover2023#Crossover2023)*]{.lang}, a paper published at [GPCE 2023](https://2023.splashcon.org/home/gpce-2023#event-overview#event-overview), doi:[10.1145/3624007.3624055](https://doi.org/10.1145/3624007.3624055), 22--23 October 2023
-   [Vadim Zaytsev](http://grammarware.net), *[Software Evolution](https://osiris.utwente.nl/student/OnderwijsCatalogusSelect.do?selectie=cursus&cursus=201400225&collegejaar=2022&taal=en)*, a [CBL](https://en.wikipedia.org/wiki/Challenge-based_learning)-based [flipped](https://en.wikipedia.org/wiki/Flipped_classroom) course, Universiteit Twente, 8 February 2023 -- 6 July 2023
-   [Vadim Zaytsev](http://grammarware.net), *Legacy and Software Renovation*, [Software Evolution](https://datanose.nl/#course%5B110305%5D#course%5B110305%5D) guest lecture, Universiteit van Amsterdam, 28 November 2022
-   [Vadim Zaytsev](http://grammarware.net), *[Software Evolution](https://osiris.utwente.nl/student/OnderwijsCatalogusSelect.do?selectie=cursus&cursus=201400225&collegejaar=2021&taal=en)*, a [CBL](https://en.wikipedia.org/wiki/Challenge-based_learning)-based [flipped](https://en.wikipedia.org/wiki/Flipped_classroom) course, Universiteit Twente, 9 February 2022 -- 22 April 2022
-   [Vadim Zaytsev](http://grammarware.net), *Legacy and Software Renovation*, [Software Evolution](https://datanose.nl/#course%5B98644%5D#course%5B98644%5D) guest lecture, Universiteit van Amsterdam, 29 November 2021
-   [Vadim Zaytsev](http://grammarware.net), *[Software Evolution](https://osiris.utwente.nl/student/OnderwijsCatalogusSelect.do?selectie=cursus&cursus=201400225&collegejaar=2020&taal=en)*, a [CBL](https://en.wikipedia.org/wiki/Challenge-based_learning)-based [flipped](https://en.wikipedia.org/wiki/Flipped_classroom) course, Universiteit Twente, 3 February 2021 -- 23 June 2021
-   UTwente EEMCS Faculty, Formal Methods and Tools, *[Manfred Paul Award for Vadim Zaytsev](https://www.utwente.nl/en/eemcs/fmt/news-events/news/2021/2/951815/manfred-paul-award-for-vadim-zaytsev)*, a news item, 3 February 2021
-   🏆 [IFIP TC2](https://ifip-tc2.paluno.uni-due.de/mediawiki/index.php/Main_Page), [*Manfred Paul Award for Excellence in Software Theory and Practice*](https://ifip-tc2.paluno.uni-due.de/mediawiki/index.php/Main_Page#The_IFIP_TC2_Manfred_Paul_Award_for_Excellence_in_Software:_Theory_and_Practice#The_IFIP_TC2_Manfred_Paul_Award_for_Excellence_in_Software:_Theory_and_Practice) "for boldness in seeking real-world test cases for modern software language engineering tools by mining languages from the distant past", 22 December 2020
-   [Vadim Zaytsev](http://grammarware.net), *Legacy and Software Renovation*, [Software Evolution](https://datanose.nl/#course%5B88231%5D#course%5B88231%5D) guest lecture, Universiteit van Amsterdam, 30 November 2020
-   📜 [Vadim Zaytsev](http://grammarware.net), [[*Software Language Engineers' Worst Nightmare*](http://grammarware.net/writes/#BabyCobol2020#BabyCobol2020)]{.lang}, a paper published at [SLE 2020](http://www.sleconf.org/2020/AcceptedPapers.html), doi:[10.1145/3426425.3426933](https://doi.org/10.1145/3426425.3426933), 15--16 November 2020
-   [Vadim Zaytsev](http://grammarware.net), [*Software Language Engineers' Worst Nightmare*](https://youtu.be/sSkIUTdfDjs), a pre-recorded presentation at SLE@SPLASH, 13 November 2020
-   [Bernd Fischer](http://www.cs.sun.ac.za/~bfischer/index.html), *Breaking Parsers: Mutation-based Generation of Programs with Guaranteed Syntax Errors*, IFIP TC-2 WG 2.11 on [Program Generation](https://wiki.hh.se/wg211/index.php/WG211/M20Schedule), invited/impromptu presentation, Sorbonne Université, 20 February 2020 (first implementation of BabyCobol in Prolog capable of generating hundreds of BabyCobol programs)
-   [Vadim Zaytsev](http://grammarware.net), *BabyCobol: The Challenge to Program Generation Tool Developers*, IFIP TC-2 WG 2.11 on [Program Generation](https://wiki.hh.se/wg211/index.php/WG211/M20Zaytsev), invited presentation, Sorbonne Université, 17 February 2020
-   [Vadim Zaytsev](http://grammarware.net), *Legacy and Software Renovation*, [Software Evolution](https://datanose.nl/#course%5B77550%5D#course%5B77550%5D) guest lecture, Universiteit van Amsterdam, 9 December 2019
-   [Vadim Zaytsev](http://grammarware.net), *Blind Men and a Room Full of Elephants*, [BENEVOL 2019](http://soft.vub.ac.be/benevol2019/#secondKeynote#secondKeynote) keynote, Vrije Universiteit Brussel (VUB), 28 November 2019

## Code pearls

-   [FizzBuzz](code/fizzbuzz.html)
-   [99 Bottles of Beer](code/99-bottles.html)
-   [100 Doors](code/100-doors.html)
-   [Interplay between PERFORM THROUGH and GO TO](code/perform-goto.html)

------------------------------------------------------------------------
loop.md
# [[BabyCobol](index.html)]{.ff .lang}: [[LOOP](loop.html)]{.ff .used}

a composite statement for structural execution of a sequence of commands

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMTAwcHgiIGhlaWdodD0iMjAwcHgiPgogICAgPGRlZnM+CiAgICAgICAgPHN0eWxlIHR5cGU9InRleHQvY3NzIj4KICAgICAgICAgICAgQG5hbWVzcGFjZSAiaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciOwogICAgICAgICAgICBzdmcge2JhY2tncm91bmQtY29sb3I6IHdoaXRlO30KICAgICAgICAgICAgcGF0aCB7ZmlsbDogbm9uZTsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHBvbHlnb24ge2ZpbGw6IGJsYWNrOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgdGV4dCB7Zm9udC1zaXplOjE2cHg7ZmlsbDpibGFjaztmb250LXdlaWdodDpib2xkO2ZvbnQtZmFtaWx5Om1vbm9zcGFjZTt9CiAgICAgICAgICAgIHRleHQuaSB7Zm9udC1zdHlsZTppdGFsaWM7fQogICAgICAgIDwvc3R5bGU+CiAgICA8L2RlZnM+Cjxwb2x5Z29uIHBvaW50cz0iMjggMzAgMjAgMjYgMjAgMzQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjM2IDMwIDI4IDI2IDI4IDM0Ij48L3BvbHlnb24+PHBhdGggZD0iTSAzNiAzMCBoIDIwIiAvPjx0ZXh0IHg9IjU2IiB5PSIzNCI+TE9PUDwvdGV4dD48cGF0aCBkPSJNIDk2IDMwIGggMjAiIC8+PHBhdGggZD0iTSAxMTYgMzAgaCA4ODAiIC8+PHRleHQgeD0iOTk2IiB5PSIzNCI+wqBFTkQ8L3RleHQ+PHBhdGggZD0iTSAxMDM2IDMwIGggMjAiIC8+PHBvbHlnb24gcG9pbnRzPSIxMDY0IDMwIDEwNTYgMjYgMTA1NiAzNCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMTA2NCAzMCAxMDcyIDI2IDEwNzIgMzQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDExNiAzMCB2IDQwIGggMjAiIC8+PHBhdGggZD0iTSAxMzYgNzAgaCAyMCIgLz48cGF0aCBkPSJNIDE1NiA3MCBoIDIwIiAvPjx0ZXh0IHg9IjE3NiIgeT0iNzQiPsKgVkFSWUlORzwvdGV4dD48cGF0aCBkPSJNIDI1NiA3MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMjc2IDcwIGggMTYwIiAvPjxwYXRoIGQ9Ik0gNDM2IDcwIGggMTgwIiAvPjxwYXRoIGQ9Ik0gNjE2IDcwIGggMTYwIiAvPjxwYXRoIGQ9Ik0gNzc2IDcwIGggMTYwIiAvPjxwYXRoIGQ9Ik0gOTM2IDcwIGggMjAiIC8+PHBhdGggZD0iTSA5NTYgNzAgdiAtMjAgaCAtODIwIHYgMjAiIC8+PHBvbHlnb24gcG9pbnRzPSIxMzYgNjkgMTMyIDYxIDE0MCA2MSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gOTU2IDcwIGggMjAgdiAtNDAiIC8+PHBvbHlnb24gcG9pbnRzPSI5NzYgMzEgOTcyIDM5IDk4MCAzOSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMjc2IDcwIHYgMjAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMjk2IiB5PSI5NCI+SWRlbnRpZmllcjwvdGV4dD48cGF0aCBkPSJNIDM5NiA5MCBoIDIwIHYgLTIwIiAvPjxwb2x5Z29uIHBvaW50cz0iNDE2IDcxIDQxMiA3OSA0MjAgNzkiPjwvcG9seWdvbj48cGF0aCBkPSJNIDQzNiA3MCB2IDIwIGggMjAiIC8+PHRleHQgeD0iNDU2IiB5PSI5NCI+RlJPTTwvdGV4dD48cGF0aCBkPSJNIDQ5NiA5MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSI1MTYiIHk9Ijk0Ij5BdG9taWM8L3RleHQ+PHBhdGggZD0iTSA1NzYgOTAgaCAyMCB2IC0yMCIgLz48cG9seWdvbiBwb2ludHM9IjU5NiA3MSA1OTIgNzkgNjAwIDc5Ij48L3BvbHlnb24+PHBhdGggZD0iTSA2MTYgNzAgdiAyMCBoIDIwIiAvPjx0ZXh0IHg9IjYzNiIgeT0iOTQiPlRPPC90ZXh0PjxwYXRoIGQ9Ik0gNjU2IDkwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjY3NiIgeT0iOTQiPkF0b21pYzwvdGV4dD48cGF0aCBkPSJNIDczNiA5MCBoIDIwIHYgLTIwIiAvPjxwb2x5Z29uIHBvaW50cz0iNzU2IDcxIDc1MiA3OSA3NjAgNzkiPjwvcG9seWdvbj48cGF0aCBkPSJNIDc3NiA3MCB2IDIwIGggMjAiIC8+PHRleHQgeD0iNzk2IiB5PSI5NCI+Qlk8L3RleHQ+PHBhdGggZD0iTSA4MTYgOTAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iODM2IiB5PSI5NCI+QXRvbWljPC90ZXh0PjxwYXRoIGQ9Ik0gODk2IDkwIGggMjAgdiAtMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI5MTYgNzEgOTEyIDc5IDkyMCA3OSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMTU2IDcwIHYgNjAgaCAyMCIgLz48dGV4dCB4PSIxNzYiIHk9IjEzNCI+wqBXSElMRTwvdGV4dD48cGF0aCBkPSJNIDIzNiAxMzAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMjU2IiB5PSIxMzQiPsKgQm9vbGVhbkV4cHJlc3Npb248L3RleHQ+PHBhdGggZD0iTSA0MzYgMTMwIGggNTAwIiAvPjxwYXRoIGQ9Ik0gMTU2IDcwIHYgODAgaCAyMCIgLz48dGV4dCB4PSIxNzYiIHk9IjE1NCI+wqBVTlRJTDwvdGV4dD48cGF0aCBkPSJNIDIzNiAxNTAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMjU2IiB5PSIxNTQiPsKgQm9vbGVhbkV4cHJlc3Npb248L3RleHQ+PHBhdGggZD0iTSA0MzYgMTUwIGggNTAwIiAvPjxwYXRoIGQ9Ik0gMTU2IDcwIHYgMTAwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjE3NiIgeT0iMTc0Ij7CoFN0YXRlbWVudDwvdGV4dD48cGF0aCBkPSJNIDI3NiAxNzAgaCA2NDAiIC8+PHBhdGggZD0iTSA5MTYgMTcwIGggMjAgdiAtMTAwIiAvPjxwb2x5Z29uIHBvaW50cz0iOTM2IDcxIDkzMiA3OSA5NDAgNzkiPjwvcG9seWdvbj4KPC9zdmc+)

## Remarks

-   the `VARYING` clause can only refer to a numeric field
-   if the `VARYING` clause is present but the `FROM` clause is missing, then the initial value is 1
-   if the `VARYING` clause is present but the `TO` clause is missing, then the final value is the highest possible for the varied field\'s type
-   if the `VARYING` clause is present but the `BY` clause is missing, then the looping step is 1
-   Essentially, the hard part of this statement stems from the fact that, unlike in modern languages where the loop construct has a clearly defined header and footer, its metainformation can be scattered across the body. This means that while parsing, being inside the loop intrinsically broadens the choice of possible statements by three.

## Origins

[[AppBuilder](appbuilder.html)]{.ff .lang}   [[DO](appbuilder.html "a multi-purpose loop statement with a number of detachable clauses")]{.ff .used}\
[[CLIST](clist.html)]{.ff .lang}   [[DO](clist.html "execute a sequence of commands repeatedly")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[PERFORM (in-line)](cobol.html "execute a list of statements repeatedly")]{.ff .used}\
[[FORTRAN](fortran.html)]{.ff .lang}   [[DO](fortran.html "execute a statement repeatedly")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[DO](pli.html "repeat a group of statements under certain conditions")]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[DO](rexx.html "execute a group of instructions repeatedly")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[DO](rpg.html "execute a group of operations several times")]{.ff .used} [[DOU](rpg.html "execute a group of operations until a condition is met")]{.ff .used} [[DOW](rpg.html "execute a group of operations while a condition holds")]{.ff .used} [[FOR](rpg.html "iterate over a group of operations")]{.ff .used}\

------------------------------------------------------------------------
move.md
# [[BabyCobol](index.html)]{.ff .lang}: [[MOVE](move.html)]{.ff .used}

a partial structural assignment statement

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI1MjBweCIgaGVpZ2h0PSIxMjBweCI+CiAgICA8ZGVmcz4KICAgICAgICA8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgICAgICAgICBAbmFtZXNwYWNlICJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI7CiAgICAgICAgICAgIHN2ZyB7YmFja2dyb3VuZC1jb2xvcjogd2hpdGU7fQogICAgICAgICAgICBwYXRoIHtmaWxsOiBub25lOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgcG9seWdvbiB7ZmlsbDogYmxhY2s7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICB0ZXh0IHtmb250LXNpemU6MTZweDtmaWxsOmJsYWNrO2ZvbnQtd2VpZ2h0OmJvbGQ7Zm9udC1mYW1pbHk6bW9ub3NwYWNlO30KICAgICAgICAgICAgdGV4dC5pIHtmb250LXN0eWxlOml0YWxpYzt9CiAgICAgICAgPC9zdHlsZT4KICAgIDwvZGVmcz4KPHBvbHlnb24gcG9pbnRzPSIyOCA0MCAyMCAzNiAyMCA0NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMzYgNDAgMjggMzYgMjggNDQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM2IDQwIGggMjAiIC8+PHRleHQgeD0iNTYiIHk9IjQ0Ij5NT1ZFPC90ZXh0PjxwYXRoIGQ9Ik0gOTYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDExNiA0MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIxMzYiIHk9IjQ0Ij5BdG9taWM8L3RleHQ+PHBhdGggZD0iTSAxOTYgNDAgaCAxMDAiIC8+PHRleHQgeD0iMjk2IiB5PSI0NCI+VE88L3RleHQ+PHBhdGggZD0iTSAzMTYgNDAgaCA0MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMzU2IiB5PSI0NCI+SWRlbnRpZmllcjwvdGV4dD48cGF0aCBkPSJNIDQ1NiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNDc2IDQwIHYgLTIwIGggLTE0MCB2IDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMzM2IDM5IDMzMiAzMSAzNDAgMzEiPjwvcG9seWdvbj48cGF0aCBkPSJNIDQ3NiA0MCBoIDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iNTA0IDQwIDQ5NiAzNiA0OTYgNDQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjUwNCA0MCA1MTIgMzYgNTEyIDQ0Ij48L3BvbHlnb24+PHBhdGggZD0iTSAxMTYgNDAgdiAyMCBoIDIwIiAvPjx0ZXh0IHg9IjEzNiIgeT0iNjQiPsKgSElHSC1WQUxVRVM8L3RleHQ+PHBhdGggZD0iTSAyNTYgNjAgaCAyMCIgLz48cGF0aCBkPSJNIDExNiA0MCB2IDQwIGggMjAiIC8+PHRleHQgeD0iMTM2IiB5PSI4NCI+TE9XLVZBTFVFUzwvdGV4dD48cGF0aCBkPSJNIDIzNiA4MCBoIDQwIiAvPjxwYXRoIGQ9Ik0gMTE2IDQwIHYgNjAgaCAyMCIgLz48dGV4dCB4PSIxMzYiIHk9IjEwNCI+U1BBQ0VTPC90ZXh0PjxwYXRoIGQ9Ik0gMTk2IDEwMCBoIDYwIiAvPjxwYXRoIGQ9Ik0gMjU2IDEwMCBoIDIwIHYgLTYwIiAvPjxwb2x5Z29uIHBvaW50cz0iMjc2IDQxIDI3MiA0OSAyODAgNDkiPjwvcG9seWdvbj4KPC9zdmc+)

## Remarks

-   for numeric fields (defined with picture clauses without `A` nor `X`), BabyCobol\'s `MOVE SPACES` behaves like COBOL\'s `MOVE ZEROES`
-   if the first argument is an identifier, BabyCobol\'s `MOVE` behaves like COBOL\'s `MOVE CORRESPONDING`
-   if the first argument is a figurative value, it can lead to assigning vastly different values to second arguments depending on their individual types
-   the first argument is evaluated once, before any assignments take place; all other arguments are evaluated in order, taking into account previous assignments

## Origins

[[AppBuilder](appbuilder.html)]{.ff .lang}   [[CLEAR](appbuilder.html "return a field or a view to its freshly initialised state")]{.ff .used} [[MAP](appbuilder.html "name-based deep structural asssignment")]{.ff .used} [[SET](appbuilder.html "an alternative assignment statement")]{.ff .used}\
[[CLIST](clist.html)]{.ff .lang}   [[SET](clist.html "assign a value to a variable")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[MOVE](cobol.html "transfer data from one storage area to another")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[EVAL-CORR](rpg.html "evaluate an expression and assign corresponding subfields")]{.ff .used} [[MOVE](rpg.html "move character data")]{.ff .used} [[MOVEA](rpg.html "move data between arrays")]{.ff .used} [[MOVEL](rpg.html "move character data starting from the leftmost one")]{.ff .used}\

------------------------------------------------------------------------
multiply.md
# [[BabyCobol](index.html)]{.ff .lang}: [[MULTIPLY](multiply.html)]{.ff .used}

a statement to multiply two or more numeric values

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI2NjBweCIgaGVpZ2h0PSI4MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDQwIDIwIDM2IDIwIDQ0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiA0MCAyOCAzNiAyOCA0NCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgNDAgaCAyMCIgLz48dGV4dCB4PSI1NiIgeT0iNDQiPk1VTFRJUExZPC90ZXh0PjxwYXRoIGQ9Ik0gMTM2IDQwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjE1NiIgeT0iNDQiPkF0b21pYzwvdGV4dD48cGF0aCBkPSJNIDIxNiA0MCBoIDIwIiAvPjx0ZXh0IHg9IjIzNiIgeT0iNDQiPkJZPC90ZXh0PjxwYXRoIGQ9Ik0gMjU2IDQwIGggNDAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjI5NiIgeT0iNDQiPkF0b21pYzwvdGV4dD48cGF0aCBkPSJNIDM1NiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMzc2IDQwIHYgLTIwIGggLTEwMCB2IDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMjc2IDM5IDI3MiAzMSAyODAgMzEiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM3NiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMzk2IDQwIGggMjQwIiAvPjxwb2x5Z29uIHBvaW50cz0iNjQ0IDQwIDYzNiAzNiA2MzYgNDQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjY0NCA0MCA2NTIgMzYgNjUyIDQ0Ij48L3BvbHlnb24+PHBhdGggZD0iTSAzOTYgNDAgdiAyMCBoIDIwIiAvPjx0ZXh0IHg9IjQxNiIgeT0iNjQiPkdJVklORzwvdGV4dD48cGF0aCBkPSJNIDQ3NiA2MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSI0OTYiIHk9IjY0Ij5JZGVudGlmaWVyPC90ZXh0PjxwYXRoIGQ9Ik0gNTk2IDYwIGggMjAgdiAtMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI2MTYgNDEgNjEyIDQ5IDYyMCA0OSI+PC9wb2x5Z29uPgo8L3N2Zz4=)

## Remarks

-   multiplies each of the second arguments by the first argument and overwrites the old value of each second argument, unless the third argument specifies a different storage place
-   so, for example, `MULTIPLY 2 BY X Y Z` will double the values of `X`, `Y` and `Z`
-   all three arguments obey the rules of sufficient qualification
-   the first argument can be a literal
-   if the second argument is a literal, the third argument is mandatory
-   any of the three arguments can be an identifier defined with a numeric picture clause (free from `A` and `X`)

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[MULTIPLY](cobol.html "multiply numeric values")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[MULT](rpg.html "multiply two numbers")]{.ff .used}\

------------------------------------------------------------------------
nextsentence.md
# [[BabyCobol](index.html)]{.ff .lang}: [[NEXT SENTENCE](nextsentence.html)]{.ff .used}

a statement to branch unconditionally to the beginning of the next sentence

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNjBweCIgaGVpZ2h0PSI2MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDMwIDIwIDI2IDIwIDM0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiAzMCAyOCAyNiAyOCAzNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgMzAgaCAyMCIgLz48dGV4dCB4PSI1NiIgeT0iMzQiPsKgTkVYVCBTRU5URU5DRTwvdGV4dD48cGF0aCBkPSJNIDE5NiAzMCBoIDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMjI0IDMwIDIxNiAyNiAyMTYgMzQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjIyNCAzMCAyMzIgMjYgMjMyIDM0Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Semantics

The problems of implementing the NEXT SENTENCE statement come from the clash between the hierarchical tree-like structure of the program and its flat textual structure. The official semantics of the NEXT SENTENCE statement is that it transfers control to the first statement of the next [sentence](proceduredivision.html) (after the dot). If the AST already incorporates the information about sentences in addition to statements, then implementing the NEXT SENTENCE statement should not be too difficult: we need to prepare a possibly useful target label at the beginning of each sentence, and use it when needed. Due to BabyCobol\'s simplified design, this is the only possible way in the language to essentially [`GO TO`](goto.html) within a paragraph. Watch out for the interaction of NEXT SENTENCE with the other BabyCobol features:

-   `NEXT SENTENCE` as the last statement of the paragraph does nothing, because it has to end with a dot (as each paragraph consists of only full sentences!), so it ends a sentence itself and transfers control to the sentence after that.
-   If the transfer takes the execution away from the current context of [`LOOP`](loop.html), it cleans up call stack contents, temporary variables and other local effects of the context(s) being left behind.

## Style

-   `NEXT SENTENCE` at the beginning of the paragraph can be used to comment out one of more of the immediately following statements, making them dead code without marking each line as an explicit comment.
-   `NEXT SENTENCE.` (with a dot) does nothing, since it transfers control to the next sentence that was about to be executed anyway. As such, it can be used in any place where a sentence is expected but no action is needed, similar to the role `EXIT` plays in [COBOL](cobol.html), or comparable "no operation" statements in other languages.

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[CONTINUE](cobol.html "do nothing")]{.ff .used} [[EXIT](cobol.html "do nothing")]{.ff .used} [[NEXT SENTENCE](cobol.html "a special clause of the IF statement to transfer control to the statement after the next dot")]{.ff .used}\
[[FORTRAN](fortran.html)]{.ff .lang}   [[CONTINUE](fortran.html "do nothing")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[;](pli.html)]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[NOP](rexx.html "do nothing")]{.ff .used}\

------------------------------------------------------------------------
perform.md
# [[BabyCobol](index.html)]{.ff .lang}: [[PERFORM](perform.html)]{.ff .used}

a statement to call a paragraph or section within the program

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI4NDBweCIgaGVpZ2h0PSI2MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDIwIDIwIDE2IDIwIDI0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiAyMCAyOCAxNiAyOCAyNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgMjAgaCAyMCIgLz48dGV4dCB4PSI1NiIgeT0iMjQiPsKgUEVSRk9STTwvdGV4dD48cGF0aCBkPSJNIDEzNiAyMCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIxNTYiIHk9IjI0Ij7CoFByb2NlZHVyZU5hbWU8L3RleHQ+PHBhdGggZD0iTSAyOTYgMjAgaCAyMCIgLz48cGF0aCBkPSJNIDMxNiAyMCBoIDMwMCIgLz48cGF0aCBkPSJNIDYxNiAyMCBoIDIwMCIgLz48cG9seWdvbiBwb2ludHM9IjgyNCAyMCA4MTYgMTYgODE2IDI0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSI4MjQgMjAgODMyIDE2IDgzMiAyNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzE2IDIwIHYgMjAgaCAyMCIgLz48dGV4dCB4PSIzMzYiIHk9IjQ0Ij7CoFRIUk9VR0g8L3RleHQ+PHBhdGggZD0iTSA0MTYgNDAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNDM2IiB5PSI0NCI+wqBQcm9jZWR1cmVOYW1lPC90ZXh0PjxwYXRoIGQ9Ik0gNTc2IDQwIGggMjAgdiAtMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI1OTYgMjEgNTkyIDI5IDYwMCAyOSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNjE2IDIwIHYgMjAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iNjM2IiB5PSI0NCI+QXRvbWljPC90ZXh0PjxwYXRoIGQ9Ik0gNjk2IDQwIGggMjAiIC8+PHRleHQgeD0iNzE2IiB5PSI0NCI+wqBUSU1FUzwvdGV4dD48cGF0aCBkPSJNIDc3NiA0MCBoIDIwIHYgLTIwIiAvPjxwb2x5Z29uIHBvaW50cz0iNzk2IDIxIDc5MiAyOSA4MDAgMjkiPjwvcG9seWdvbj4KPC9zdmc+)

## Remarks

See a [separate example](code/perform-goto.html) showcasing the interplay between `PERFORM` and [`GO TO`](goto.html).

## Origins

[[AppBuilder](appbuilder.html)]{.ff .lang}   [[PERFORM](appbuilder.html "call a local procedure")]{.ff .used}\
[[CLIST](clist.html)]{.ff .lang}   [[SYSCALL](clist.html "call a local procedure")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[PERFORM (out-of-line)](cobol.html "call a local procedure")]{.ff .used}\
[[FORTRAN](fortran.html)]{.ff .lang}   [[CALL](fortran.html "invoke a subroutine")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[CALL](pli.html "invoke a local subroutine")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[CAS](rpg.html "conditionally invoke subroutine")]{.ff .used} [[EXSR](rpg.html "invoke a local subroutine")]{.ff .used}\

------------------------------------------------------------------------
pli.md
# [[BabyCobol](index.html)]{.ff .lang}: The [[PL/I](pli.html)]{.ff .lang} Origins

------------------------------------------------------------------------

## Features:

[[%INCLUDE](copy.html "incorporate external text into the source program")]{.ff .used} [%PROCESS]{.ff title="override compiler options"} [%PUSH]{.ff title="save the %PRINT status in a pushdown stack"} [%SKIP]{.ff title=""} [%XINCLUDE]{.ff title=""} [[;](nextsentence.html)]{.ff .used} [ALLOCATE]{.ff title="allocate storage for controlled variables"} [ATTACH]{.ff title="create a new thread"} [BEGIN]{.ff title="start a block of statements"} [[CALL](perform.html "invoke a local subroutine")]{.ff .used} [CLOSE]{.ff title="dissociate a file from its data set"} [[DECLARE](datadivision.html "declare a variable and specify its attributes")]{.ff .used} [DEFAULT]{.ff title="specify data attribute defaults"} [[DEFINE](datadivision.html "define a structure or an alias for the data type")]{.ff .used} [DELAY]{.ff title="suspend execution for some time"} [DELETE]{.ff title="delete a record from an update file"} [DETACH]{.ff title="free system resources"} [[DISPLAY](display.html "display a message on user's screen")]{.ff .used} [[DO](loop.html "repeat a group of statements under certain conditions")]{.ff .used} [[END](https://slebok.github.io/baby/end.html "end one or more blocks or groups of statements")]{.ff .used} [ENTRY]{.ff title="specify a secondary entry point to a procedure"} [[EXIT](stop.html "stop the current thread")]{.ff .used} [FETCH]{.ff title="check the main storage for named procedures"} [FLUSH]{.ff title="flush buffers associated with an output file"} [FORMAT]{.ff title="specify a format list to be used in stream data transmissions"} [FREE]{.ff title="free the storage allocated for based and controlled variables"} [GET]{.ff title="assign parts of the stream data transmission to variables"} [[GO TO](goto.html "transfer control to a labelled statement")]{.ff .used} [[IF](if.html "branch conditionally within a program")]{.ff .used} [ITERATE]{.ff title="continue a fresh iteration in a loop"} [LEAVE]{.ff title="break from a loop"} [LOCATE]{.ff title="allocate storage within an output buffer"} [[ON](signal.html "define a handler for an established condition")]{.ff .used} [OPEN]{.ff title="associate a file with a data set"} [OPTIONS]{.ff title="annotate a package, procedure, entry or a statement block with metadata"} [[OTHERWISE](evaluate.html)]{.ff .used} [PACKAGE]{.ff title="define a package"} [PROCEDURE]{.ff title="define a local callable procedure"} [PUT]{.ff title="transmit values of local variables into the stream of data"} [READ]{.ff title=""} [RELEASE]{.ff title=""} [RESIGNAL]{.ff title="terminate an ON-unit and start a new one"} [RETURN]{.ff title="terminates execution of the subroutine or function procedure and returns control"} [REVERT]{.ff title="terminate an ON-unit"} [REWRITE]{.ff title=""} [SELECT]{.ff title=""} [SIGNAL]{.ff title="verify the action of an ON-unit"} [[STOP](stop.html "stops the current application")]{.ff .used} [WAIT]{.ff title=""} [[WHEN](evaluate.html)]{.ff .used} [WRITE]{.ff title=""}

------------------------------------------------------------------------
proceduredivision.md
# [[BabyCobol](index.html)]{.ff .lang}: [[PROCEDURE DIVISION](proceduredivision.html)]{.ff .used}

the code-containing division in a program

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI5NDBweCIgaGVpZ2h0PSIzNDBweCI+CiAgICA8ZGVmcz4KICAgICAgICA8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgICAgICAgICBAbmFtZXNwYWNlICJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI7CiAgICAgICAgICAgIHN2ZyB7YmFja2dyb3VuZC1jb2xvcjogd2hpdGU7fQogICAgICAgICAgICBwYXRoIHtmaWxsOiBub25lOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgcG9seWdvbiB7ZmlsbDogYmxhY2s7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICB0ZXh0IHtmb250LXNpemU6MTZweDtmaWxsOmJsYWNrO2ZvbnQtd2VpZ2h0OmJvbGQ7Zm9udC1mYW1pbHk6bW9ub3NwYWNlO30KICAgICAgICAgICAgdGV4dC5pIHtmb250LXN0eWxlOml0YWxpYzt9CiAgICAgICAgPC9zdHlsZT4KICAgIDwvZGVmcz4KPHRleHQgY2xhc3M9ImkiIHg9IjIwIiB5PSIzNCI+RGl2aXNpb248L3RleHQ+PHBvbHlnb24gcG9pbnRzPSIxMDggMzAgMTAwIDI2IDEwMCAzNCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMTE2IDMwIDEwOCAyNiAxMDggMzQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDExNiAzMCBoIDIwIiAvPjx0ZXh0IHg9IjEzNiIgeT0iMzQiPlBST0NFRFVSRSBESVZJU0lPTjwvdGV4dD48cGF0aCBkPSJNIDMxNiAzMCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMzM2IDMwIGggMTIwIiAvPjx0ZXh0IHg9IjQ1NiIgeT0iMzQiPsKgLjwvdGV4dD48cGF0aCBkPSJNIDQ3NiAzMCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNDk2IDMwIGggMjAwIiAvPjxwYXRoIGQ9Ik0gNjk2IDMwIGggMjAwIiAvPjxwb2x5Z29uIHBvaW50cz0iOTA0IDMwIDg5NiAyNiA4OTYgMzQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjkwNCAzMCA5MTIgMjYgOTEyIDM0Ij48L3BvbHlnb24+PHBhdGggZD0iTSA2OTYgMzAgdiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNzE2IDcwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjczNiIgeT0iNzQiPsKgUGFyYWdyYXBoPC90ZXh0PjxwYXRoIGQ9Ik0gODM2IDcwIGggMjAiIC8+PHBhdGggZD0iTSA4NTYgNzAgdiAtMjAgaCAtMTQwIHYgMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI3MTYgNjkgNzEyIDYxIDcyMCA2MSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gODU2IDcwIGggMjAgdiAtNDAiIC8+PHBvbHlnb24gcG9pbnRzPSI4NzYgMzEgODcyIDM5IDg4MCAzOSI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gNDk2IDMwIHYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDUxNiA3MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSI1MzYiIHk9Ijc0Ij5TZW50ZW5jZTwvdGV4dD48cGF0aCBkPSJNIDYxNiA3MCBoIDQwIiAvPjxwYXRoIGQ9Ik0gNjU2IDcwIHYgLTIwIGggLTE0MCB2IDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iNTE2IDY5IDUxMiA2MSA1MjAgNjEiPjwvcG9seWdvbj48cGF0aCBkPSJNIDY1NiA3MCBoIDIwIHYgLTQwIiAvPjxwb2x5Z29uIHBvaW50cz0iNjc2IDMxIDY3MiAzOSA2ODAgMzkiPjwvcG9seWdvbj48cGF0aCBkPSJNIDMzNiAzMCB2IDIwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjM1NiIgeT0iNTQiPsKgVXNpbmc8L3RleHQ+PHBhdGggZD0iTSA0MTYgNTAgaCAyMCB2IC0yMCIgLz48cG9seWdvbiBwb2ludHM9IjQzNiAzMSA0MzIgMzkgNDQwIDM5Ij48L3BvbHlnb24+PHRleHQgY2xhc3M9ImkiIHg9IjIwIiB5PSIxMzQiPsKgVXNpbmc8L3RleHQ+PHBvbHlnb24gcG9pbnRzPSI4OCAxMzAgODAgMTI2IDgwIDEzNCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iOTYgMTMwIDg4IDEyNiA4OCAxMzQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDk2IDEzMCBoIDIwIiAvPjx0ZXh0IHg9IjExNiIgeT0iMTM0Ij7CoFVTSU5HPC90ZXh0PjxwYXRoIGQ9Ik0gMTc2IDEzMCBoIDQwIiAvPjxwYXRoIGQ9Ik0gMjE2IDEzMCBoIDIwIiAvPjx0ZXh0IHg9IjIzNiIgeT0iMTM0Ij5CWSBSRUZFUkVOQ0U8L3RleHQ+PHBhdGggZD0iTSAzNTYgMTMwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjM3NiIgeT0iMTM0Ij5JZGVudGlmaWVyPC90ZXh0PjxwYXRoIGQ9Ik0gNDc2IDEzMCBoIDQwIiAvPjxwYXRoIGQ9Ik0gNTE2IDEzMCB2IC0yMCBoIC0zMjAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjE5NiAxMjkgMTkyIDEyMSAyMDAgMTIxIj48L3BvbHlnb24+PHBhdGggZD0iTSA1MTYgMTMwIGggMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI1NDQgMTMwIDUzNiAxMjYgNTM2IDEzNCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iNTQ0IDEzMCA1NTIgMTI2IDU1MiAxMzQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDIxNiAxMzAgdiAyMCBoIDIwIiAvPjx0ZXh0IHg9IjIzNiIgeT0iMTU0Ij5CWSBDT05URU5UPC90ZXh0PjxwYXRoIGQ9Ik0gMzM2IDE1MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIzNTYiIHk9IjE1NCI+QXRvbWljPC90ZXh0PjxwYXRoIGQ9Ik0gNDE2IDE1MCBoIDgwIiAvPjxwYXRoIGQ9Ik0gMjE2IDEzMCB2IDQwIGggMjAiIC8+PHRleHQgeD0iMjM2IiB5PSIxNzQiPkJZIFZBTFVFPC90ZXh0PjxwYXRoIGQ9Ik0gMzE2IDE3MCBoIDIwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIzMzYiIHk9IjE3NCI+QXRvbWljPC90ZXh0PjxwYXRoIGQ9Ik0gMzk2IDE3MCBoIDgwIiAvPjxwYXRoIGQ9Ik0gNDc2IDE3MCBoIDIwIHYgLTQwIiAvPjxwb2x5Z29uIHBvaW50cz0iNDk2IDEzMSA0OTIgMTM5IDUwMCAxMzkiPjwvcG9seWdvbj48dGV4dCBjbGFzcz0iaSIgeD0iMjAiIHk9IjIzNCI+wqBQYXJhZ3JhcGg8L3RleHQ+PHBvbHlnb24gcG9pbnRzPSIxMjggMjMwIDEyMCAyMjYgMTIwIDIzNCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMTM2IDIzMCAxMjggMjI2IDEyOCAyMzQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDEzNiAyMzAgaCAyMCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMTU2IiB5PSIyMzQiPklkZW50aWZpZXI8L3RleHQ+PHBhdGggZD0iTSAyNTYgMjMwIGggMjAiIC8+PHRleHQgeD0iMjc2IiB5PSIyMzQiPsKgLjwvdGV4dD48cGF0aCBkPSJNIDI5NiAyMzAgaCA0MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMzM2IiB5PSIyMzQiPlNlbnRlbmNlPC90ZXh0PjxwYXRoIGQ9Ik0gNDE2IDIzMCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNDM2IDIzMCB2IC0yMCBoIC0xMjAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjMxNiAyMjkgMzEyIDIyMSAzMjAgMjIxIj48L3BvbHlnb24+PHBhdGggZD0iTSA0MzYgMjMwIGggMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI0NjQgMjMwIDQ1NiAyMjYgNDU2IDIzNCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iNDY0IDIzMCA0NzIgMjI2IDQ3MiAyMzQiPjwvcG9seWdvbj48dGV4dCBjbGFzcz0iaSIgeD0iMjAiIHk9IjI5NCI+U2VudGVuY2U8L3RleHQ+PHBvbHlnb24gcG9pbnRzPSIxMDggMjkwIDEwMCAyODYgMTAwIDI5NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMTE2IDI5MCAxMDggMjg2IDEwOCAyOTQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDExNiAyOTAgaCA0MCIgLz48dGV4dCBjbGFzcz0iaSIgeD0iMTU2IiB5PSIyOTQiPsKgU3RhdGVtZW50PC90ZXh0PjxwYXRoIGQ9Ik0gMjU2IDI5MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMjc2IDI5MCB2IC0yMCBoIC0xNDAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjEzNiAyODkgMTMyIDI4MSAxNDAgMjgxIj48L3BvbHlnb24+PHBhdGggZD0iTSAyNzYgMjkwIGggMjAiIC8+PHRleHQgeD0iMjk2IiB5PSIyOTQiPsKgLjwvdGV4dD48cGF0aCBkPSJNIDMxNiAyOTAgaCAyMCIgLz48cG9seWdvbiBwb2ludHM9IjM0NCAyOTAgMzM2IDI4NiAzMzYgMjk0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNDQgMjkwIDM1MiAyODYgMzUyIDI5NCI+PC9wb2x5Z29uPgo8L3N2Zz4=)

## Remarks

-   the procedure division follows either the [`DATA DIVISION`](datadivision.html), if it was present, or the [`IDENTIFICATION DIVISION`](identificationdivision.html), and is the last division in a BabyCobol program
-   the *`Using`* clauses of the procedure division are used with the [`CALL`](call.html) statement
-   the procedure division consists of paragraphs, the first of which can essentially remain unnamed
-   each paragraph has a name that can be used in [`ALTER`](alter.html), [`GO TO`](goto.html) and [`PERFORM`](perform.html) statements, and a non-empty list of sentences
-   each sentence is a list of statements terminated by a dot
-   each statement can be [`ACCEPT`](accept.html), [`ADD`](add.html), [`ALTER`](alter.html), [`CALL`](call.html), [`COPY`](copy.html), [`DISPLAY`](display.html), [`DIVIDE`](divide.html), [`EVALUATE`](evaluate.html), [`GO TO`](goto.html), [`IF`](if.html), [`LOOP`](loop.html), [`MOVE`](move.html), [`MULTIPLY`](multiply.html), [`NEXT SENTENCE`](nextsentence.html), [`PERFORM`](perform.html), [`SIGNAL`](signal.html), [`STOP`](stop.html) or [`SUBTRACT`](subtract.html)
-   execution of statements proceeds sequentially from the start of the division downwards, overstepping boundaries of sentences and paragraphs, until either the end of the file is reached or a [`STOP`](stop.html) statement is encountered

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[PROCEDURE DIVISION](cobol.html "the top level program unit with executable code")]{.ff .used}\

------------------------------------------------------------------------
rexx.md
# [[BabyCobol](index.html)]{.ff .lang}: The [[REXX](rexx.html)]{.ff .lang} Origins

------------------------------------------------------------------------

## Features:

[[ADDRESS](alter.html "change the destination of commands")]{.ff .used} [ARG]{.ff title="syntactic sugar for PARSE UPPER ARG"} [[CALL](call.html "call a built-in or external function")]{.ff .used} [[CALL OFF](signal.html "cancel an established error handler")]{.ff .used} [[CALL ON](signal.html "change an error handler")]{.ff .used} [[DO](loop.html "execute a group of instructions repeatedly")]{.ff .used} [DROP]{.ff title="unassign a named variable"} [[EXIT](stop.html "terminate a program")]{.ff .used} [[IF](if.html "branch conditionally within a program")]{.ff .used} [INTERPRET]{.ff title="execute an expression as code"} [ITERATE]{.ff title="go into the next iteration of the innermost or named DO loop"} [LEAVE]{.ff title="terminate the innermost or named DO loop"} [[NOP](nextsentence.html "do nothing")]{.ff .used} [NUMERIC]{.ff title="specify numeric conventions: significant digits, notation format, etc"} [OPTIONS]{.ff title="change DBCS strings treatment"} [PARSE]{.ff title="parsing in a broad sense"} [PARSE SOURCE]{.ff title="read files and access other sources"} [PROCEDURE]{.ff title="protect variables by making them local (otherwise all are global)"} [[PULL](accept.html "syntactic sugar for PARSE UPPER PULL")]{.ff .used} [PUSH]{.ff title="LIFO output"} [QUEUE]{.ff title="FIFO output"} [[RETURN](stop.html "terminates a procedure or the program if no procedure is active")]{.ff .used} [[SAY](display.html "display a line on the terminal")]{.ff .used} [[SELECT](evaluate.html "branch to multiple locations based on several conditions")]{.ff .used} [[SIGNAL](goto.html "branch unconditionally within a program")]{.ff .used} [[SIGNAL OFF](signal.html "cancel an established error handler")]{.ff .used} [[SIGNAL ON](signal.html "define a handler for a specific condition")]{.ff .used} [TRACE]{.ff title="perform debugging actions"} [UPPER]{.ff title="makes contents of a variable uppercase"}

## Sources:

-   IBM SA32-0982-00, z/OS TSO/E REXX User\'s Guide Version 2 Release 1, 1988--2013

------------------------------------------------------------------------
rpg.md
# [[BabyCobol](index.html)]{.ff .lang}: The [[RPG](rpg.html)]{.ff .lang} Origins

------------------------------------------------------------------------

## Features:

[ACQ]{.ff title="acquire a program device"} [[ADD](add.html "add two numbers together")]{.ff .used} [ADDDUR]{.ff title="add duration to a date/time/timestamp"} [ALLOC]{.ff title="allocate storage in the heap"} [AND]{.ff title="combine two conditions with a conjunction"} [BEGSR]{.ff title="begin a subrouting"} [BITOFF]{.ff title="assign some bits to zeroes"} [BITON]{.ff title="assign some bits to ones"} [[CAB](if.html "branch conditionally within a program")]{.ff .used} [CALL]{.ff title="transfer control to another program"} [CALLB]{.ff title="transfer control to a bound procedure"} [CALLP]{.ff title="transfer control to a prototyped procedure or program"} [[CAS](perform.html "conditionally invoke subroutine")]{.ff .used} [CAT]{.ff title="concatenate two strings"} [CHAIN]{.ff title="fetch a record from a file"} [CHECK]{.ff title="verify that a string contains only allowed characters"} [CHECKR]{.ff title="verify that a string contains only allowed characters, in reverse"} [CLEAR]{.ff title="assign default values to a structure"} [CLOSE]{.ff title="close one or more files or devices and disconnects them"} [COMMIT]{.ff title="commit to scheduled changes"} [COMP]{.ff title="compare two values and set appropriate indicators"} [[DATA-INTO](accept.html "parse a document into a variable")]{.ff .used} [DEALLOC]{.ff title="free one allocation of heap storage"} [DEFINE]{.ff title="define a data field"} [DELETE]{.ff title="delete a record from a database file"} [[DIV](divide.html "divide a number by another number")]{.ff .used} [[DO](loop.html "execute a group of operations several times")]{.ff .used} [[DOU](loop.html "execute a group of operations until a condition is met")]{.ff .used} [[DOW](loop.html "execute a group of operations while a condition holds")]{.ff .used} [[DSPLY](display.html "display a message")]{.ff .used} [DUMP]{.ff title="provide a full dump of the executed module"} [[ELSE](if.html "the optional part of an IF or IFxx operator")]{.ff .used} [[ELSEIF](if.html "a combination of ELSE and IF")]{.ff .used} [[END](https://slebok.github.io/baby/end.html "a family of operations ending a CASxx, DO, DOU, DOUxx, DOW, DOWxx, FOR, IF, IFxx or SELECT operation")]{.ff .used} [EVAL]{.ff title="evaluate an expression"} [[EVAL-CORR](move.html "evaluate an expression and assign corresponding subfields")]{.ff .used} [EVALR]{.ff title="evaluate an expression and place is right-adjusted into the result"} [EXCEPT]{.ff title="write one or more records during detail calculations or total calculations"} [EXFMT]{.ff title="a combination of WRITE and READ"} [[EXSR](perform.html "invoke a local subroutine")]{.ff .used} [EXTRCT]{.ff title="extract date or time details from a timestamp"} [FEOD]{.ff title="signal the forced end of data"} [[FOR](loop.html "iterate over a group of operations")]{.ff .used} [FORCE]{.ff title="select from which file will the next read happen"} [[GOTO](goto.html "branch unconditionally within a program")]{.ff .used} [IF]{.ff title="conditionally execute a series of operations"} [IN]{.ff title="retrieve a data area"} [ITER]{.ff title="forces an outer loop to proceed to the next iteration"} [KFLD]{.ff title="designate a field as a part of a search argument"} [KLIST]{.ff title="create a named search argument from a list of fields"} [LEAVE]{.ff title="terminate an outer loop prematurely"} [LEAVESR]{.ff title="exit a subroutine"} [LOOKUP]{.ff title="search for an element in an array or table"} [[MONITOR](signal.html "perform error handling based on the status code")]{.ff .used} [[MOVE](move.html "move character data")]{.ff .used} [[MOVEA](move.html "move data between arrays")]{.ff .used} [[MOVEL](move.html "move character data starting from the leftmost one")]{.ff .used} [[MULT](multiply.html "multiply two numbers")]{.ff .used} [[MVR](divide.html "calculate a reminder of a division")]{.ff .used} [MxxZO]{.ff title="move zone portions of characters"} [NEXT]{.ff title="schedules the next READ/CHAIN/... operation to read from a particular source"} [OCCUR]{.ff title="specify the occurrence of a data structure"} [[ON-ERROR](signal.html "specify which error conditions trigger the handler")]{.ff .used} [ON-EXIT]{.ff title="specify code that is run every time a subroutine is terminated"} [OPEN]{.ff title="open a file for processing"} [OR]{.ff title="combine two conditions with a disjunction"} [[OTHER](evaluate.html "define a branch in a SELECT group to be executed if no WHEN condition matches")]{.ff .used} [OUT]{.ff title="update a data area"} [PARM]{.ff title="define parameters in a parameter list"} [PLIST]{.ff title="identify a parameter list for a CALL/CALLB"} [POST]{.ff title="fill in a file information data structure (INFDS)"} [READ]{.ff title="read a record from a full procedural file"} [READC]{.ff title="read the next changed record from a WORKSTN file"} [READE]{.ff title="read a record from a full procedural file with a known key or EOF otherwise"} [READP]{.ff title="read a prior record from a full procedural file"} [READPE]{.ff title="read the next prior sequential record from a full procedural file with a known key or EOF otherwise"} [REALLOC]{.ff title="reallocate storage with new length"} [REL]{.ff title="release the block on a program device"} [RESET]{.ff title="restore a variable to its default value"} [RETURN]{.ff title="return to caller"} [ROLBK]{.ff title="roll back — eliminates all changes to all files since the last rollback or commit"} [SCAN]{.ff title="find a substring in a string"} [[SELECT](evaluate.html "branch to multiple locations based on several conditions")]{.ff .used} [SETGT]{.ff title="position a file at a record with a key greater than the given value"} [SETLL]{.ff title="position a file at a record with a key greater or equal than the given value"} [SETOFF]{.ff title="set off all indicators from positions 71–76"} [SETON]{.ff title="set on all indicators from positions 71–76"} [SHTDN]{.ff title="check whether the system operator has requested a system shutdown"} [SORTA]{.ff title="sort an array"} [SQRT]{.ff title="calculate square root of expression"} [[SUB](subtract.html "subtract one number from another number")]{.ff .used} [SUBDUR]{.ff title="subtract duration from a date/time/timestamp"} [SUBST]{.ff title="carve a substring from a string"} [TAG]{.ff title="define a label to use with GO TO"} [TEST]{.ff title="test the validity of a date/time/timestamp"} [TESTB]{.ff title="compare operands bitwise"} [TESTN]{.ff title="find zoned decimal digits within a string"} [TESTZ]{.ff title="test the zone of the leftmost character in a string"} [TIME]{.ff title="retrieve date/time/timestamp"} [UNLOCK]{.ff title="release record locks and unlock a data area"} [UPDATE]{.ff title="modify an existing record"} [[WHEN](evaluate.html "define a branch within a SELECT block")]{.ff .used} [WRITE]{.ff title="create new records"} [XFOOT]{.ff title="sum all elements of an array"} [XLATE]{.ff title="translate a string according to a given mapping"} [XML-INTO]{.ff title="parse an XML document into a variable"} [XML-SAX]{.ff title="initiate reactive parsing of an XML document into a variable"} [[Z-ADD](add.html "add a number to a field of zeroes")]{.ff .used} [[Z-SUB](subtract.html "subtract a number from a field of zeroes")]{.ff .used}

## Sources:

-   IBM SC09-2508-09, IBM i Version 7.2, Programming IBM Rational Development Studio for i ILE RPG Reference, 1994--2013

------------------------------------------------------------------------
signal.md
# [[BabyCobol](index.html)]{.ff .lang}: [[SIGNAL](signal.html)]{.ff .used}

an instruction to specify an error handler

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI0ODBweCIgaGVpZ2h0PSI4MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDMwIDIwIDI2IDIwIDM0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiAzMCAyOCAyNiAyOCAzNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgMzAgaCAyMCIgLz48dGV4dCB4PSI1NiIgeT0iMzQiPlNJR05BTDwvdGV4dD48cGF0aCBkPSJNIDExNiAzMCBoIDIwIiAvPjxwYXRoIGQ9Ik0gMTM2IDMwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjE1NiIgeT0iMzQiPsKgUHJvY2VkdXJlTmFtZTwvdGV4dD48cGF0aCBkPSJNIDI5NiAzMCBoIDQwIiAvPjx0ZXh0IHg9IjMzNiIgeT0iMzQiPk9OIEVSUk9SPC90ZXh0PjxwYXRoIGQ9Ik0gNDE2IDMwIGggMjAiIC8+PHBvbHlnb24gcG9pbnRzPSI0NDQgMzAgNDM2IDI2IDQzNiAzNCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iNDQ0IDMwIDQ1MiAyNiA0NTIgMzQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDEzNiAzMCB2IDIwIGggMjAiIC8+PHRleHQgeD0iMTU2IiB5PSI1NCI+wqBPRkY8L3RleHQ+PHBhdGggZD0iTSAxOTYgNTAgaCAxMDAiIC8+PHBhdGggZD0iTSAyOTYgNTAgaCAyMCB2IC0yMCIgLz48cG9seWdvbiBwb2ludHM9IjMxNiAzMSAzMTIgMzkgMzIwIDM5Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Origins

[[CLIST](clist.html)]{.ff .lang}   [[ATTN](clist.html "define a handler for an attention interrupt")]{.ff .used} [[ERROR](clist.html "check for non-zero return codes of other commands")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[USE](cobol.html "a directive to activate debugging or exception handling")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[ON](pli.html "define a handler for an established condition")]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[CALL OFF](rexx.html "cancel an established error handler")]{.ff .used} [[CALL ON](rexx.html "change an error handler")]{.ff .used} [[SIGNAL OFF](rexx.html "cancel an established error handler")]{.ff .used} [[SIGNAL ON](rexx.html "define a handler for a specific condition")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[MONITOR](rpg.html "perform error handling based on the status code")]{.ff .used} [[ON-ERROR](rpg.html "specify which error conditions trigger the handler")]{.ff .used}\

------------------------------------------------------------------------
stop.md
# [[BabyCobol](index.html)]{.ff .lang}: [[STOP](stop.html)]{.ff .used}

an instruction to terminate the program

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxNjBweCIgaGVpZ2h0PSI2MHB4Ij4KICAgIDxkZWZzPgogICAgICAgIDxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+CiAgICAgICAgICAgIEBuYW1lc3BhY2UgImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIjsKICAgICAgICAgICAgc3ZnIHtiYWNrZ3JvdW5kLWNvbG9yOiB3aGl0ZTt9CiAgICAgICAgICAgIHBhdGgge2ZpbGw6IG5vbmU7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICBwb2x5Z29uIHtmaWxsOiBibGFjazsgc3Ryb2tlOiBibGFjazt9CiAgICAgICAgICAgIHRleHQge2ZvbnQtc2l6ZToxNnB4O2ZpbGw6YmxhY2s7Zm9udC13ZWlnaHQ6Ym9sZDtmb250LWZhbWlseTptb25vc3BhY2U7fQogICAgICAgICAgICB0ZXh0Lmkge2ZvbnQtc3R5bGU6aXRhbGljO30KICAgICAgICA8L3N0eWxlPgogICAgPC9kZWZzPgo8cG9seWdvbiBwb2ludHM9IjI4IDMwIDIwIDI2IDIwIDM0Ij48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSIzNiAzMCAyOCAyNiAyOCAzNCI+PC9wb2x5Z29uPjxwYXRoIGQ9Ik0gMzYgMzAgaCAyMCIgLz48dGV4dCB4PSI1NiIgeT0iMzQiPlNUT1A8L3RleHQ+PHBhdGggZD0iTSA5NiAzMCBoIDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMTI0IDMwIDExNiAyNiAxMTYgMzQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjEyNCAzMCAxMzIgMjYgMTMyIDM0Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Origins

[[AppBuilder](appbuilder.html)]{.ff .lang}   [[RETURN](appbuilder.html "terminate the execution of a rule")]{.ff .used}\
[[CLIST](clist.html)]{.ff .lang}   [[EXIT](clist.html "terminate the program")]{.ff .used}\
[[COBOL](cobol.html)]{.ff .lang}   [[EXIT PROGRAM](cobol.html "terminate the execution of a subprogram")]{.ff .used} [[GOBACK](cobol.html "act like an EXIT PROGRAM or like a STOP RUN")]{.ff .used} [[STOP RUN](cobol.html "terminate the program")]{.ff .used}\
[[FORTRAN](fortran.html)]{.ff .lang}   [[RETURN](fortran.html "terminate a subprogram and return control to the calling program unit")]{.ff .used} [[STOP](fortran.html "definitively terminate the program")]{.ff .used}\
[[PL/I](pli.html)]{.ff .lang}   [[EXIT](pli.html "stop the current thread")]{.ff .used} [[STOP](pli.html "stops the current application")]{.ff .used}\
[[REXX](rexx.html)]{.ff .lang}   [[EXIT](rexx.html "terminate a program")]{.ff .used} [[RETURN](rexx.html "terminates a procedure or the program if no procedure is active")]{.ff .used}\

------------------------------------------------------------------------
subtract.md
# [[BabyCobol](index.html)]{.ff .lang}: [[SUBTRACT](subtract.html)]{.ff .used}

a statement to subtract numeric values from other values

------------------------------------------------------------------------

## Format

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI3ODBweCIgaGVpZ2h0PSIxMDBweCI+CiAgICA8ZGVmcz4KICAgICAgICA8c3R5bGUgdHlwZT0idGV4dC9jc3MiPgogICAgICAgICAgICBAbmFtZXNwYWNlICJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI7CiAgICAgICAgICAgIHN2ZyB7YmFja2dyb3VuZC1jb2xvcjogd2hpdGU7fQogICAgICAgICAgICBwYXRoIHtmaWxsOiBub25lOyBzdHJva2U6IGJsYWNrO30KICAgICAgICAgICAgcG9seWdvbiB7ZmlsbDogYmxhY2s7IHN0cm9rZTogYmxhY2s7fQogICAgICAgICAgICB0ZXh0IHtmb250LXNpemU6MTZweDtmaWxsOmJsYWNrO2ZvbnQtd2VpZ2h0OmJvbGQ7Zm9udC1mYW1pbHk6bW9ub3NwYWNlO30KICAgICAgICAgICAgdGV4dC5pIHtmb250LXN0eWxlOml0YWxpYzt9CiAgICAgICAgPC9zdHlsZT4KICAgIDwvZGVmcz4KPHBvbHlnb24gcG9pbnRzPSIyOCA0MCAyMCAzNiAyMCA0NCI+PC9wb2x5Z29uPjxwb2x5Z29uIHBvaW50cz0iMzYgNDAgMjggMzYgMjggNDQiPjwvcG9seWdvbj48cGF0aCBkPSJNIDM2IDQwIGggMjAiIC8+PHRleHQgeD0iNTYiIHk9IjQ0Ij5TVUJUUkFDVDwvdGV4dD48cGF0aCBkPSJNIDEzNiA0MCBoIDQwIiAvPjx0ZXh0IGNsYXNzPSJpIiB4PSIxNzYiIHk9IjQ0Ij5BdG9taWM8L3RleHQ+PHBhdGggZD0iTSAyMzYgNDAgaCAyMCIgLz48cGF0aCBkPSJNIDI1NiA0MCB2IC0yMCBoIC0xMDAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjE1NiAzOSAxNTIgMzEgMTYwIDMxIj48L3BvbHlnb24+PHBhdGggZD0iTSAyNTYgNDAgaCAyMCIgLz48dGV4dCB4PSIyNzYiIHk9IjQ0Ij5GUk9NPC90ZXh0PjxwYXRoIGQ9Ik0gMzE2IDQwIGggNDAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjM1NiIgeT0iNDQiPkF0b21pYzwvdGV4dD48cGF0aCBkPSJNIDQxNiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNDM2IDQwIHYgLTIwIGggLTEwMCB2IDIwIiAvPjxwb2x5Z29uIHBvaW50cz0iMzM2IDM5IDMzMiAzMSAzNDAgMzEiPjwvcG9seWdvbj48cGF0aCBkPSJNIDQzNiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNDU2IDQwIGggMjgwIiAvPjxwb2x5Z29uIHBvaW50cz0iNzQ0IDQwIDczNiAzNiA3MzYgNDQiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9Ijc0NCA0MCA3NTIgMzYgNzUyIDQ0Ij48L3BvbHlnb24+PHBhdGggZD0iTSA0NTYgNDAgdiA0MCBoIDIwIiAvPjxwYXRoIGQ9Ik0gNDc2IDgwIGggMjAiIC8+PHRleHQgeD0iNDk2IiB5PSI4NCI+R0lWSU5HPC90ZXh0PjxwYXRoIGQ9Ik0gNTU2IDgwIGggMjAiIC8+PHRleHQgY2xhc3M9ImkiIHg9IjU3NiIgeT0iODQiPklkZW50aWZpZXI8L3RleHQ+PHBhdGggZD0iTSA2NzYgODAgaCAyMCIgLz48cGF0aCBkPSJNIDY5NiA4MCB2IC0yMCBoIC0yMjAgdiAyMCIgLz48cG9seWdvbiBwb2ludHM9IjQ3NiA3OSA0NzIgNzEgNDgwIDcxIj48L3BvbHlnb24+PHBhdGggZD0iTSA2OTYgODAgaCAyMCB2IC00MCIgLz48cG9seWdvbiBwb2ludHM9IjcxNiA0MSA3MTIgNDkgNzIwIDQ5Ij48L3BvbHlnb24+Cjwvc3ZnPg==)

## Remarks

-   subtracts the sum of all first arguments from each of the second arguments and overwrites the old value of each second argument, unless the third argument specifies a different storage place
-   so, for example, `SUBTRACT 1 FROM X Y Z` will decrement the values of `X`, `Y` and `Z`
-   all three arguments obey the rules of sufficient qualification
-   the first argument can be a literal
-   if the second argument is a literal, the third argument is mandatory
-   if the third argument is present, there can be only one second argument
-   any of the three arguments can be an identifier defined with a numeric picture clause (free from `A` and `X`)
-   if the first argument denotes a composite field, then the second argument should also denote a composite field, and the subtraction works correspondingly: for instance, if the first argument is a field `X` with inner numeric fields `A`, `B` and `C`, and the second argument is a field `Y` with inner numeric fields `A`, `C` and `D`, then `A OF Y` is decreased by `A OF X` and `C OF Y` is decreased by `C OF X`, and all other fields remain unchanged
-   if all three arguments are composite fields, combine the description above with a `MOVE` to the target location
-   if some arguments are composite fields while others are not, this statement is invalid

## Origins

[[COBOL](cobol.html)]{.ff .lang}   [[SUBTRACT](cobol.html "subtract one or more numeric values from another value")]{.ff .used}\
[[RPG](rpg.html)]{.ff .lang}   [[SUB](rpg.html "subtract one number from another number")]{.ff .used} [[Z-SUB](rpg.html "subtract a number from a field of zeroes")]{.ff .used}\

------------------------------------------------------------------------
