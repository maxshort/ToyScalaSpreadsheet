# Toy Scala Spreadsheet

Fun little "command line spreadsheet"

Disclaimer: Hobby project, probably bugs, brittle, not idiomatic scala, etc.

To build and run:

In directory -- 
* `scala3-compiler spreadsheets.scala`
* `scala3 spreadsheets.scala`

Example commands session:

```
SET A1 10
SET C2 A1 + B1 + C1
SET B1 5
SET A1 -2
INSERT-AFTER C
SET D1 A1 + B1 + C1
INSERT-AFTER B
MODE FORMULA
READ E1 # Notice formula was auto-adjusted on INSERT-AFTER
MODE RESULT
# Stopping here as deleting cells that are a part of formulas is error-prone.
```


List of commands

* `SET`
	* Description: Set the value of a cell to a number or formula
	* Examples:
		* `SET A1 3`
		* `SET C1 A1 + B1`
* `DELETE`
	* Descripton: Two uses: 1). Remove a row or column. 2). Set a cell to blank. NOTE: Error-prone around formula cells.
	* Examples:
		* `DELETE C`
		* `DELETE 2`
		* `DELETE A2`
* `READ`
	* Description: Display the full contents of a cell. Useful for viewing hidden overflowed cell contents e.g. in formula mode.
	* Example: `READ C1`
* `MODE`
	* Description: Toggle between showing user-entered values and calculated formula results.
	* Examples:
		* `MODE FORMULA`
		* `MODE RESULT`.
* `INSERT-AFTER`
	* Description: Add a row or column after the specified row or column.
	* Examples:
		* `INSERT-AFTER B`
		* `INSERT-AFTER 2`
