// Reactive spreadsheet
// Commandl line - enter commands on the command line and spreadsheet just reprints each time.

// REMINDER: Use scala3-compiler aND to run, always scala3

// Command line "grammar"
// EDIT A1 <insert text here>
// DELETE A1
// INSERT-BELOW INSERT-ABOVE
// INSERT-LEFT-OF / INSERT-RIGHT-OF
// DELETE-ROW / DELETE-COLUMN
// MODE FORMULA
// MODE RESULTS

// We'll try +, -, *, / hopefully infinetly ( have to write a parser) with references to other cells, 

// Note: This would probably be better with something like ncurses but this is a learning exercise
// and could probably be theoretically ported.

// Plan of attack (incomplete but should be plenty)
// 1. Print the spread sheet (10x10) -- including blanks???
// 2. Edit the cells
// 3. Implement one equation operator (+)
// 3.5 Formula versus results mode.
// 4. Implement some insertion operators
// 5. How to handle text/numbers that are too big for standard cell size??? 
// 4.25 More mature parsing of both commands and formulas (E.g. whitespace)
///===
// 4.5 Error checking (bounds checking, circular reference check<Idea: Resolver object with a cellHistory + ref to sheet ??) ,
// Easy but typedef the Sheet for all the 2d arrays
// 6. View port - sheets too big. - Primary issue here is how to "hint" at hidden parts of sheet.
// 7. Figure out some concept of ranges in rows, cols, or both...e.g. I want to add all the numbers in row 1 or col B...
// 8. Undo/command history would be cool but haven't thought how to implement. -- COULD ALSO pass a list of commands to the command line
      // -- useful for testing.
// n. Would probably want a help command
// n1. Save/load
// n2. Columns beyond Z
// n3. Wild: some kind of api like as a backing DB ???
// n4. Functions - like sort perhaps...could go through common spreadsheet applications
/*
    A     B     C     D     E     F     G     H     I     J
1 _____ _____ _____ _____ _____ _____ _____ _____ _____ _____ 
2  _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
3  _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
4  _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
50 _____ _____ _____ _____ _____ _____ _____ _____ _____ _____
...
*/

import scala.io.StdIn;

type ParsedCellValue = Option[Int] | Option[String] | Option[Float]
	
class Cell(var value: ParsedCellValue): 
	// Not handling any knid of truncating because of display limitaitons here.
	def displayForResultMode(sheet: Array[Array[Cell]]): String = 
		this.value match
			case None => ""
			case Some(i: Int) => i.toString()
			case Some(f: Float) => f.toString()
			case Some(s: String) => resolveFormula(s, sheet)
  
	// Almost certainly a better way to implement this.
	def displayForFormulaMode: String = 
		this.value match
			case None => ""
			case Some(i: Int) => i.toString()
			case Some(f: Float) => f.toString()
			case Some(s: String) => s

def resolveFormula(formula: String, sheetArray: Array[Array[Cell]]) : String =
	val tokens = tokenize(formula)
	val resolvedTokens = tokens.map(t => 
		locationFromUserDescription(t) match
			case Some(l: Location) => getCellAt(l, sheetArray).displayForResultMode(sheetArray)
			case None => t)
	resolveTokenizedFormula(resolvedTokens.toList)

//Intended to have resolveFormula always called 1st.
// ALSO: Intended to have all references resolved before calling...
def resolveTokenizedFormula(tokens: List[String]) : String =
	if (tokens.length == 1) {
		return tokens.head
	}
	//TODO: Just starting with ints...Double can't be in cells referenced by a formula
	val firstNum = Integer.parseInt(tokens.head)
	val op = tokens.tail.head
	//TODO: Scala can actually do == ? -- check to make sure we're not getting weird intern behavior (prob. not)
	if (!op.equals("+")) {
		throw new UnsupportedOperationException("Only + is supported as addition");
	}
	val secondNum = Integer.parseInt(tokens.tail.tail.head)
	resolveTokenizedFormula( List((firstNum+secondNum).toString).concat(tokens.tail.tail.tail)) 

// Very messy, just for internal convenience -- doing int or blank
def parseStringTemp(s: String): Array[Cell] =
	s.split(",").map(item =>
		try
			Cell(Some(Integer.parseInt(item.trim)))
		catch
			case e: NumberFormatException => Cell(None)
		)

var inResultMode = true

@main def runSheet() =
	// Arrays are mutable but this style seems to not fight the API (???) - Not sure what the idiomatic scala way is...?
	val rowOne = parseStringTemp("1, 2, 3")
	val rowTwo = parseStringTemp("5, ,3")
	var rows = Array(rowOne, rowTwo)
	
	val cellPrinter = new CellPrinter()
	while true do
		println(cellPrinter.printHeaders(rows(0).length))
		rows.indices.foreach(idx => println(cellPrinter.printRow(idx + 1, rows, inResultMode)))
		print("Enter Command: ")
		val userCommand = StdIn.readLine()
		rows = processCommand(userCommand, rows)

def extractInt(possibleInt: String): Option[Int] =
	try
		return Some(Integer.parseInt(possibleInt))
	catch
		case e: NumberFormatException => return None
		
// Both edits in place and/or may return new rows...
def processCommand(rawCommand: String, rows: Array[Array[Cell]]) : Array[Array[Cell]] =
	// Command syntax right now <Command> <Cell> <value> ; among others.
	// EDIT A1 5
	val splitCommand = rawCommand.split("\\s+", 3)
	
	splitCommand(0) match
		case "SET" =>
			translateCoords(splitCommand(1)) match // Branch off of valid/invalid coordinate supplied
				case Some(rowIdx: Int, colIdx: Int) => extractInt(splitCommand(2)) match // Branch off of different strings
					case Some(i: Int) => rows(rowIdx).update(colIdx, Cell(Some(i)))
					case None => rows(rowIdx)(colIdx).value = Some(splitCommand(2))
				case None => println("Provided location not recognized. No updates were applied")
		case "DELETE" =>
			locationFromUserDescription2(splitCommand(1)) match
				case Some(l: Location) => rows(l.zeroBasedRow).update(l.zeroBasedCol, Cell(None))
				case Some(row: RowLocation) => return deleteRow(row, rows)
				case Some(col: ColumnLocation) => return deleteCol(col, rows)
				case None => println("Provided location not recognized. No deletion executed")
		case "MODE" => splitCommand(1) match
				case "RESULT" => inResultMode = true
				case "FORMULA" => inResultMode = false
				case _ => println("Mode not recognized. Mode not changed")
		case "READ" =>
			translateCoords(splitCommand(1)) match
				case Some(rowIdx: Int, colIdx: Int) => 
					if inResultMode then
						println(rows(rowIdx)(colIdx).displayForResultMode(rows))
					else
						println(rows(rowIdx)(colIdx).displayForFormulaMode)
				case None => println("Provided location not recognized so no cells read")
		case "INSERT-AFTER" =>
			return processInsertAfter(splitCommand(1), rows)
		/*case "INSERT-BEFORE" =>
		//TODO: BRING INSERT-BEFORE back when we have easy descriptions to modify...
			//TODO: We could add a modifiction argument to processRowAfter - defaults for 0 above, -1 for others???
			val rowNum = Integer.parseInt(splitCommand(1))
			return insertRowAfter(rowNum - 1, rows)*/
		case _ => println("Command not recognized and IGNORED")
	return rows // Normally return passed in rows (may be mutated). In some cases (insertion), we make a new rows)

def processInsertAfter(insertArg: String, sheet: Array[Array[Cell]]): Array[Array[Cell]] =
	val colRegex = raw"[A-Z]".r
	val rowRegex = raw"[0-9]".r
	if rowRegex.matches(insertArg) then
		return insertRowAfter(rowFromUserDescription(insertArg), sheet)
	else if colRegex.matches(insertArg) then
		return insertColAfter(colFromUserDescription(insertArg), sheet)
	else
		println("Insert argument not recognized. No insertion performed")
		return sheet //no-op

def insertRowAfter(location: RowLocation, sheet: Array[Array[Cell]]) :Array[Array[Cell]] =
	// First we'll adjust any existing formulas
	// Modifying everything in place here...
	sheet.foreach(row => row.foreach( cell =>
		cell.value match
			case Some(s: String) => cell.value = Some(transformForRowInsertion(location, s))
			case _ => None // No-op - we only care about transforming strings.
			))

	val cellsToCreate = sheet(0).length // How many cells in the first row
	val newCells : Array[Cell] = ((0 to (cellsToCreate - 1)).map(_ => new Cell(None))).toArray
	// 0 is the top row
	// 0,-1 will produce empty which is what we want
	sheet.slice(0, (location + 1).zeroBasedLocation).concat(Array(newCells)).concat(sheet.slice((location + 1).zeroBasedLocation, sheet.length))

//TODO: Check if we handle -1 case
def transformForRowInsertion(afterRow: RowLocation, formula: String):String =
	val tokens = tokenize(formula)
	tokens.map(userDescription => (userDescription, locationFromUserDescription(userDescription)))
		.map((originalToken, parsedLocation) =>
			parsedLocation match
				case Some(location: Location) => if location.isAfterRow(afterRow) then location.withRowModification(1).toUserDescription else location.toUserDescription
				case None => originalToken)
		.mkString(" ")

//Not somewhat in place but must use return value...row...
def insertColAfter(col: ColumnLocation, sheet: Array[Array[Cell]]) :Array[Array[Cell]] =
	// First, adjust any existing formulas.
	sheet.foreach(row => row.foreach(cell =>
		cell.value match
			case Some(s: String) => cell.value = Some(transformForColInsertion(col, s))
			case _ => None // No-op - we only care about transforming strings.
		))

	// Insert 1 new cell in each row
	sheet.map(row =>
		row.slice(0, (col + 1).zeroBasedLocation)
		.concat(Array(new Cell(None)))
		.concat(row.slice((col + 1).zeroBasedLocation, row.length)))
	
def transformForColInsertion(afterCol: ColumnLocation, formula: String): String =
	val tokens = tokenize(formula)
	tokens.map(userDescription => (userDescription, locationFromUserDescription(userDescription)))
		.map((originalToken, parsedLocation) => 
			parsedLocation match
				case Some(location: Location) => if location.isAfterColumn(afterCol) then location.withColModification(1).toUserDescription else location.toUserDescription
				case None => originalToken)
		.mkString(" ")

def deleteRow(row: RowLocation, sheet: Array[Array[Cell]]): Array[Array[Cell]] =
	val rowIdx = row.zeroBasedLocation
	sheet.slice(0, rowIdx).concat(sheet.slice(rowIdx + 1, sheet.length))

def deleteCol(col: ColumnLocation, sheet: Array[Array[Cell]]): Array[Array[Cell]] =
	val colIdx = col.zeroBasedLocation
	sheet.map(row => row.slice(0, colIdx).concat(row.slice(colIdx + 1, row.length)))
	
// Returns None if not a valid location.
// NOTE: AS IS throughout - not checking bounds, just the format of the location.
// REturned rowCol
def translateCoords(externalDescription: String): Option[(Int, Int)] =
	locationFromUserDescription(externalDescription)
	.map(_.getRowColInternal())
	
// Would be good addition to Sheet class if we ever made it.
def getCellAt(l: Location, sheet: Array[Array[Cell]]) =
	val (row, col) = l.getRowColInternal()
	sheet(row)(col)

// Obviously inadequate, prob need regex to match all references, numbers, operators, etc.
// but pulling to one place for that upgrade.
def tokenize(rawFormula: String): Array[String] =
	//TODO: Add a trim...?
	rawFormula.split("\\s+")