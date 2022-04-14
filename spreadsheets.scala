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
// 4.5 Error checking (bounds checking, circular reference check) ,
// Easy but typedef the Sheet for all the 2d arrays
// 6. View port - sheets too big.
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

// internalLocation is where it is in the actual 2d array.
class ColumnLocation(internalLocation: Int):
	def toUserDescription: String =
		return (internalLocation + 65).toChar.toString
		
	// Returns immutable
	def +(modification: Int): ColumnLocation =
		return new ColumnLocation(internalLocation + modification)
		
	def zeroBasedLocation: Int=
		this.internalLocation
		
//TODO: Skipping wrapping this in Object for now...same with all other factory methods like this.
def colFromUserDescription(userDescription: String): ColumnLocation =
	// Note: did not think much beyond Z but put this in anyway.
	new ColumnLocation((userDescription.zip(userDescription.indices.reverse)
	.map((colPiece, exp) => ((colPiece - 64) * scala.math.pow(26, exp))).sum - 1).toInt	)

class RowLocation(internalLocation: Int):
	def toUserDescription: String =
		(internalLocation + 1).toString
	
	def +(modification: Int): RowLocation =
		return new RowLocation(internalLocation + 1)
		
	def zeroBasedLocation: Int =
		this.internalLocation
		
def rowFromUserDescription(userDescription: String): RowLocation =
	new RowLocation(Integer.parseInt(userDescription) - 1)
		
class Location(col: ColumnLocation, row: RowLocation):
	def toUserDescription: String =
		col.toUserDescription + row.toUserDescription

	def withRowModification(modification: Int) : Location =
		new Location(this.col, this.row + modification)
	
	def withColModification(modification: Int) : Location =
		new Location(this.col + modification, this.row)
		
	def getRowColInternal(): (Int, Int) =
		(this.row.zeroBasedLocation, this.col.zeroBasedLocation)
		
	def isAfterRow(otherRow: RowLocation) =
		this.row.zeroBasedLocation > otherRow.zeroBasedLocation
		
	def isAfterColumn(otherColumn: ColumnLocation) =
		this.col.zeroBasedLocation > otherColumn.zeroBasedLocation
		
def locationFromUserDescription(userDescription: String): Option[Location] =
	val locationPattern =  raw"([A-Z]+)([0-9]+)".r;
	if locationPattern.matches(userDescription) then		
		userDescription match {
			case locationPattern(col, row) => Some(new Location(colFromUserDescription(col), rowFromUserDescription(row)))
		}
	else
		None

def resolveFormula(formula: String, sheetArray: Array[Array[Cell]]) : String =
	val tokens = tokenize(formula)
	val resolvedTokens = tokens.map(t => 
		locationFromUserDescription(t) match
			case Some(l: Location) => getCellAt(l, sheetArray).displayForResultMode(sheetArray)
			case None => t)
	resolveTokenizedFormula(resolvedTokens)

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
			translateCoords(splitCommand(1)) match
				case Some(rowIdx: Int, colIdx: Int) => rows(rowIdx).update(colIdx, Cell(None))
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

class CellPrinter(cellLength: Int=5, cellPaddingAmount: Int=1, totalRowNumSpace: Int=3):
	def printCell(s: String): String = 
		if s.length() > cellLength then
			return s.substring(0, cellLength)
		else
			return s + "_"*(cellLength - s.length())
	
	//TODO: Mode should probably be an enum or something. Right now either formula mode or result mode.
	def printRow(rowNum: Int, sheet: Array[Array[Cell]], resultMode: Boolean) =
		val rowValues = sheet(rowNum - 1)
		val cellPadding = " " * cellPaddingAmount
		val rowNumPadding = " "*(totalRowNumSpace - (rowNum.toString().length()))
		rowNum.toString + rowNumPadding + rowValues
			.map(cell => printCell(if resultMode then cell.displayForResultMode(sheet) else cell.displayForFormulaMode))
			.reduce((acc: String, cellValue: String) => acc + cellPadding + cellValue)
			
	def printHeaders(n: Int) =
		// TODO: Obvs. would break for mulitple char rows...
		// TODO: maybe clarify but everything after the first one...
		val headers = genHeaders(n)
		val normalPadding =  " " * (cellLength + cellPaddingAmount - 1 ) // -1 for the length of the header.
		val initialPadding = " " * totalRowNumSpace + " " * (cellLength / 2)
		initialPadding + headers.reduce((acc: String, colHeader: String) => acc + normalPadding + colHeader)

	//TODO: Look up ascii -> char conversions
	//TODO: Document a maximum and auto-generate the double letters.
	private def genHeaders(n: Int) =
		val startHeaders = List(
			"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
		startHeaders.take(n)

// Obviously inadequate, prob need regex to match all references, numbers, operators, etc.
// but pulling to one place for that upgrade.
def tokenize(rawFormula: String): Array[String] =
	//TODO: Add a trim...?
	rawFormula.split("\\s+")


