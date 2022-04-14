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