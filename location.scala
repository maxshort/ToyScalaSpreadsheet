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

	def zeroBasedRow : Int =
		this.row.zeroBasedLocation

	def zeroBasedCol : Int =
		this.col.zeroBasedLocation
		
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

// (Could use a better name)
// Someone has specified a location -- e.g. to delete. Figure out what it is and return it.
def locationFromUserDescription2(externalDescription: String): Option[Location | ColumnLocation | RowLocation] =
	locationFromUserDescription(externalDescription) match
		case Some(l: Location) => Some(l)
		case None => {
			// Row pattern
			if raw"[A-Z]+".r.matches(externalDescription) then
				Some(colFromUserDescription(externalDescription))
			// Col Pattern
			else if raw"[0-9]+".r.matches(externalDescription) then
				Some(rowFromUserDescription(externalDescription))
			else
				None
		}