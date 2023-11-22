package frontEnd

object errors {
    import parsley.errors.{ErrorBuilder}

    case class WaccError(pos: (Int, Int), source: Option[String], lines: WaccErrorLines) {
        override def toString: String = s"Syntax error ${source.fold("")(name => s"in $name ")}at line ${pos._1}, column ${pos._2}:\n$lines"
    }
     
    sealed trait WaccErrorLines
    case class WaccVanillaError(unexpected: Option[WaccErrorItem], expected: Set[WaccErrorItem], reasons: Set[String], line: WaccLineInfo) extends WaccErrorLines{
        var unex = unexpected match {
            case Some(v) => s"Unexpected ${v.toString}"
            case None => ""
        }
        override def toString: String = s"${unex}\nExpected ${expected.mkString(", ")}\n${reasons.mkString}\n${line.toString}"
    }
    //Double new line if no reasons. 

    case class WaccSpecialisedError(messages: Set[String], line: WaccLineInfo) extends WaccErrorLines {
        override def toString: String = s"${messages.mkString}\n${line.toString}"
    }

    case class WaccLineInfo(current: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int) {
        var before = linesBefore.map(line => s"$errorLineStart$line")
        var curr = s"$errorLineStart$current"
        var highlight = s"$errorLineStart${" " * (errorLineStart.length - 1)}${errorPointer(errorPointsAt, errorWidth)}"
        var after = linesAfter.map(line => s"$errorLineStart$line")
        
        override def toString = s"${before.mkString("\n")}\n$curr\n$highlight\n${after.mkString("\n")}"
    }

    sealed trait WaccErrorItem
    case class WaccRawItem(item: String) extends WaccErrorItem {
        override def toString: String = item
    }
    case class WaccNamedItem(item: String) extends WaccErrorItem {
        override def toString: String = item
    }
    case object WaccEndOfInput extends WaccErrorItem {
        override def toString(): String = "end of input"
    }

    private val errorLineStart = ">"
    private def errorPointer(caretAt: Int, caretWidth: Int) = s"${" " * caretAt}${"^" * caretWidth}"
    
    abstract class WaccErrorBuilder extends ErrorBuilder[WaccError] {  

        override def format(pos: Position, source: Source, lines: ErrorInfoLines): WaccError = WaccError(pos, source, lines)
        
        type Position = (Int, Int)
        override def pos(line: Int, col: Int): Position = (line, col)

        type Source = Option[String]
        override def source(sourceName: Option[String]): Source = sourceName

        type ErrorInfoLines = WaccErrorLines
        override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = WaccVanillaError(unexpected, expected, reasons, line)
        override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = WaccSpecialisedError(msgs, line)

        type ExpectedItems = Set[WaccErrorItem]
        override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

        type Messages = Set[String]
        override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

        type UnexpectedLine = Option[WaccErrorItem]
        override def unexpected(item: Option[Item]): UnexpectedLine = item
        type ExpectedLine = Set[WaccErrorItem]
        override def expected(alts: ExpectedItems): ExpectedLine = alts

        type Message = String
        override def reason(reason: String): Message = reason
        override def message(msg: String): Message = msg

        type LineInfo = WaccLineInfo
        override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], 
            errorPointsAt: Int, errorWidth: Int): LineInfo = WaccLineInfo(line, linesBefore, linesAfter, errorPointsAt, errorWidth)

        type Item = WaccErrorItem
        type Raw = WaccRawItem
        type Named = WaccNamedItem
        type EndOfInput = WaccEndOfInput.type
        override def raw(item: String): Raw = WaccRawItem(item)
        override def named(item: String): Named = WaccNamedItem(item)
        override val endOfInput: EndOfInput = WaccEndOfInput

        override val numLinesBefore: Int = 1
        override val numLinesAfter: Int = 1
    } 
}