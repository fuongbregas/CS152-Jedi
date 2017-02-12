package ui
import scala.util.parsing.combinator._
import expressions._
import value._

class SithParsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  // def declaration, conditional, dusjunction, and other parsers
  def declaration: Parser[Expression] = "def" ~ identifier ~ "=" ~ expression ^^
    {
      case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
    }

  // OPERANDS ::= (~(EXPRESSION ~ (,~EXPRESSION)*)?~)
  // i.e. OPERANDS ::= A comma-separated list of 0 or more expressions bracketed by parentheses
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^
    {
      case None          => Nil
      case Some(e ~ Nil) => List(e)
      case Some(e ~ exp) => e :: exp
    }

  // LITERAL ::= BOOLE | NUMERAL
  def literal: Parser[Literal] = boole | numeral

  def boole: Parser[Boole] = """true|false""".r ^^
    {
      case e => new Boole(e.toBoolean)
    }

  // NUMERAL ::= (\+|-)?~DIGIT~(.~DIGIT+)?
  def numeral: Parser[Number] = """(/+|-)?[0-9]+(\.[0-9]+)?""".r ^^
    {
      case e => new Number(e.toDouble)
    }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^
    {
      case "if" ~ "(" ~ exp1 ~ ")" ~ exp2 ~ None                => Conditional(exp1, exp2)
      case "if" ~ "(" ~ exp1 ~ ")" ~ exp2 ~ Some("else" ~ exp3) => Conditional(exp1, exp2, exp3)
    }

  // INEQUALITY ::= SUM ~ ((< | > | !=) ~ SUM)?
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^
    {
      case t ~ None           => t
      case t ~ Some("<" ~ s)  => FunCall(Identifier("less"), List(t, s))
      case t ~ Some(">" ~ s)  => FunCall(Identifier("greater"), List(t, s))
      case t ~ Some("!=" ~ s) => FunCall(Identifier("unequals"), List(t, s))
    }

  // SUM ::= PRODUCT ~ ((\+|-) ~ PRODUCT)*
  def sum: Parser[Expression] =
    product ~ rep(("+" | "-") ~ product ^^ { case "+" ~ s => s case "-" ~ s => negate(s) }) ^^ {
      case p ~ Nil  => p
      case p ~ rest => FunCall(Identifier("add"), p :: rest)
    }

  // TERM ::= LAMBDA | BLOCK | LITERAL | IDENTIFIER | (~EXPRESSION~)
  def term: Parser[Expression] = assignment | iteration  |deref | lambda | block | literal | identifier | "(" ~> expression <~ ")"

  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^
    {
      case e => Identifier(e)
    }

  // exp -> 0 - exp
  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }

  // exp => 1/exp
  def invert(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1)
    FunCall(div, List(one, exp))
  }

  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^
    {
      case exp ~ Nil     => exp
      case exp ~ expList => FunCall(Identifier("equals"), exp :: expList)
    }

  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^
    {
      case exp ~ Nil     => exp
      case exp ~ expList => Conjunction(exp :: expList)
    }

  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^
    {
      case exp ~ Nil     => exp
      case exp ~ expList => Disjunction(exp :: expList)
    }

  def funcall: Parser[Expression] = term ~ opt(operands) ^^
    {
      case f ~ Some(args) => FunCall(f, args)
      case f ~ None       => f
      case f: Literal     => throw new JediException("")
    }

  def product: Parser[Expression] =
    funcall ~ rep(("/" | "*") ~ funcall ^^ { case "*" ~ s => s case "/" ~ s => invert(s) }) ^^ {
      case p ~ Nil  => p
      case p ~ rest => FunCall(Identifier("mul"), p :: rest)
    }

  /////////////////////////////////////////////////Wookie/////////////////////////////////////////

  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^
    {
      case None           => Nil
      case Some(e ~ Nil)  => List(e)
      case Some(e ~ exps) => e :: exps
      case _              => Nil
    }

  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ exp ~ explist ~ "}" => Block(exp :: explist)
    case "{" ~ exp ~ Nil ~ "}"     => Block(List(exp))
  }

  def lambda: Parser[Expression] = "lambda" ~ parameters ~ expression ^^ {
    case "lambda" ~ params ~ exp => Lambda(params, exp)
  }
  
  
  /////////////////////////////////////////////Sith/////////////////////////////////
  def assignment:Parser[Expression] = identifier ~ "=" ~ expression ^^ {
   case id ~ "=" ~ exp => Assignment(id,exp)
 }
 
 def iteration:Parser[Expression] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
   case "while" ~ "(" ~ condition ~ ")" ~ exp =>  new Iteration(condition, exp )
 }
 
 def deref:Parser[Expression] = "[" ~ expression ~ "]" ^^ { 
   case "[" ~ exp ~ "]" =>  FunCall(Identifier("content"), List(exp))
 }
}