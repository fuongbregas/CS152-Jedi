package ui

import expressions._
import value._

object system {

  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add"     => add(args)
      // mul, sub, div, equals, less, etc.
      case "sub"     => sub(args)
      case "mul"     => mul(args)
      case "div"     => div(args)
      case "less"    => less(args)
      case "equals"  => equal(args)
      case "greater" => greater(args)
      case "not"     => not(args)
      case "content" => setContent(args)
      case "var"     => makeVar(args)
      case _         => throw new UndefinedException(opcode.name)
    }
  }

  private def makeVar(args: List[Value]) = {
    if (args.isEmpty) 
      throw new JediException("Error")
    new Variable(args.head)
  }

  private def setContent( args : List[Value]):Value = {
   if (args.isEmpty) 
     throw new JediException("Error")
   else if (args.head.isInstanceOf[Variable]) {
     args.head.asInstanceOf[Variable].content 
   }
   else 
     throw new TypeException
 }
  
  private def add(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ + _)
  }

  private def sub(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ - _)
  }

  private def mul(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ * _)
  }

  private def div(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])
    args2.reduce(_ / _)
  }

  private def less(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])

    var check = true
    for (i <- 1 until args2.length) {
      if (args2(i - 1).value < args2(i).value)
        check = true
      else
        check = false
    }
    if (check)
      new Boole(true)
    else
      new Boole(false)
  }

  private def greater(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])

    var check = true
    for (i <- 1 until args2.length) {
      if (args2(i - 1).value > args2(i).value)
        check = true
      else
        check = false
    }
    if (check)
      new Boole(true)
    else
      new Boole(false)
  }

  private def equal(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Number])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
    val args2 = vals.map(_.asInstanceOf[Number])

    var check = true
    for (i <- 1 until args2.length) {
      if (args2(i - 1).value == args2(i).value)
        check = true
      else
        check = false
    }
    if (check)
      new Boole(true)
    else
      new Boole(false)
  }

  ///////////////////BOOLEAN////////////////
  private def not(vals: List[Value]): Value = {
    if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Boole])
    if (ok.length < vals.length) throw new TypeException("all addition inputs must be Boolean")
    if (vals.length > 1) throw new JediException("Only 1 Boolean")
    val args2 = vals.map(_.asInstanceOf[Boole])
    args2(0)!
  }

}