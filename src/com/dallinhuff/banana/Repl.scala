package com.dallinhuff.banana

import scala.io.StdIn.readLine
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

import Types.*
import com.dallinhuff.banana.Parser

// app entry point
@main def main(): Unit =
  println("Welcome to Banana Interpreter\n")
  println("e - exit")
  println("r - start a repl session")
  println("f - run a file")

  readLine("\n> ").trim match
    case "e" => ()
    case "r" => runRepl()
    case "f" =>
      Try(Source.fromFile(readLine("enter file path: ").trim))
        .fold(
          err => println(err.getMessage),
          readFile
        )
end main

// repl loop
def runRepl(): Unit =
  println("Starting REPL session...")
  println("Enter an expression or type \"exit\" to end session")

  @tailrec
  def readInput(): Unit =
    readLine("\nbanana> ").trim match
      case "exit" => ()
      case i =>
        val result = eval(i)
        println(result.fold(err => s"ERROR: $err", value => value.display))
        readInput()

  readInput()
end runRepl

// parse/desugar/interpret a file
def readFile(src: Source): Unit =
  val result = eval(src.getLines().mkString("\n"))
  println(result.fold(err => s"ERROR: $err", value => value.display))
  src.close()

// parse/desugar/interpret a string
def eval(str: String): Either[String, Val] =
  for
    sExp <- Parser.parse(str)
    cExp <- Right(Desugarer.desugar(sExp))
    result <- Interpreter.interpret(cExp)
  yield result

