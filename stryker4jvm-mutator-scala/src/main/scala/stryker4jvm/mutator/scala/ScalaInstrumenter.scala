package stryker4jvm.mutator.scala

import stryker4jvm.core.model.Instrumenter

import java.util as ju
import scala.collection.JavaConverters.*

import cats.data.{NonEmptyList, NonEmptyVector}
import cats.syntax.all.*
import stryker4jvm.mutator.scala.extensions.TreeExtensions.TransformOnceExtension
import stryker4jvm.core.model.MutantWithId

import scala.meta.*
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class ScalaInstrumenter(options: ScalaInstrumenterOptions) extends Instrumenter[ScalaAST] {

  /** Method that places mutations in a given AST
    *
    * @param source
    *   The input AST
    * @param mutations
    *   All the mutations found in the input AST, which are used in the returned AST
    * @return
    *   AST with all mutations placed (using match statements)
    */
  override def instrument(source: ScalaAST, mutations: ju.Map[ScalaAST, ju.List[MutantWithId[ScalaAST]]]): ScalaAST = {
    if (source.value == null) {
      return new ScalaAST // TODO
    }

    val newTree = source.value
      .transformOnce {
        Function.unlift { originalTree =>
          val p = new ScalaAST(value = originalTree)

          // Get all mutations for a given tree
          val mutGet = mutations.get(p)

          if (mutGet != null) {
            val mut: Vector[MutantWithId[ScalaAST]] = mutGet.asScala.toVector

            val mutableCases = mut.map(mutantToCase)
            val maybeNonemptyList = NonEmptyList.fromList(mut.map(_.id).toList);

            var nonEmptylist: NonEmptyList[Int] = NonEmptyList.one(0);
            maybeNonemptyList match {
              case Some(value) => nonEmptylist = value
              case None        =>
            };

            val default = defaultCase(p, nonEmptylist)
            val cases = mutableCases :+ default

            // Build the match statement and cast to a Tree
            try Some(buildMatch(cases.toNev.get).asInstanceOf[Tree])
            catch {
              case NonFatal(_) => throw new Exception
            }
          } else {
            None
          }
        }
      } match {
      case Success(tree) => tree
      case Failure(e)    => throw e
    }

    new ScalaAST(value = newTree)
  }

  def mutantToCase(mutant: MutantWithId[ScalaAST]): Case = {
    val newTree = mutant.mutatedCode.mutatedStatement.value.asInstanceOf[Term]

    buildCase(newTree, options.pattern(mutant.id))
  }

  def defaultCase(scalaAST: ScalaAST, mutantIds: NonEmptyList[Int]): Case =
    p"case _ if ${options.condition.mapApply(mutantIds)} => ${scalaAST.value.asInstanceOf[Term]}"

  def buildCase(expression: Term, pattern: Pat): Case = p"case $pattern => $expression"

  def buildMatch(cases: NonEmptyVector[Case]): Term.Match =
    q"(${options.mutationContext} match { ..case ${cases.toList} })"

  // TODO: rollback should be added to Instrumenter interface
  /** Removes any mutants that are in the same range as a compile error
    */
  def attemptRemoveMutant(errors: NonEmptyList[CompilerErrMsg]): PartialFunction[Tree, Tree] = {
    // Match on mutation switching trees
    case tree: Term.Match if tree.expr.isEqual(options.mutationContext) =>
      // Filter out any cases that are in the same range as a compile error
      val newCases = tree.cases.filterNot(caze => errors.exists(compileErrorIsInCaseStatement(caze, _)))

      tree.copy(cases = newCases)
  }

  def mutantIdsForCompileErrors(tree: Tree, errors: NonEmptyList[CompilerErrMsg]) = {
    val mutationSwitchingCases = tree.collect {
      // Match on mutation switching trees
      case tree: Term.Match if tree.expr.isEqual(options.mutationContext) =>
        // Filter out default case as it's not mutated
        tree.cases.filterNot(c => c.pat.isEqual(p"_"))
    }.flatten

    errors
      .nonEmptyPartition(err =>
        mutationSwitchingCases
          .find(compileErrorIsInCaseStatement(_, err))
          .map(caze => extractMutantId(caze.pat) -> err)
          .toRight(err)
      ) match {
      case Both(a, b)   => (a.some, b.toList.toMap)
      case Ior.Left(a)  => (a.some, Map.empty[MutantId, CompilerErrMsg])
      case Ior.Right(b) => (None, b.toList.toMap)
    }
  }

  /** Extracts the mutant id from a case statement
    */
  private def extractMutantId(pat: Pat) = pat match {
    case Lit.Int(value) => MutantId(value)
    case Pat.Extract.After_4_6_0(Term.Name("Some"), Pat.ArgClause(List(Lit.String(value)))) =>
      MutantId(value.toInt)
    case _ => throw new IllegalArgumentException(s"Could not extract mutant id from '${pat.syntax}'")
  }

  /** Checks if the compile error is inside the mutant case statement
    */
  private def compileErrorIsInCaseStatement(caze: Case, error: CompilerErrMsg): Boolean = {
    (caze.pos.startLine + 1) <= error.line && (caze.pos.endLine + 1) >= error.line
  }
}
