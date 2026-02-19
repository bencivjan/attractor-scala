package ai.attractor.pipeline.pipelines

import scala.io.Source

/** Embedded pipeline definitions for the Attractor engine.
  *
  * Pipelines are loaded from classpath resources at first access and
  * cached. Additional pipelines can be registered at runtime via
  * [[register]].
  */
object Pipelines:

  /** Identifier for the built-in default pipeline. */
  val DefaultName: String = "plan_build_verify"

  // Classpath-loaded pipelines, cached on first access
  private lazy val developerDot: String =
    loadResource("/pipelines/developer.dot")

  private lazy val evaluatorDot: String =
    loadResource("/pipelines/evaluator.dot")

  private lazy val factoryDot: String =
    loadResource("/pipelines/factory.dot")

  // Mutable catalog seeded with the built-in pipelines
  private val catalog: scala.collection.mutable.Map[String, String] =
    scala.collection.mutable.Map.empty

  private def ensureBuiltins(): Unit =
    if !catalog.contains(DefaultName) then
      catalog(DefaultName) = developerDot
      catalog("evaluator") = evaluatorDot
      catalog("factory") = factoryDot

  /** Returns the DOT source for the default Plan-Build-Verify pipeline. */
  def default: String =
    ensureBuiltins()
    catalog(DefaultName)

  /** Returns the DOT source for a named pipeline, or None if not found. */
  def get(name: String): Option[String] =
    ensureBuiltins()
    catalog.get(name)

  /** Registers a named pipeline. Existing entries with the same name are overwritten. */
  def register(name: String, dotSource: String): Unit =
    ensureBuiltins()
    catalog(name) = dotSource

  /** Returns the names of all registered pipelines. */
  def names: List[String] =
    ensureBuiltins()
    catalog.keys.toList.sorted

  private def loadResource(path: String): String =
    val stream = getClass.getResourceAsStream(path)
    if stream == null then
      throw new IllegalStateException(s"Pipeline resource not found: $path")
    try Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
