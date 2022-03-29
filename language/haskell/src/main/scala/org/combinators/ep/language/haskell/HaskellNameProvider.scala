package org.combinators.ep.language.haskell

import org.combinators.ep.generator.NameProvider
import org.combinators.ep.language.haskell.Syntax.MangledName

import java.nio.charset.StandardCharsets

object HaskellNameProvider extends NameProvider[Syntax.MangledName] {
  def mangle(name: String): Syntax.MangledName = {
    if (!name.matches("[a-zA-Z][a-zA-Z0-9_]*") || name.startsWith("mangled_")) {
      MangledName(name, s"mangled_${name.getBytes(StandardCharsets.UTF_8).mkString("_")}")
    } else {
      MangledName(name, name)
    }
  }

  def addPrefix(prefix: String, name: Syntax.MangledName): Syntax.MangledName = {
    mangle(prefix + name.original)
  }

  def addSuffix(name: Syntax.MangledName, suffix: String): Syntax.MangledName = {
    mangle(name.original + name)
  }
}
