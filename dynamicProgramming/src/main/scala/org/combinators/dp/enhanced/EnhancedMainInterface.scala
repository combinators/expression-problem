package org.combinators.dp.enhanced

import org.combinators.cogen.FileWithPath
import org.combinators.dp.original.GenerationOption
import org.combinators.models.EnhancedModel

/**
 * Presents the ability to generate files from an EnhancedModel.
 *
 * This allows for customized *MainJava and *MainScalafiles. 
 */
trait EnhancedMainInterface {
  def model: EnhancedModel
  def filesToGenerate(option: GenerationOption): Seq[FileWithPath]
}