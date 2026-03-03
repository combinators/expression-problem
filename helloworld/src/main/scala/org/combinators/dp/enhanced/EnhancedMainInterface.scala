package org.combinators.dp.enhanced

import org.combinators.dp.GenerationOption
import org.combinators.ep.generator.FileWithPath
import org.combinators.models.EnhancedModel

/**
 * Presents the ability to generate files from an EnhancedModel.
 *
 * This allows for customized *MainJava files. The EnhancedDPMainjava file uses genericTests and if you
 * have customized tests, you need to implement your own, and then that class must support this interface.
 */
trait EnhancedMainInterface {
  def filesToGenerate(model: EnhancedModel, option: GenerationOption): Seq[FileWithPath]
}