package stryker4jvm.testutil.stubs

import cats.effect.IO
import fs2.Stream
import fs2.io.file.Path
import stryker4jvm.files.{FilesFileResolver, MutatesFileResolver}

class TestFileResolver(returns: Seq[Path]) extends MutatesFileResolver with FilesFileResolver {
  def files: fs2.Stream[IO, Path] = Stream.emits(returns)
}
