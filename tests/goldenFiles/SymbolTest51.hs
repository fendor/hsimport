Tried to remove Constructors from a Type that exposed all Constructors.
This does not work because other Constructors are not available for HsImport.
Thus, this operation can not be performed.

Example:
import Foo.Bar (Baz(..))

> hsimport --hiding -m Foo.Bar -s Baz -w A

The correct solution would be, assuming Constructors are A, B and C, to change the import to:
import Foo.Bar (Baz(B,C))

However, this is not possible for this program, thus, we abort the program execution.

