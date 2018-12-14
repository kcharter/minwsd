'minwsd' is a program that minimizes the differences in white space
between two files. It's intended for repairing unwanted formatting
introduced by source-to-source transformation tools.

I'm interested in trying out some source-to-source transformation
tools, but they're not great at preserving comments or white space. In
particular, they often have their own pretty-printers and don't
preserve the formatting of the original code. This is a pain since it
introduces spurious differences in the history of files that are under
revision control. Diff tools are often smart enough to ignore
differences in whitespace other than line breaks, but if the positions
of line breaks change, that doesn't help. It's a lousy practice to mix
real source changes with formatting changes.

Here's the basic use case. Suppose there is a program T that performs
some (minor) transformation on an input code file P to produce P'.

 T P > P'

If T strips comments and changes the formatting, then

 minwsd P P'

writes on standard out a new code stream P'' that contains all the
same code as P', but has whitespace as similar to the original file P
as possible. It's not guaranteed to be perfect, but it should be close
enough that a small amount of manual editing fixes any lingering
problems.

Of course, an easier way to deal with this problem is to adopt
mechanical pretty-printers in the first place, and use them after
applying the transformation T. However, there is still the problem of
recovering the comments from the original file.

