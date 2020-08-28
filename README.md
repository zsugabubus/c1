# c1: C Unit Testing Framework
Aims to be a compact, fully-featured unit testing framework for C89, targeting
POSIX-compilant systems only.

## Features

* Boilerplate-less syntax.
* No unsafe macro hacks; no built-in limits.
* Easy hooking and test skipping.
* Address space protection.

## Configuration

```c
/* disable forking */
#define TEST_NOFORK

/* print output during testing; otherwise wait until all tests are completed */
#define TEST_NOGATHEROUTPUT

```

## Examples

See my other projects…

## License

Released under the GNU General Public License version v3.0 or later.
