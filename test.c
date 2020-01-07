/*
 * Copyright (C) 2019,2020 zsugabubus
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#undef _GNU_SOURCE
#define _GNU_SOURCE
#undef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#ifndef TEST_NOLIBSEGFAULT
# include <dlfcn.h>
#endif
#include <execinfo.h>
#include <limits.h>
#include <signal.h>
#include <sys/mman.h>
#include <time.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <regex.h>

#include "test.h"

#define OUTPUT_SUITE_MINLEN (5 + 4)
#define OUTPUT_TEST_MINLEN (5 + 5 + 11)

/* Provided by linker. If not, use "test.ld" linker script.  */
extern char __start_test;
extern char __stop_test;

struct _test_shared *_test_shared;
struct _test_suite_info defaultsuite;
struct _test_suite_info *currsuite = NULL;
struct _test_test_info *currtest = NULL;
struct _test_case_info *currcase = NULL;

#define US_NS 1000ul
#define MS_NS 1000000ul
#define SEC_NS 1000000000ul

#ifndef CLOCK_MONOTONIC_RAW
#define CLOCK_MONOTONIC_RAW CLOCK_MONOTONIC
#endif

#ifndef TEST_NOFORK
int test_fork(int *exitcode) {
	switch (fork()) {
	case 0: return 0;
	case -1:
		perror("fork");
		_exit(127);
	default: {
		int stat_val;
		wait(&stat_val);
		*exitcode = WIFEXITED(stat_val) ? WEXITSTATUS(stat_val) : EXIT_FAILURE;
		return 1;
	}
	}
}
#endif

void test_exit(int code) {
#ifndef TEST_NOFORK
	_exit(code);
#else
	*(currcase ? &currcase->exitcode : &currtest->exitcode) = code;
	longjmp(currcase ? currcase->env : test_test_env, code + 1);
#endif
}

#ifdef TEST_NOFORK
static struct {
	jmp_buf env;
} fake_test_env;
#endif

/** Write content of `fd` to stderr. */
static
void cat(int fd) {
	char buf[(1 << 13)];
	ssize_t len = 0;
	ssize_t plen;
	off_t offset;

	for (offset = 0;plen = len, (len = pread(fd, buf, sizeof buf, offset)) > 0;offset += len) {
		while (-1 == write(STDOUT_FILENO, buf, len)) {
			if (EAGAIN == errno)
				continue;

			perror("write");
			break;
		}
	}

	if (0 == plen || buf[plen - 1] != '\n')
		fputs("\x1b[7m$\x1b[0m\n", stderr);
	fputc('\n', stderr);
}

static unsigned time_base;
static char const*time_unit;

/** Return a time unit that makes sense for human beings. */
static
void normalize_time(unsigned long ns) {
	if (ns < 10 * US_NS)
		time_base = ns, time_unit = "ns";
	else if (ns < 10 * MS_NS)
		time_base = ns / US_NS, time_unit = "us";
	else if (ns < 10 * SEC_NS)
		time_base = ns / MS_NS, time_unit = "ms";
	else
		time_base = ns / SEC_NS, time_unit = "s ";
}

/** Return time in nanoseconds. */
__attribute__((const)) static
unsigned long tstons(struct timespec const *const ts) {
	return ts->tv_sec * SEC_NS + ts->tv_nsec;
}

/** Compute elapsed time from `start` time to `end` time. Write result into `result`. */
static
void tssub(struct timespec *result, struct timespec const *end, struct timespec const *start) {
	result->tv_sec  = end->tv_sec  - start->tv_sec  + (end->tv_nsec < start->tv_nsec);
	result->tv_nsec = end->tv_nsec - start->tv_nsec + (end->tv_nsec < start->tv_nsec ? SEC_NS : 0);
}

/** Print a `n` character wide horizontal separator using `ch` character. */
static
void print_hline(int n, char ch) {
	int i;

	for (i = 0;i < n;++i)
		fputc(ch, stdout);
	fputc('\n', stdout);
}

void _test_print_suite_path(void) {
	if (strlen(currsuite->name) > 0)
		fprintf(stdout, "\nsuite \x1b[1m%s\x1b[0m (%s:%u):\n",
			currsuite->name, currsuite->file, currsuite->line);
	else
		fprintf(stdout, "\nfile %s\x1b[1m%s\x1b[0m:\n",
			currsuite->name, currsuite->file);
}

void _test_print_test_path(void) {
	fprintf(stdout, "\x1b[1mtest %s\x1b[0m (%s:%u):\n",
		currtest->name, currtest->file, currtest->line);
}

void _test_print_case_path(void) {
	struct _test_case_info *p = currcase;

	while (p->parent)
		p = p->parent;

	fprintf(stdout, "\x1b[1mcase\x1b[0m %s/", currtest->name);

	for (;p->child;p = p->child)
		fprintf(stdout, "%s\x1b[0m/",
			p->name);

	fprintf(stdout, "\x1b[1m%s\x1b[0m (%s:%u):\n",
		p->name, p->file, p->line);
}

/** Default signal handler. */
static
void sighandler(int sig, siginfo_t *info, void *ucontext) {
	void *array[50];
	int size;

	psiginfo(info, "Unhandled signal");

	size = backtrace(array, 50);

	fprintf(stderr, "\nBacktrace:\n");
	backtrace_symbols_fd(array, size, STDERR_FILENO);

	test_exit(EXIT_FAILURE);
}

/** Initialize signal handling. */
static
void sighandler_init(void) {
	static struct sigaction sa;
	sa.sa_sigaction = sighandler;
	sa.sa_flags = SA_RESTART | SA_SIGINFO;
	sigemptyset(&sa.sa_mask);

#define SIG(name) \
	sigaction(SIG##name, &sa, NULL);

	SIG(ABRT  ) SIG(ALRM  ) SIG(BUS   ) SIG(FPE   ) SIG(HUP   )
	SIG(ILL   ) SIG(INT   ) SIG(KILL  ) SIG(PIPE  ) SIG(POLL  )
	SIG(PROF  ) SIG(QUIT  ) SIG(SEGV  ) SIG(STOP  ) SIG(TSTP  )
	SIG(SYS   ) SIG(TERM  ) SIG(TRAP  ) SIG(TTIN  ) SIG(TTOU  )
	SIG(USR1  ) SIG(USR2  ) SIG(VTALRM) SIG(XCPU  ) SIG(XFSZ  )

#undef SIG
}

union object_ptr {
	void *ptr;
	struct _test_info_header *header;
	struct _test_suite_info *suite;
	struct _test_hook_info *hook;
	struct _test_test_info *test;
};

static
void emit_event(int event) {
	union object_ptr p;

	for (p.ptr = (void *)&__start_test;p.test < currtest;) {
		switch (p.header->type) {
		case test_object_id_suite:
			++p.suite;
			break;
		case test_object_id_hook:
			if (2/*global*/ == p.hook->scope ||
			   (1/*suite */ == p.hook->scope && p.suite >= currsuite && currsuite != &defaultsuite))
				p.hook->hook(event);
			++p.hook;
			continue;
		case test_object_id_test:
			++p.test;
			break;
		default:
			__builtin_unreachable();
		}
	}
}

#ifdef __GNUC__
/* FIXME: So... I don't know why I really got these errors. */
__attribute__((no_sanitize_undefined))
#endif
int main(int argc, char **argv)
{
	enum { test_suite, test_total, test_passed, test_failed, test_ignored, test_result_count };
	unsigned stat[test_result_count] = {0};
	union object_ptr p;
	int print_width = 30;
#ifndef TEST_NOGATHEROUTPUT
	int gathered = 0;
#endif
	struct timespec ts_start, ts_end;
#ifndef TEST_NOLIBSEGFAULT
	void *libsegfault;
#endif

	sighandler_init();

#ifndef TEST_NOLIBSEGFAULT
	if (NULL == (libsegfault = dlopen("/lib/libSegFault.so", RTLD_LAZY)))
		perror("dlopen");
#endif

	clock_gettime(CLOCK_MONOTONIC_RAW, &ts_start);

	currtest = NULL;
	currsuite = &defaultsuite;
	for (p.ptr = (void *)&__start_test;p.ptr < (void *)&__stop_test;) {
		int len;

		switch (p.header->type) {
		case test_object_id_suite:
			if (NULL != currtest)
				currtest->last_in_suite = 1;

			currsuite = p.suite;
			++stat[test_suite];

			len = strlen(currsuite->name) + strlen(currsuite->file) + OUTPUT_SUITE_MINLEN;
			if (len > print_width)
				print_width = len;

			++p.suite;
			break;
		case test_object_id_hook:
			++p.hook;
			continue;
		case test_object_id_test:
			currtest = p.test;
			++stat[test_total];

			len = strlen(p.test->name) + OUTPUT_TEST_MINLEN;
			if (len > print_width)
				print_width = len;

			++p.test;
			break;
		default:
			__builtin_unreachable();
		}
	}
	if (NULL != currtest)
		currtest->last_in_suite = 1;

	{
		struct winsize ws;
		ws.ws_col = 72;
		(void)ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);

		if (print_width >= ws.ws_col * 4 / 5)
			print_width = ws.ws_col;
	}

	fprintf(stdout, "running %u tests\n",
			stat[test_total]);

	_test_shared = mmap(NULL, sizeof *_test_shared,
		PROT_READ | PROT_WRITE,
		MAP_SHARED | MAP_ANONYMOUS, -1, 0);

	if (MAP_FAILED == _test_shared) {
		perror("mmap");
		exit(127);
	}

	currsuite = &defaultsuite;
	for (p.ptr = (void *)&__start_test;p.ptr < (void *)&__stop_test;) {
		int argi;
		int skip;
#ifdef TEST_NOFORK
		int oldstderr;
		int oldstdout;
#endif

		switch (p.header->type) {
		case test_object_id_suite:
			/* Suite start marker. */
			currsuite = p.suite;
			_test_print_suite_path();
			print_hline(print_width, '-');

			emit_event(test_event_setup_suite);

			++p.suite;
			continue;
		case test_object_id_hook:
			++p.hook;
			continue;
		case test_object_id_test:
			currtest = p.test;
			++p.test;
			break;
		default:
			__builtin_unreachable();
		}

		fprintf(stdout, "test \x1b[1m%s\x1b[0m ... %*s",
			currtest->name,
			print_width - OUTPUT_TEST_MINLEN - (int)strlen(currtest->name), "");
		fflush(stdout);

		for (skip = 0, argi = 1;argi < argc;++argi) {
			char *pattern = argv[argi];
			regex_t reg;
			int should_skip;
			int ret;

			switch (*pattern) {
			case '+':
				++pattern;
			default:
				should_skip = 0;
				break;
			case '-':
				++pattern;
				should_skip = 1;
				break;
			}

			if (1 == argi)
				skip = !should_skip;

			if (0 != (ret = regcomp(&reg, pattern, REG_EXTENDED | REG_ICASE | REG_NOSUB))) {
				char buf[100];
				regerror(ret, &reg, buf, sizeof buf);
				fprintf(stderr, "test: failed to compile pattern \"%s\": %s\n", pattern, buf);
				exit(127);
			}

			if (0 == regexec(&reg, currtest->name, 0, NULL, 0))
				skip = should_skip;

			regfree(&reg);
		}

		if (skip || 0 == currtest->iters) {
			++stat[test_ignored];
			fprintf(stdout, "\x1b[34mignored\x1b[0m\n");
			goto drop_output;
		}

		currtest->outfd = memfd_create(currtest->name, MFD_CLOEXEC);
		if (-1 == currtest->outfd) {
			perror("memfd_create");
			exit(127);
		}

		_test_shared->total_ns = -1;

		emit_event(test_event_setup_test);

#ifdef TEST_NOFORK
		oldstderr = dup(STDERR_FILENO);
		oldstdout = dup(STDOUT_FILENO);
#endif

#ifndef TEST_NOFORK
		if (!_test_fork_(*currtest)) {
#else
		if (!_test_fork_(fake_test_env)) {
#endif
			struct timespec ts_test_start;
			struct timespec ts_test_end;
			unsigned long iters = currtest->iters;

			dup2(currtest->outfd, STDERR_FILENO);
			setvbuf(stderr, NULL, _IONBF, 0);
			dup2(currtest->outfd, STDOUT_FILENO);
			setvbuf(stdout, NULL, _IONBF, 0);
			clock_gettime(CLOCK_MONOTONIC_RAW, &ts_test_start);

			while (iters-- > 0)
				currtest->run();

			clock_gettime(CLOCK_MONOTONIC_RAW, &ts_test_end);
			tssub(&ts_test_start, &ts_test_end, &ts_test_start);
			_test_shared->total_ns = tstons(&ts_test_start);

			test_exit(currtest->exitcode);
		} else {
#ifdef TEST_NOFORK
			dup2(oldstderr, STDERR_FILENO);
			dup2(oldstdout, STDOUT_FILENO);
#endif

			switch (currtest->exitcode) {
			case EXIT_SUCCESS: {
				char info[50];

				currtest->total_ns = _test_shared->total_ns;

				if (-1 == currtest->total_ns)
					goto test_failed;

				normalize_time(currtest->total_ns);
				sprintf(info, "%4u %s", time_base, time_unit);

				if (currtest->iters > 1) {
					normalize_time((currtest->total_ns + currtest->iters - 1) / currtest->iters);
					sprintf(info + strlen(info), " / %9lu iters = %4u %s/iter",
						currtest->iters,
						time_base,
						time_unit);
				}

				++stat[test_passed];
				fprintf(stdout, "\x1b[1;32mok\x1b[0m, %s\n", info);
				goto drop_output;
			}
			case EXIT_SKIP:
				++stat[test_ignored];
				fprintf(stdout, "\x1b[1;34mskipped\x1b[0m\n");
				goto drop_output;
			default: {
			test_failed:
				fprintf(stdout, "\x1b[1;31mFAILED\x1b[0m\n");
					++stat[test_failed];

#ifdef TEST_NOGATHEROUTPUT
				cat(currtest->outfd);
#else
				goto keep_output;
#endif
			}
			}
		}

	drop_output:
		close(currtest->outfd);
		currtest->outfd = -1;
	keep_output:
		emit_event(test_event_teardown_test);

		if (currtest->last_in_suite)
			emit_event(test_event_teardown_suite);
	}

#ifndef TEST_NOGATHEROUTPUT
	print_hline(print_width, '=');
	for (p.ptr = (void *)&__start_test;p.ptr < (void *)&__stop_test;) {
		switch (p.header->type) {
		case test_object_id_suite:
			++p.suite;
			break;
		case test_object_id_hook:
			++p.hook;
			break;
		case test_object_id_test:
			if (-1 != p.test->outfd) {
				gathered = 1;
				currtest = p.test;
				_test_print_test_path();
				cat(p.test->outfd);

				close(p.test->outfd);
				p.test->outfd = -1;
			}
			++p.test;
			break;
		default:
			__builtin_unreachable();
		}

	}
#endif

	clock_gettime(CLOCK_MONOTONIC_RAW, &ts_end);
	tssub(&ts_start, &ts_end, &ts_start);
	normalize_time(ts_start.tv_sec * SEC_NS + ts_start.tv_nsec);

	if (gathered)
		print_hline(print_width, '=');
	fprintf(stdout,
		"%*s%10ld assertions, %4u %s\n"
		"\x1b[0;1mtest result\x1b[0m: \x1b[1m%s\x1b[0m. "
		"\x1b[1;32m%u\x1b[0;32m passed\x1b[0m; "
		"\x1b[1;31m%u\x1b[0;31m failed\x1b[0m; "
		"\x1b[1;34m%u\x1b[0;34m ignored\x1b[0m\n",
		print_width - 30, "", _test_shared->num_assertions, time_base, time_unit,
		0 == stat[test_failed] ? "\x1b[1;32mOK" : "\x1b[1;31mFAILED",
		stat[test_passed],
		stat[test_failed],
		stat[test_ignored]);

	munmap(_test_shared, sizeof *_test_shared);
#ifndef TEST_NOLIBSEGFAULT
	dlclose(libsegfault);
#endif

	exit(0 == stat[test_failed] ? EXIT_SUCCESS : EXIT_FAILURE);
}
/* vim:set ft=c ts=4 sw=4 noet: */
