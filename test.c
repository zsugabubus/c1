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
#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE
#include <errno.h>
#include <execinfo.h>
#include <limits.h>
#include <regex.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <time.h>

#include "test.h"

#define OUTPUT_SUITE_MINLEN (5 + 4)
#define OUTPUT_TEST_MINLEN (5 + 5 + 11)

#define ARRAY_LEN(a) (sizeof a / sizeof *a)

#define FOREACH_OBJECT for (o.ptr = (void *)&__start_test; o.ptr < (void *)&__stop_test;)

#define NS_PER_US  UINT64_C(1000)
#define NS_PER_MS  UINT64_C(1000000)
#define NS_PER_SEC UINT64_C(1000000000)

/* provided by linker; if not, use "test.ld" linker script */
extern char __start_test;
extern char __stop_test;

union test_object {
	void *ptr;
	struct test_header_ *header;
	struct test_suite_ *suite;
	struct test_hook_ *hook;
	struct test_test_ *test;
};

struct test_shared_ *test_shared_;
struct test_suite_ *currsuite = NULL;
#define currtest test_current_test_
struct test_test_ *currtest = NULL;
#define currcase test_current_case_
struct test_case_ *currcase = NULL;

static int ctty;

#ifdef TEST_NOFORK
static struct {
	jmp_buf env;
} currtest_env;
#endif

#ifndef CLOCK_MONOTONIC_RAW
# define CLOCK_MONOTONIC_RAW CLOCK_MONOTONIC
#endif

#ifndef TEST_NOFORK
#ifdef __GNUC__
__attribute__((noinline))
#endif
int
test_fork__(int *status)
{
	pid_t pid;

	if (0 == (pid = fork())) {
		return 0;
	} else if (0 < pid) {
		int wstatus;

		*status = -1 != waitpid(pid, &wstatus, 0) && WIFEXITED(wstatus)
			? WEXITSTATUS(wstatus)
			: EXIT_FAILURE;

		return 1;
	} else {
		perror("fork");
		_exit(127);
	}
}
#endif

#ifdef __GNUC__
__attribute__((noinline))
#endif
static void
try_attach_debugger(void)
{
	pid_t pid, dbgpid;

	if (-1 == ctty)
		return;

	pid = getpid();

	if (0 == (dbgpid = fork())) {
		char szpid[20];

		sprintf(szpid, "%u", pid);

		dup2(ctty, STDIN_FILENO);
		dup2(ctty, STDOUT_FILENO);
		dup2(ctty, STDERR_FILENO);

		execlp("gdb", "gdb",
				"-q",
				"--pid", szpid,
#ifdef __GNUC__
				/* using GCC, we exactly know how much frames
				 * should we go up to reach the origin of the
				 * signal, since functions said not to be
				 * inlined */
				"-ex", "up 4",
				"-ex", "list",
				"-ex", "info locals",
#endif
				NULL);
		_exit(127);
	} else {
		waitpid(dbgpid, NULL, 0);
	}
}

/* our exit(3) implementation */
void
test_exit(int status)
{
	switch (status) {
	case EXIT_SKIP:
	case EXIT_SUCCESS:
		/* do nothing */
		break;

	default:
		if (test_shared_->top_level)
			try_attach_debugger();
		break;
	}

	test_shared_->top_level = 0;

#ifndef TEST_NOFORK
	_exit(status);
#else
	*(NULL != currcase
	  ? &currcase->status
	  : &currtest->status
	) = status;
	longjmp(NULL != currcase ? currcase->env : currtest_env.env, status + 1);
#endif
}

static void
cat(int fd)
{
	char buf[(1 << 13)];
	ssize_t len = 0;
	ssize_t plen;
	off_t offset;

	for (offset = 0; plen = len, 0 < (len = pread(fd, buf, sizeof buf, offset)); offset += len) {
		while (-1 == write(STDERR_FILENO, buf, len)) {
			if (EAGAIN == errno)
				continue;

			perror("write");
			break;
		}
	}

	if (0 == plen || buf[plen - 1] != '\n')
		fputs("\x1b[7m$\x1b[0m", stderr);
	fputc('\n', stderr);
}

/* humanize time */
static char *
pretty_time(unsigned long ns) {
	static char buf[8];

	char *unit;

	if (ns < 10 * NS_PER_US)
		unit = "ns";
	else if (ns < 10 * NS_PER_MS)
		ns /= NS_PER_US, unit = "us";
	else if (ns < 10 * NS_PER_SEC)
		ns /= NS_PER_MS, unit = "ms";
	else
		ns /= NS_PER_SEC, unit = "s ";

	sprintf(buf, "%4lu %s", ns, unit);

	return buf;
}

/* return |ts| in nanoseconds */
__attribute__((const)) static unsigned long
tstons(struct timespec const *ts)
{
	return ts->tv_sec * NS_PER_SEC + ts->tv_nsec;
}

/* *|result| = |end| - |start| */
static void
ts_sub(
		struct timespec *result,
		struct timespec const *end,
		struct timespec const *start
) {
	result->tv_sec  = end->tv_sec  - start->tv_sec  + (end->tv_nsec < start->tv_nsec);
	result->tv_nsec = end->tv_nsec - start->tv_nsec + (end->tv_nsec < start->tv_nsec ? NS_PER_SEC : 0);
}

/* print an |n| characters width horizontal line of |ch| */
static void
hbar(size_t n, char c) {
	while (n-- > 0)
		fputc(c, stdout);
	fputc('\n', stdout);
}

void
test_print_case_header_(void)
{
	struct test_case_ const *p = currcase;

	while (NULL != p->parent)
		p = p->parent;

	printf("case %s/", currtest->name);

	for (; NULL != p->child; p = p->child)
		printf("%s\x1b[0m/", p->name);

	printf("\x1b[1m%s\x1b[0m (%s:%u):\n", p->name, p->file, p->line);
}

static void
signal_handler(int sig, siginfo_t *info, void *ucontext)
{
	void *array[100];
	int size;

	(void)sig, (void)ucontext;

	psiginfo(info, "\x1b[0;1;31mReceived unhandled signal\x1b[0m");

	size = backtrace(array, ARRAY_LEN(array));

	fprintf(stderr, "\nBacktrace:\n");
	backtrace_symbols_fd(array, size, STDERR_FILENO);

	test_exit(EXIT_FAILURE);
}

static
void setup_signals(void)
{
	struct sigaction sa;
	stack_t ss;
	int signum;

	/* setup alternative signal stack so a debugger process can easily
	 * trace back origin of the signal */
	ss.ss_sp = malloc((ss.ss_size = SIGSTKSZ));
	ss.ss_flags = 0;
		sigaltstack(&ss, NULL);

	sa.sa_sigaction = signal_handler;
	sa.sa_flags = SA_RESTART | SA_SIGINFO | SA_ONSTACK;
	sigfillset(&sa.sa_mask);

	for (signum = 0; signum < SIGRTMAX; ++signum) {
		struct sigaction oldsa;

		if (signum == SIGCHLD)
			continue;

		if (sigaction(signum, &sa, &oldsa)) {
			if (SIG_DFL != oldsa.sa_handler &&
			    SIG_IGN != oldsa.sa_handler)
				sigaction(signum, &oldsa, NULL);
		}
	}
}

static void
emit_event(int event)
{
	union test_object o;
	struct test_suite_ *actualsuite = NULL;

	FOREACH_OBJECT {
		switch (o.header->type) {
		case test_object_type_suite:
			actualsuite = o.suite;
			++o.suite;
			break;

		case test_object_type_hook:
			if (2/*global*/ == o.hook->scope ||
			   (1/*suite */ == o.hook->scope && actualsuite == currsuite))
				o.hook->proc(event);
			++o.hook;
			continue;

		case test_object_type_test:
			++o.test;
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
int
main(int argc, char *argv[])
{
	enum {
		stat_suite,
		stat_total,
		stat_passed,
		stat_failed,
		stat_ignored,
		stat_count_
	};
	uint32_t stat[stat_count_] = {0};
	union test_object o;
	size_t print_width = 30;
	struct timespec start, end;

	setup_signals();

	ctty = isatty(STDOUT_FILENO) ? dup(STDOUT_FILENO) : -1;

	/* prepare; count total tests and mark last ones in a suite */
	currsuite = NULL;
	currtest = NULL;
	FOREACH_OBJECT {
		size_t len;

		switch (o.header->type) {
		case test_object_type_suite:
			if (NULL != currtest)
				currtest->last_in_suite = 1;

			currsuite = o.suite;
			++stat[stat_suite];

			len = strlen(currsuite->name) + strlen(currsuite->file) + OUTPUT_SUITE_MINLEN;
			if (len > print_width)
				print_width = len;

			++o.suite;
			break;

		case test_object_type_hook:
			++o.hook;
			continue;

		case test_object_type_test:
			currtest = o.test;
			++stat[stat_total];

			len = strlen(o.test->name) + OUTPUT_TEST_MINLEN;
			if (len > print_width)
				print_width = len;

			++o.test;
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
		(void)ioctl(ctty, TIOCGWINSZ, &ws);

		if (ws.ws_col * 4 / 5 <= print_width)
			print_width = ws.ws_col;
	}

	printf("running %u tests\n", stat[stat_total]);

#ifdef TEST_NOFORK
	test_shared_ = alloca(sizeof *test_shared_);
#else
	test_shared_ = mmap(NULL, sizeof *test_shared_,
		PROT_READ | PROT_WRITE,
		MAP_SHARED | MAP_ANONYMOUS, -1, 0);
	if (MAP_FAILED == test_shared_) {
		perror("mmap");
		exit(127);
	}
#endif

	clock_gettime(CLOCK_MONOTONIC_RAW, &start);

	currsuite = NULL;
	currtest = NULL;
	FOREACH_OBJECT {
		int argi;
		int skip;
#ifdef TEST_NOFORK
		int orig_stderr, orig_stdout;
#endif

		/* find next test */
		switch (o.header->type) {
		case test_object_type_suite:
			currsuite = o.suite;

			if ('\0' != *currsuite->name)
				printf("\nsuite \x1b[1m%s\x1b[0m (%s:%u):\n",
						currsuite->name, currsuite->file, currsuite->line);
			else
				printf("\nfile \x1b[1m%s\x1b[0m:\n", currsuite->file);
			hbar(print_width, '-');

			emit_event(test_event_setup_suite);

			++o.suite;
			continue;

		case test_object_type_hook:
			++o.hook;
			continue;

		case test_object_type_test:
			currtest = o.test;
			++o.test;
			break;

		default:
			__builtin_unreachable();
		}

		printf("test \x1b[1m%s\x1b[0m ... %*s",
			currtest->name,
			print_width - OUTPUT_TEST_MINLEN - (int)strlen(currtest->name), "");
		fflush(stdout);

		for (skip = 0, argi = 1; argi < argc; ++argi) {
			char *pattern = argv[argi];
			regex_t reg;
			int should_skip;
			int ret;

			switch (*pattern) {
			case '+':
				++pattern;
				/* fall through */
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

		if (skip || 0 == currtest->num_iters) {
			++stat[stat_ignored];
			printf("\x1b[34mignored\x1b[0m\n");
			goto drop_output;
		}

		if (-1 == (currtest->outfd = memfd_create(currtest->name, MFD_CLOEXEC))) {
			perror("memfd_create");
			currtest->outfd = STDERR_FILENO;
		}

		emit_event(test_event_setup_test);

#ifdef TEST_NOFORK
		orig_stderr = dup(STDERR_FILENO);
		orig_stdout = dup(STDOUT_FILENO);
#endif

		test_shared_->top_level = 1;

		if (!test_fork_(
#ifndef TEST_NOFORK
				*currtest
#else
				currtest_env
#endif
		)) {
			struct timespec test_start, test_end;
			unsigned long iters = currtest->num_iters;

			dup2(currtest->outfd, STDERR_FILENO);
			setvbuf(stderr, NULL, _IONBF, 0);
			dup2(currtest->outfd, STDOUT_FILENO);
			setvbuf(stdout, NULL, _IONBF, 0);

			clock_gettime(CLOCK_MONOTONIC_RAW, &test_start);

			while (iters-- > 0)
				currtest->run();

			clock_gettime(CLOCK_MONOTONIC_RAW, &test_end);

			ts_sub(&test_start, &test_end, &test_start);
			test_shared_->total_ns = tstons(&test_start);

			test_exit(currtest->status);
		}

#ifdef TEST_NOFORK
		/* restore std{out,err} */
		dup2(orig_stderr, STDERR_FILENO);
		dup2(orig_stdout, STDOUT_FILENO);
#endif

		switch (currtest->status) {
		case EXIT_SUCCESS:
			currtest->time_ns = test_shared_->total_ns;

			printf("\x1b[1;32mok\x1b[0m, %s\n",
					pretty_time(currtest->time_ns));

			if (1 < currtest->num_iters)
				printf(" / %9lu iters = %s/iter",
						currtest->num_iters,
						pretty_time((currtest->time_ns + (currtest->num_iters - 1)/*round up*/) / currtest->num_iters));

			++stat[stat_passed];
			goto drop_output;

		case EXIT_SKIP:
			++stat[stat_ignored];
			printf("\x1b[1;34mskipped\x1b[0m\n");
			goto drop_output;

		default:
			printf("\x1b[1;31mFAILED\x1b[0m\n");
				++stat[stat_failed];

			if (TEST_NOGATHEROUTPUT)
				cat(currtest->outfd);
			else
				goto keep_output;
		}

	drop_output:
		if (STDERR_FILENO != currtest->outfd)
			close(currtest->outfd);
		currtest->outfd = -1;

	keep_output:
		emit_event(test_event_teardown_test);

		if (currtest->last_in_suite)
			emit_event(test_event_teardown_suite);
	}

	clock_gettime(CLOCK_MONOTONIC_RAW, &end);
	ts_sub(&start, &end, &start);

	if (!TEST_NOGATHEROUTPUT) {
		int any_gathered = 0;

		hbar(print_width, '=');

		FOREACH_OBJECT {
			switch (o.header->type) {
			case test_object_type_suite:
				++o.suite;
				break;

			case test_object_type_hook:
				++o.hook;
				break;

			case test_object_type_test:
				if (-1 != o.test->outfd) {
					currtest = o.test;

					printf("test \x1b[1m%s\x1b[0m (%s:%u):\n",
							currtest->name, currtest->file, currtest->line);

					cat(o.test->outfd);

					close(o.test->outfd), o.test->outfd = -1;

					any_gathered = 1;
				}
				++o.test;
				break;

			default:
				__builtin_unreachable();
			}
		}

		if (any_gathered)
			hbar(print_width, '=');
	}

	printf(
		"%*s%10ld assertions, %s\n"
		"\x1b[0;1mtest result\x1b[0m: \x1b[1m%s\x1b[0m. "
		"\x1b[1;32m%u\x1b[0;32m passed\x1b[0m; "
		"\x1b[1;31m%u\x1b[0;31m failed\x1b[0m; "
		"\x1b[1;34m%u\x1b[0;34m ignored\x1b[0m\n",
		print_width - 30, "", test_shared_->num_assertions, pretty_time(tstons(&start)),
		0 == stat[stat_failed] ? "\x1b[1;32mOK" : "\x1b[1;31mFAILED",
		stat[stat_passed],
		stat[stat_failed],
		stat[stat_ignored]
	);

	return 0 == stat[stat_failed] ? EXIT_SUCCESS : EXIT_FAILURE;
}
