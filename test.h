/*
 * Copyright (C) 2019 zsugabubus
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
#ifndef TEST_H
#define TEST_H

#include <fcntl.h> /* O_WRONLY */
#include <stdio.h> /* fprintf, perror */
#include <stdlib.h> /* exit */
#include <string.h> /* strcmp, memcmp */
#include <sys/wait.h> /* wait */
#include <unistd.h> /* fork, dup2 */

#ifndef TEST_CASESEP
# define TEST_CASESEP " / "
#endif

#ifndef TEST_NOSTYLES
# define ANSI_BOLD  "\x1b[1m"
# define ANSI_RESET "\x1b[0m"
# ifndef TEST_NOCOLORS
#  define ANSI_RED "\x1b[31m"
#  define ANSI_GREEN "\x1b[32m"
#  define ANSI_BLUE "\x1b[34m"
# else
#  define ANSI_RED ""
#  define ANSI_GREEN ""
#  define ANSI_BLUE ""
# endif
#else
#define ANSI_BOLD  ""
#define ANSI_RESET ""
#define ANSI_RED ""
#define ANSI_GREEN ""
#define ANSI_BLUE ""
#endif

#ifndef TEST_NOFORK
# define TEST_PARAM_ void
# define TEST_FORK_IMPL_ fork()
# define TEST_CASE_EXIT_IMPL exit(EXIT_SUCCESS)
# define FAILURE(code) exit((code));
#else
# define TEST_PARAM_ int *const _test_result
# define TEST_FORK_IMPL_ 0
# define TEST_CASE_EXIT_IMPL ftruncate(currtest->outfd, test_case_info[--test_case_depth].outpos)
# define FAILURE(code) do { *_test_result = (code); return; } while (0);
#endif

/** Info about tests and suites. */
struct test_test_info {
	void(*const run)(TEST_PARAM_)  __attribute__((aligned(16)));
	char const *const desc;
	char const *const file;
	unsigned line;
	unsigned long const iters;
	int outfd;
	unsigned long total_ns;
	char a[4];
};

/** Info about test case. */
struct test_case_info {
	char const *name;
	char const *file;
	unsigned line;
	off_t outpos;
};

extern int test_case_depth;
extern struct test_case_info test_case_info[7];
extern struct test_test_info *currtest;

/* Default XML output. */
#ifdef TEST_OUTPUT_XML
# define TEST_OUTPUT_LIBCHECKXML
#endif

/* Default output format. */
#if !defined(TEST_OUTPUT_HUMAN) && \
    !defined(TEST_OUTPUT_LIBCHECKXML)
# define TEST_OUTPUT_HUMAN
#endif

#define BENCH(name, repeat) \
	BENCH_(void, name, #name, repeat)

#define BENCHND(name, desc, repeat) \
	BENCH_(void, name, desc, repeat)

/* Must be static, because we can't generate unique names. */
#define BENCHD(desc, repeat) \
	BENCH_(static void, TOKENPASTE(test_at_line_, __LINE__), desc, repeat)

/* Test is just a bench that runs once. */
#define TEST(name) \
	BENCH(name, 1)

#define TESTD(desc) \
	BENCHD(desc, 1)

#define TESTND(name, desc) \
	BENCHD(name, desc, 1)

/* Have to hack with rtype, because empty macro argument is not valid. */
#define BENCH_(static_rtype, name, desc, repeat) \
	static_rtype name(TEST_PARAM_); \
	TEST_VAR_(name) = {name, desc, __FILE__, __LINE__, repeat, -1}; \
	static_rtype name(TEST_PARAM_)

#define CASE(name) \
	CASED(#name)

#define CASED(case_desc) \
	assert_msg(test_case_depth < (sizeof(test_case_info) / sizeof(*test_case_info)), "cannot nest more cases"); \
	test_case_info[test_case_depth].name = (case_desc); \
	test_case_info[test_case_depth].file = __FILE__; \
	test_case_info[test_case_depth].line = __LINE__; \
	test_case_info[test_case_depth].outpos = lseek(currtest->outfd, 0, SEEK_CUR); \
\
	switch (TEST_FORK_IMPL_) { \
	case -1: \
		perror("fork"); \
		exit(EXIT_FAILURE); \
	case 0: { \
		int i; \
		++test_case_depth; \
\
		/* Prints what test cases we are in. */ \
		fprintf(stdout, "case "); \
		for (i = 0;i < test_case_depth - 1;++i) \
			fprintf(stdout, ANSI_BOLD "%s" ANSI_RESET TEST_CASESEP, \
				test_case_info[i].name); \
\
		fprintf(stdout, ANSI_BOLD "%s" ANSI_RESET " (%s:%u):\n", \
			test_case_info[i].name, \
			test_case_info[i].file, \
			test_case_info[i].line \
		); \
		goto TOKENPASTE(test_case_run_, __LINE__); \
	} \
	default: { \
		int stat_val; \
		 /* Wait test to finish. */ \
		wait(&stat_val); \
		if (WIFEXITED(stat_val) && EXIT_SUCCESS == WEXITSTATUS(stat_val)) \
			/* We don't care about output of passed cases. */ \
			ftruncate(currtest->outfd, test_case_info[test_case_depth].outpos); \
		else \
			/* Propagate *exit code*. */ \
			exit(WIFEXITED(stat_val) ? WEXITSTATUS(stat_val) : EXIT_FAILURE); \
	} \
	} \
	for (;0;TEST_CASE_EXIT_IMPL) TOKENPASTE(test_case_run_, __LINE__):

#define TEST_VAR_(name) \
	static struct test_test_info test_##name __attribute__((no_reorder, __used__, unused, __section__("test"))) \

#define SUITE(name) \
	SUITED(#name)

#define SUITED(desc) \
	SUITE_(desc)

#define SUITE_(desc) \
	TEST_VAR_(unit) = {NULL, desc, __FILE__, 0, 0, -1};

#define TOKENPASTE_(a, b) a##b
#define TOKENPASTE(a, b) TOKENPASTE_(a, b)

#ifdef TEST_OUTPUT_HUMAN
# define TEST_ASSERT_FORMAT \
	"%s:%u: " ANSI_RED ANSI_BOLD "Assertion" ANSI_RESET " %s failed.\n", __FILE__, __LINE__
# define TEST_EXPECT_FORMAT(fmt) \
	"%s:%u: " ANSI_RED ANSI_BOLD "Expected" ANSI_RESET " " fmt ", got " fmt ".\n", __FILE__, __LINE__
#else
# define TEST_ASSERT_FORMAT \
	"Assertion %s failed"
# define TEST_EXPECT_FORMAT(fmt) \
	"Expected " fmt ", got " fmt
#endif

#define assert_msg(c, msg) \
	if (!(c)) {  \
		fprintf(stderr, TEST_ASSERT_FORMAT, msg); \
		FAILURE(EXIT_FAILURE); \
	}

#define assert(c) \
	assert_msg(c, "`" #c "'")

#define assert_eq(lhs, rhs) \
	assert(lhs == rhs);

#define assert_streq(lhs, rhs) \
	assert(0 == strcmp(lhs, rhs));

#define assert_memeq(lhs, rhs, size) \
	assert(0 == memcmp(lhs, rhs, size));

#define expect_msg(exp, act, type, cmp, fmt) \
do { \
	type const EXP = (exp); \
	type const ACT = (act); \
	if (!(cmp)) {  \
		fprintf(stderr, TEST_EXPECT_FORMAT(fmt), EXP, ACT); \
		FAILURE(EXIT_FAILURE); \
	} \
} while (0);

#define expect_int(exp, act) \
	expect_msg(exp, act, long int const, (EXP == ACT), "%ld")

#define expect_uint(exp, act) \
	expect_msg(exp, act, long unsigned const, (EXP == ACT), "%lu")

#define expect_str(exp, act) \
	expect_msg(exp, act, char const*, (0 == strcmp(EXP, ACT)), "`%s'")

#endif /* TEST_H */
/* vim:set ft=c ts=4 sw=4 noet: */
