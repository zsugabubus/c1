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

#ifndef TEST_NOANSIES
# define ANSI_BOLD  "\x1b[1m"
# define ANSI_RESET "\x1b[0m"
# ifndef TEST_NOCOLORS
#  define ANSI_RED "\x1b[31m"
#  define ANSI_GREEN "\x1b[32m"
#  define ANSI_ORANGE "\x1b[33m"
#  define ANSI_BLUE "\x1b[34m"
#  define ANSI_MAGENTA "\x1b[35m"
# else
#  define ANSI_RED ""
#  define ANSI_GREEN ""
#  define ANSI_ORANGE ""
#  define ANSI_BLUE ""
#  define ANSI_MAGENTA ""
# endif
#else
#define ANSI_BOLD  ""
#define ANSI_RESET ""
#define ANSI_RED ""
#define ANSI_GREEN ""
#define ANSI_ORANGE ""
#define ANSI_BLUE ""
#define ANSI_MAGENTA ""
#endif

struct test_case_info {
	char const *name;
	char const *file;
	unsigned line;
	off_t outpos;
};

struct test_test_info {
	void(*const run)(void)  __attribute__((aligned(16)));
	char const *const desc;
	char const *const file;
	unsigned line;
	unsigned long repeat;
	int outfd;
	unsigned long total_ns;
	char a[4];
};

extern int test_case_depth;
extern struct test_case_info test_case_info[7];
extern struct test_test_info *currtest;
__attribute__((section("test")))
extern struct test_test_info __start_test, __stop_test;

#define BENCH(name, repeat) \
	BENCH_(void, name, #name, repeat)

/* Must be static, because we can't generate unique names. */
#define BENCHD(desc, repeat) \
	BENCH_(static void, TOKENPASTE(test_at_line_, __LINE__), desc, repeat)

#define TEST(name) \
	BENCH(name, 1)

#define TESTD(desc) \
	BENCHD(desc, 1)

#define BENCH_(static_void, name, desc, repeat) \
	static_void name(void); \
	TEST_VAR(name) = {name, desc, __FILE__, __LINE__, repeat, -1}; \
	static_void name(void)

#define CASE(name) \
	CASED(#name)

#define CASED(case_desc) \
	assert_msg_(test_case_depth < (sizeof(test_case_info) / sizeof(*test_case_info)), "cannot nest more cases"); \
	test_case_info[test_case_depth].name = (case_desc); \
	test_case_info[test_case_depth].file = __FILE__; \
	test_case_info[test_case_depth].line = __LINE__; \
	test_case_info[test_case_depth].outpos = lseek(currtest->outfd, 0, SEEK_CUR); \
\
	switch (fork()) { \
	case -1: \
		perror("fork"); \
		exit(EXIT_FAILURE); \
	case 0: { \
		int i; \
		++test_case_depth; \
\
		fprintf(stderr, "case "); \
		for (i = 0;i < test_case_depth - 1;++i) \
			fprintf(stderr, ANSI_BOLD "%s" ANSI_RESET TEST_CASESEP, \
				test_case_info[i].name); \
\
		fprintf(stderr, ANSI_BOLD "%s" ANSI_RESET " (%s:%u):\n", \
			test_case_info[i].name, \
			test_case_info[i].file, \
			test_case_info[i].line \
		); \
		goto TOKENPASTE(test_case_run_, __LINE__); \
	} \
	default: { \
		int stat_val; \
		wait(&stat_val); \
		if (WIFEXITED(stat_val) && EXIT_SUCCESS == WEXITSTATUS(stat_val)) \
			/* Drop output of passed cases. */ \
			ftruncate(currtest->outfd, test_case_info[test_case_depth].outpos); \
		else \
			/* Propagate exit code. */ \
			exit(WIFEXITED(stat_val) ? WEXITSTATUS(stat_val) : EXIT_FAILURE); \
	} \
	} \
	for (;0;exit(EXIT_SUCCESS)) TOKENPASTE(test_case_run_, __LINE__):

#define TEST_VAR(name) \
	static struct test_test_info test_##name __attribute__((unused, section("test")))

#define UNIT(name) \
	UNITD(#name)

#define UNITD(desc) \
	UNIT_(desc)

#define UNIT_(desc) \
	TEST_VAR(unit) = {NULL, desc, __FILE__, 0, 0, -1};

#define TOKENPASTE_(a, b) a##b
#define TOKENPASTE(a, b) TOKENPASTE_(a, b)

#define assert_msg_(c, msg) \
	if (!(c)) {  \
		fprintf(stderr, "%s:%u: " ANSI_ORANGE ANSI_BOLD "Assertion" ANSI_RESET " %s\n", \
			__FILE__, __LINE__, msg); \
		exit(EXIT_FAILURE); \
	}

#define assert(c) \
	assert_msg_(c, "`" #c "' failed.")

#define assert_eq(lhs, rhs) \
	assert(lhs == rhs);

#define assert_cstreq(lhs, rhs) \
	assert(0 == strcmp(lhs, rhs));

#define assert_memeq(lhs, rhs, size) \
	assert(0 == memcmp(lhs, rhs, size));

#define expect_msg_(expected, actual, type, cmp, fmt) \
do { \
	type exp = (expected); \
	type act = (actual); \
	if (!(cmp)) {  \
		fprintf(stderr, "%s:%u: " ANSI_ORANGE ANSI_BOLD "Expected" ANSI_RESET " " fmt ", got " fmt ".\n", \
			__FILE__, __LINE__, exp, act); \
		exit(EXIT_FAILURE); \
	} \
} while (0);

#define expect_int(expected, actual) \
	expect_msg_(expected, actual, long int const, (exp == act), "%ld")

#define expect_uint(expected, actual) \
	expect_msg_(expected, actual, long unsigned const, (exp == act), "%lu")

#define expect_str(expected, actual) \
	expect_msg_(expected, actual, char const*, (0 == strcmp(exp, act)), "`%s'")

#endif /* TEST_H */
/* vim:set ft=c ts=4 sw=4 noet: */
