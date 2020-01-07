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
#ifndef TEST_H
#define TEST_H

#undef _GNU_SOURCE
#define _GNU_SOURCE
#include <alloca.h>
#include <fcntl.h>
#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef TEST_VERBOSE
#define TEST_VERBOSE 0
#else
#undef TEST_VERBOSE
#define TEST_VERBOSE 1
#endif

#define EXIT_SKIP 77
#define EXIT_NONFATAL_FAILURE 123

extern struct _test_shared *_test_shared;
extern struct _test_suite_info *currsuite;
extern struct _test_test_info *currtest;
extern struct _test_case_info *currcase;

enum { test_return_code_passed, test_return_code_fatal_failure, test_return_code_nonfatal_failur };

#ifndef TEST_NOFORK
#define _test_fork_(obj) test_fork(&(obj).exitcode)
#else
#define _test_fork_(obj) setjmp((obj).env)
#endif

#define SUITE(name) \
	_TEST_BUILD_SUITE_(#name)

#define SUITE_FILE \
	_TEST_BUILD_SUITE_("")

#define _TEST_BUILD_SUITE_(name) \
	_TEST_VAR_(suite, suite) = {{test_object_id_suite}, name, __FILE__, __LINE__};

#define TEST(name) \
	_TEST_BUILD_TEST_(#name, 1)

#define TEST_IF(name, cond) \
	_TEST_BUILD_TEST_(#name, !!(cond))

#define BENCH(name, repeat) \
	_TEST_BUILD_TEST_(#name, repeat)

#define BENCH_IF(name, repeat, cond) \
	_TEST_BUILD_TEST_(#name, (cond) ? (repeat) : 0)

#define _TEST_BUILD_TEST_(name, repeat) \
	_TEST_BUILD_TEST__(_TEST_TOKENPASTE_(_test_func_, __LINE__), name, repeat)

#define _TEST_BUILD_TEST__(func, name, repeat) \
	static void func(void); \
	_TEST_VAR_(test, func) = { \
		{test_object_id_test}, \
		name, \
		__FILE__, __LINE__, \
		func, \
		repeat, \
		-1, \
		0, \
		EXIT_SUCCESS \
	}; \
	static void func(void)

#define CASE(casename) \
	if (currcase) { \
		if (!currcase->child) { \
			currcase->child = alloca(sizeof(struct _test_case_info)); \
			currcase->child->parent = currcase; \
		} \
		currcase = currcase->child; \
	} else { \
		currcase = alloca(sizeof(struct _test_case_info)); \
		currcase->parent = NULL; \
	} \
	currcase->name = (#casename); \
	currcase->file = __FILE__; \
	currcase->line = __LINE__; \
	currcase->child = NULL; \
	currcase->exitcode = EXIT_SUCCESS; \
	currcase->outlen = lseek(currtest->outfd, 0, SEEK_CUR); \
\
	if (!_test_fork_(*currcase)) { \
		_test_print_case_path(); \
		goto _TEST_TOKENPASTE_(_test_case_run_, __LINE__); \
	} else { \
		switch (currcase->exitcode) { \
		case EXIT_SUCCESS: \
		case EXIT_SKIP: \
			/* Clear output of passed cases. */ \
			ftruncate(currtest->outfd, currcase->outlen); \
			break; \
		case EXIT_NONFATAL_FAILURE: \
			*(currcase->parent ? &currcase->parent->exitcode : &currtest->exitcode) = EXIT_NONFATAL_FAILURE; \
			break; \
		default: {/* Propagate return code. */ \
			int exitcode = currcase->exitcode; \
			currcase = currcase->parent; \
			test_exit(exitcode); \
		} \
		} \
		currcase = currcase->parent; \
	} \
	for (;0;test_exit(currcase->exitcode)) _TEST_TOKENPASTE_(_test_case_run_, __LINE__):

#define _TEST_VAR_(type, id) \
	static struct _test_##type##_info test_##id __attribute__((no_reorder, __used__, unused, __section__("test"))) \

enum test_object_id {
	test_object_id_suite,
	test_object_id_hook,
	test_object_id_test
};

enum test_event {
	test_event_setup_suite,
	test_event_teardown_suite,
	test_event_setup_test,
	test_event_teardown_test
};

#define HOOK(scope) \
	_TEST_TOKENPASTE3_(_HOOK_SCOPE_, scope, _)

#define SETUP() \
	static void _test_hook_setup(void); \
	HOOK(SUITE) \
	{ \
		if (test_event_setup_suite == event) \
			_test_hook_setup(); \
	} \
	static void _test_hook_setup(void)

#define teardown() \
	static void _test_hook_teardown(void); \
	HOOK(SUITE) \
	{ \
		if (test_event_teardown_suite == event) \
			_test_hook_teardown(); \
	} \
	static void _test_hook_teardown(void)

#define _HOOK_SCOPE_SUITE_ _TEST_BUILD_HOOK_(1)
#define _HOOK_SCOPE_GLOBAL_ _TEST_BUILD_HOOK_(2)

#define _TEST_BUILD_HOOK_(scope) \
	_TEST_BUILD_HOOK__(scope, _TEST_TOKENPASTE_(test_hook_, __LINE__))

#define _TEST_BUILD_HOOK__(scope, func) \
	static void func(int); \
	_TEST_VAR_(HOOK, func) = {{test_object_id_hook}, func, scope}; \
	static void func(int event)

#define STRINGIFY(t) #t
#define _TEST_TOKENPASTE__(a, b) a##b
#define _TEST_TOKENPASTE3__(a, b, c) a##b##c
#define _TEST_TOKENPASTE_(a, b) _TEST_TOKENPASTE__(a, b)
#define _TEST_TOKENPASTE3_(a, b, c) _TEST_TOKENPASTE3__(a, b, c)

#define _TEST_TEST_ASSERT_FAILURE_(exitcode) FATAL_FAILURE(exitcode)
#define _TEST_TEST_EXPECT_FAILURE_(_ignored) NONFATAL_FAILURE(EXIT_NONFATAL_FAILURE)

#define _TEST_TEST_ASSERT_NOUN_ "Assertion"
#define _TEST_TEST_EXPECT_NOUN_ "Expectation"

#define _TEST_FORMAT_SUCCESS_(test) \
	"%s:%u: \x1b[32m" test##NOUN_ "\x1b[0m %s passed.\n"
#define _TEST_FORMAT_FAILURE_(test) \
	"%s:%u: \x1b[1;31m" test##NOUN_ "\x1b[0m %s \x1b[1;31mfailed\x1b[0m.\n"
#define _TEST_FORMAT_TWO_(test, fmt) \
	"%s:%u: \x1b[1;31m" test##NOUN_ "\x1b[0m %s \x1b[1;31mfailed\x1b[0m\n  left=" fmt ";\n  right=" fmt ".\n"
#define _TEST_FORMAT_ONE_(test, fmt) \
	"%s:%u: \x1b[1;31m" test##NOUN_ "\x1b[0m %s \x1b[1;31mfailed\x1b[0m; got " fmt ".\n"

#define _TEST_FORMAT_(test, type, kind) \
	__builtin_choose_expr(_TEST_ISTYPE_(type, char *) || _TEST_ISTYPE_(type, char[]), \
	                      _TEST_FORMAT_##kind##_(test, "\"%s\""), \
	__builtin_choose_expr(_TEST_ISTYPE_(type, char), \
	                      _TEST_FORMAT_##kind##_(test, "'%c'"), \
	__builtin_choose_expr(_TEST_ISTYPE_(type, double) || _TEST_ISTYPE_(type, float), \
	                      _TEST_FORMAT_##kind##_(test, "%f"), \
	__builtin_choose_expr(_TEST_ISTYPE_(type, void *), \
	                      _TEST_FORMAT_##kind##_(test, "%p"), \
	                      _TEST_FORMAT_##kind##_(test, "%d")))))

#define _TEST_COND_(test, cond, msg) \
	if (++_test_shared->num_assertions, !(cond)) { \
		fprintf(stderr, _TEST_FORMAT_FAILURE_(test), __FILE__, __LINE__, msg); \
		test##FAILURE_(EXIT_FAILURE); \
	} \

#define _TEST_ISTYPE_(val, type) \
	__builtin_types_compatible_p(__typeof__(val), type)

#define _TEST_CMP__(lhs, cmp, rhs) \
	__builtin_choose_expr(_TEST_ISTYPE_(lhs, char *) || _TEST_ISTYPE_(lhs, char[]) || _TEST_ISTYPE_(lhs, char (*)[]), \
	                      (strcmp((char const*)(size_t)lhs, (char const*)(size_t)rhs) cmp 0), \
	                      (lhs cmp (__typeof__(lhs))rhs)/* FIXME: It can result in false positives. */)

#define _TEST_CMP_(test, lhs, cmp, rhs) \
do { \
	__typeof__(lhs) const lhsv = lhs; /* no parenthesis: ("invalid") */ \
	__typeof__(rhs) const rhsv = rhs; \
	if (++_test_shared->num_assertions, !_TEST_CMP__(lhsv, cmp, rhsv)) { \
		__builtin_choose_expr(__builtin_constant_p(lhs) + __builtin_constant_p(rhs) == 1, \
			fprintf(stderr, _TEST_FORMAT_(test, __typeof__(rhs), ONE), \
				__FILE__, __LINE__, #lhs " " #cmp " " #rhs, __builtin_choose_expr(__builtin_constant_p(rhs), lhsv, rhsv)) \
			, \
			fprintf(stderr, _TEST_FORMAT_(test, __typeof__(rhs), TWO), \
				__FILE__, __LINE__, #lhs " " #cmp " " #rhs, lhsv, rhsv) \
			); \
		test##FAILURE_(EXIT_FAILURE); \
	} else if (TEST_VERBOSE) { \
		fprintf(stderr, _TEST_FORMAT_SUCCESS_(test), \
			__FILE__, __LINE__, #lhs " " #cmp " " #rhs); \
	} \
} while(0)

#define assert_msg(cond, msg) _TEST_COND_(_TEST_TEST_ASSERT_, cond, msg)
#define expect_msg(cond, msg) _TEST_COND_(_TEST_TEST_EXPECT_, cond, msg)

#define assert_true(cond) _TEST_COND_(_TEST_TEST_ASSERT_, cond, "`" #cond "'")
#define assert_null(expr) assert_true((expr) == NULL)
#define assert_nonnull(expr) assert_true((expr) != NULL)
#define assert_false(cond) assert_true(!(cond))
#define assert_memequal(lhs, rhs, size) assert_equal(memcmp(lhs, rhs, size), 0);

#define expect_true(cond) _TEST_COND_(_TEST_TEST_EXPECT_, cond, "`" #cond "'")
#define expect_null(expr) expect_true((expr) == NULL)
#define expect_nonnull(expr) expect_true((expr) != NULL)
#define expect_false(cond) expect_true(!(cond))
#define expect_memequal(lhs, rhs, size) expect_equal(memcmp(lhs, rhs, size), 0);

#define assert_cmp(lhs, cmp, rhs)   _TEST_CMP_(_TEST_TEST_ASSERT_, lhs, cmp, rhs)
#define expect_cmp(lhs, cmp, rhs)   _TEST_CMP_(_TEST_TEST_EXPECT_, lhs, cmp, rhs)

#define assert_equal(lhs, rhs)   assert_cmp(lhs, ==, rhs)
#define expect_equal(lhs, rhs)   expect_cmp(lhs, ==, rhs)
#define assert_less(lhs, rhs)    assert_cmp(lhs, <,  rhs)
#define expect_less(lhs, rhs)    expect_cmp(lhs, <,  rhs)
#define assert_greater(lhs, rhs) assert_cmp(lhs, >,  rhs)
#define expect_greater(lhs, rhs) expect_cmp(lhs, >,  rhs)

void _test_print_suite_path(void);
void _test_print_test_path(void);
void _test_print_case_path(void);

#define FATAL_FAILURE(code) test_exit(code);
#define NONFATAL_FAILURE(code) do { \
	int *result = (currcase ? &currcase->exitcode : &currtest->exitcode); \
	if (EXIT_SUCCESS == *result) \
		*result = (code); \
} while(0)

struct _test_shared {
	size_t num_assertions;
	unsigned long total_ns;
};

struct _test_info_header {
	unsigned char const type;
};

struct _test_suite_info {
	struct _test_info_header const header;
	char const *const name;
	char const *const file;
	unsigned const line;
} __attribute__((aligned(64)));

struct _test_hook_info {
	struct _test_info_header const header;
	void(*const hook)(int event);
	unsigned scope;
} __attribute__((aligned(64)));

struct _test_test_info {
	struct _test_info_header const header;
	char const *const name;
	char const *const file;
	unsigned const line;
	void(*const run)(void);
	unsigned long iters;
	int outfd;
	unsigned long total_ns;
	int exitcode;
	int last_in_suite;
} __attribute__((aligned(64)));

/** Info about test case. */
struct _test_case_info {
	char const *name;
	char const *file;
	unsigned line;
	off_t outlen;
	struct _test_case_info *parent;
	struct _test_case_info *child;
	int exitcode;
#ifdef TEST_NOFORK
	jmp_buf env;
#endif
};

void test_exit(int exitcode) __attribute__((noreturn));
int test_fork(int *exitcode) __attribute__((returns_twice));

#endif /* TEST_H */
/* vim:set ft=c ts=4 sw=4 noet: */
