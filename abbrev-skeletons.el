;;; abbrev-skeletons.el --- Definition of abbrev skeletons -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'cc-mode
  (my/define-abbrev-skeleton cc-main "$main" (c++-mode c-mode asdf-mode)
                             nil
                             "int main(int argc, char *argv[]) {" > \n
                             _ \n
                             "return 0;" > \n
                             "}" >))

(with-eval-after-load 'bazel
  (my/define-abbrev-skeleton bazel-cc-binary "cc_binary" (bazel-build-mode)
                             "Library name: "
                             "cc_binary(" \n
                             "name = \"" str "_main\"," > \n
                             "srcs = [\"" str "_main.cc\"]," > \n
                             "deps = [" > \n
                             "\":" str "\"," > \n
                             "\"@gflags\"," > \n
                             "\"@glog\"," > \n
                             "]," > \n
                             ")" > \n)
  (my/define-abbrev-skeleton bazel-cc-library "cc_library" (bazel-build-mode)
                             "Library name: "
                             "cc_library(" \n
                             "name = \"" str "\"," > \n
                             "srcs = [\"" str ".cc\"]," > \n
                             "hdrs = [\"" str ".h\"]," > \n
                             ")" > \n)
  (my/define-abbrev-skeleton bazel-cc-test "cc_test" (bazel-build-mode)
                             "Library name: "
                             "cc_test(" \n
                             "name = \"" str "_test\"," > \n
                             "srcs = [\"" str "_test.cc\"]," > \n
                             "deps = [" > \n
                             "\":" str "\"," > \n
                             "\"//common/utils/testing:pony_test_main\"," > \n
                             "]," > \n
                             ")" > \n))

(with-eval-after-load 'python
  (my/define-abbrev-skeleton py-ifmain "ifmain" (python-mode)
                             nil
                             "if __name__ == '__main__':" > \n
                             _ >))

;;; abbrev-skeletons.el ends here
